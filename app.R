#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rsconnect)
library(DT)


# Setup Cluster variables
set.seed(3)
cluster_variables <- c("SFPerCycle", "PercentAOG", "PlatoonRatio", 
                       "TotalRedLightViolations")

# Read corridor dataset
df <- read_rds("dfCorridors_NA.rds") %>%
    select(BinStartTime, SignalId, ApproachId, TotalVolume, SFPerCycle,
           PercentAOG, PlatoonRatio, TotalRedLightViolations,
           AMPeak, Corrdior) %>%
    # change ridiculous data points to NA
    mutate(
        PlatoonRatio = ifelse(PlatoonRatio > 10, NA, PlatoonRatio)
    ) %>%
    
    # initialize cluster variable
    mutate(cluster = NA) 
    

# get complete data for clustering
clusters <- df %>%
    select(cluster_variables) %>%
    na.omit() %>%
    mutate_all(scale) %>%
    kmeans(center = 5)

# The output of kmeans corresponds to the elements of the object passed as
# argument x. In your case, you omit the NA elements, and so $cluster indicates
# the cluster that each element of na.omit(x) belongs to.
df$cluster[complete.cases(df %>% select(cluster_variables))] <- clusters$cluster

optional_xaxis <- c(cluster_variables, "TotalVolume")


# read threshold dataframe
thresholds <- read_csv("threshold_definitions.csv")


# Convert the thresholds into a list that can be sorted and compared against
th_grouped <- thresholds %>%
    group_by(variable) %>% nest()
th_list <- lapply(th_grouped$data, function(x) {
    l <- c(x$level)
    names(l) <- x$id
    l
})
names(th_list) <- th_grouped$variable

# Create a function to find the threshold cutoff directly beneath the value
my_lookup <- function(x, v){
    # check value, find last bigger
    r <- names(rev(v[x > v])[1])
    if(is.na(r)) NA else as.numeric(r)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ATSPM Data Plot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # x - axis selector
            shiny::selectInput("Xvar", "X-axis", choices = optional_xaxis, multiple = F),
            
            # y - axis selector
            shiny::selectInput("Yvar", "Y-axis", choices = optional_xaxis, multiple = F),
            
            # corridor filter
            checkboxGroupInput("Cor", "Select a Corridor:",
                               c(unique(as.character(df$Corrdior))), 
                               inline = TRUE),
            shiny::checkboxInput("cor_facet", label = "Facet corridor?", value = FALSE),
            
            # Time of day filter
            checkboxGroupInput("tod", "Select a Time of Day:",
                               c("AMPeak",
                                 "MidDay"),
                               inline = TRUE),
            shiny::checkboxInput("tod_facet", label = "Facet time of day?", value = FALSE),
            
            # Signals filter
            shiny::selectInput("Intersection", "Select a SignalId",
                               choices = c("All", c(unique(df$SignalId)))),
            
            # Date range filter
            dateRangeInput("daterange1", "Date range:",
                           start = "2018-03-05",
                           end   = "2018-10-25"),

            ## TODO: Add other weights
            h4("Weights for Intersection Scoring"),
            shiny::sliderInput("prweight", "Platoon Ratio", min = 0, max = 1, step = 0.1, value = 0.4),
            shiny::sliderInput("sfweight", "Split Fail", min = 0, max = 1, step = 0.1, value = 0.2),
            shiny::sliderInput("aogweight", "Percent AOG", min = 0, max = 1, step = 0.1, value = 0.2),
            shiny::sliderInput("rlweight", "Red Light Violation", min = 0, max = 1, step = 0.1, value = 0.2),
            
            # Download Button
            downloadButton("downloadData", "Download")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           DT::dataTableOutput("table"),
           verbatimTextOutput("summary"),
           plotOutput("boxPlot"),
           
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # This reactive object constructs an analysis dataset from the
    # user-specified time periods
    plotdata <- reactive({
        pd <- tibble(            
            SignalId = df$SignalId,
            Date = df$BinStartTime,
            x = df[[input$Xvar]],
            y = df[[input$Yvar]],
            pr = df$PlatoonRatio,
            sf = df$SFPerCycle,
            aog = df$PercentAOG,
            rl = df$TotalRedLightViolations,
            cluster = df[["cluster"]],
            Corridor = df$Corrdior,
            TOD = ifelse(df$AMPeak, "AMPeak", "MidDay")
        ) 
        
        if (!is.null(input$Cor)){
          pd <- pd %>% filter(Corridor %in% input$Cor)
        } 
        
        if (!is.null(input$tod)){
          pd <- pd %>% filter(TOD %in% input$tod)
        }
        
        if (input$Intersection != "All"){
          pd <- pd %>% filter(SignalId %in% input$Intersection)    
        }
        
        if (!is.null(input$daterange1)){
          pd <- pd %>% filter(Date >= input$daterange1[1], Date <= input$daterange1[2])
        }
        
        pd
    })
    
    # This reactive object returns the threshold lines only for the selected
    # X variable.
    threshold_lines <- reactive({
        thresholds %>% filter(variable %in% input$Xvar)
    })
    
    # This reactive the thresheld_data function
    threshheld_data <- reactive({
        plotdata() %>%
            # look up threshold score values
            mutate(
                pr_score  = map_dbl(pr, my_lookup,  v = th_list$PlatoonRatio),
                sf_score  = map_dbl(sf, my_lookup,  v = th_list$SFPerCycle),
                aog_score = map_dbl(aog, my_lookup, v = th_list$PercentAOG),
                rl_score  = map_dbl(rl, my_lookup,  v = th_list$TotalRedLightViolations),
                pr_weight = wts["pr"],
                sf_weight = wts["sf"],
                aog_weight = wts["aog"],
                rl_weight = wts["rl"])
    })
   
    # This output plot is either an X-Y scatter plot (if using two different measures)
    # or a histogram (if X and Y are the same)
    output$distPlot <- renderPlot({
        
        pd <- plotdata()
        
        if(input$Xvar == input$Yvar) {
            p <- ggplot(pd, aes(x = x, fill = factor(cluster))) +
                geom_histogram(aes(y = stat(width*density))) + 
                geom_vline(data = threshold_lines(), 
                           aes(xintercept = level, color = factor(id)), 
                           lty = "dashed") +
                xlab(input$Xvar) + ylab("Percentage") +
                scale_fill_discrete("Assigned Cluster") + 
                scale_color_brewer("Threshhold", palette = "Dark2") +
                scale_y_continuous(labels = scales::percent_format())
        } else {
            p <- ggplot(pd, aes(x = x, y = y, color = factor(cluster))) +
                geom_point() + xlab(input$Xvar) + ylab(input$Yvar)  +
                scale_color_discrete("Assigned Cluster")
        }

        if(input$cor_facet & input$tod_facet) {
            p <- p + facet_grid(TOD~Corridor)
        } else if(input$cor_facet) {
          p <- p + facet_wrap(~Corridor)
        } else if(input$tod_facet) {
          p <- p + facet_wrap(~TOD)
        }
        
        p +  theme_bw() 
    })
    
    # This is a table of all the time periods for which we have data in the 
    # view.
    output$table <- DT::renderDataTable(DT::datatable({
        
        # collect weights from input sliders
        wts <- c(
            input$prweight,
            input$sfweight,
            input$aogweight,
            input$rlweight
        )
        names(wts) <- c("pr", "sf", "aog", "rl")
        
        # scale weights to sum to 1
        wts <- wts / sum(wts)

        dt <- threshheld_data() %>%
            
            group_by_all() %>%
            
            # Calculate overall score
            mutate(
                # Overall = sf_score  * wts["sf"] +  pr_score  * wts["pr"] +
                #     aog_score * wts["aog"] + rl_score  * wts["rl"]
                Overall = weighted.mean(c(pr_score, sf_score, aog_score, rl_score), c(pr_weight, sf_weight, aog_weight, rl_weight), na.rm = TRUE)
            ) %>%
            
            select(-x, -y)
        
        
        dt
    }))
    
    output$summary <- renderPrint({
        
        wts <- c(
            input$prweight,
            input$sfweight,
            input$aogweight,
            input$rlweight
        )
        names(wts) <- c("pr", "sf", "aog", "rl")
        
        # scale weights to sum to 1
        wts <- wts / sum(wts)
        
        sum <- threshheld_data %>%
            
            group_by_all() %>%
            
            # Calculate overall score
            mutate(
                # Overall = sf_score  * wts["sf"] +  pr_score  * wts["pr"] +
                #     aog_score * wts["aog"] + rl_score  * wts["rl"]
                Overall = weighted.mean(c(pr_score, sf_score, aog_score, rl_score), c(pr_weight, sf_weight, aog_weight, rl_weight), na.rm = TRUE)
            ) %>%
            
            select(-x, -y, TOD) %>%
            
            summary() 
            
        sum
    })
    
    output$boxPlot <- renderPlot({
        
        wts <- c(
            input$prweight,
            input$sfweight,
            input$aogweight,
            input$rlweight
        )
        names(wts) <- c("pr", "sf", "aog", "rl")
        
        # scale weights to sum to 1
        wts <- wts / sum(wts)
        
        bp <- threshheld_data %>%
            
            group_by_all() %>%
            
            # Calculate overall score
            mutate(
                # Overall = sf_score  * wts["sf"] +  pr_score  * wts["pr"] +
                #     aog_score * wts["aog"] + rl_score  * wts["rl"]
                Overall = weighted.mean(c(pr_score, sf_score, aog_score, rl_score), c(pr_weight, sf_weight, aog_weight, rl_weight), na.rm = TRUE)
            ) %>%
            
            select(-x, -y, TOD) %>%
            
            ggplot(bp %>% mutate(SignalId = factor(SignalId)),
                aes(x = Overall,group = SignalId, colour = SignalId)) +
            stat_ecdf() +
            # geom_point() +
            # stat_summary(fun.y = "mean",
            #              geom = "point",
            #              shape = 18,
            #              size = 1.5,
            #              color = "red") +
            facet_wrap(~TOD) 
        
        bp
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(output$table, ".csv", sep = "")
        },
        content = function(file) {
            write_csv(output$table(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
