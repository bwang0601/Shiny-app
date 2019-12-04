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


# Setup Cluster variables
set.seed(3)
cluster_variables <- c("SFPerCycle", "PercentAOG", "PlatoonRatio", 
                       "TotalRedLightViolations", "PercentForceOffs")

# Read corridor dataset
df <- read_rds("dfCorridors_NA.rds") %>%
    select(BinStartTime, SignalId, ApproachId, TotalVolume, SFPerCycle,
           PercentAOG, PlatoonRatio, TotalRedLightViolations, PercentForceOffs,
           AMPeak, Corrdior) %>%
    # change ridiculous data points to NA
    mutate(
        PercentForceOffs = ifelse(PercentForceOffs > 1.3, NA, PercentForceOffs),
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
                           end   = "2018-10-25")

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plotdata <- reactive({
        pd <- tibble(
            x = df[[input$Xvar]],
            y = df[[input$Yvar]],
            cluster = df[["cluster"]],
            Corridor = df$Corrdior,
            TOD = ifelse(df$AMPeak, "AMPeak", "MidDay"),
            Date = df$BinStartTime,
            SignalId = df$SignalId
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
    
    threshold_lines <- reactive({
        thresholds %>%
            filter(variable %in% input$Xvar)
        
    })
   
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
}

# Run the application 
shinyApp(ui = ui, server = server)
