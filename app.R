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

# Read corridor dataset

# Setup Cluster variables
set.seed(3)

cluster_variables <- c("SFPerCycle", "PercentAOG", "PlatoonRatio", "TotalRedLightViolations", "PercentForceOffs")

dfCorridors_NA <- readRDS("dfCorridors_NA.rds") %>%
    select(BinStartTime, SignalId, ApproachId, TotalVolume, SFPerCycle,
           PercentAOG, PlatoonRatio, TotalRedLightViolations, PercentForceOffs,
           AMPeak, Corrdior)

complete_dfCorridors <- na.omit(dfCorridors_NA) %>%
    filter(!ApproachId %in% c(3169, 3172, 5883, 6007, 6042, 6565))

cluster_dfCorridors <- complete_dfCorridors %>%
    select(cluster_variables) %>%
    kmeans(center = 5)

scaled_dfCorridors <- complete_dfCorridors %>%
    select(cluster_variables) %>%
    mutate_all(scale) %>% # want to scale the data to make it normalized for kmeans
    kmeans(centers = cluster_dfCorridors$centers %>% scale())

clustered_dfCorridors <- complete_dfCorridors %>%
    mutate(cluster = cluster_dfCorridors$cluster,
           scaled_cluster = scaled_dfCorridors$cluster) 

dfCorridors <- left_join(
    dfCorridors_NA, clustered_dfCorridors,
    by = c("BinStartTime", "SignalId", "ApproachId", "TotalVolume", "SFPerCycle", 
           "PercentAOG", "PlatoonRatio", "TotalRedLightViolations", "PercentForceOffs", "AMPeak", "Corrdior")
) %>%
    write_rds("dfCorridors.rds")

optional_xaxis <- c("TotalVolume", "SFPerCycle", "PercentAOG", "PlatoonRatio", "TotalRedLightViolations", "PercentForceOffs")

select_signalId <- c(unique(dfCorridors$SignalId))

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
                               c(unique(as.character(dfCorridors$Corrdior))), 
                               inline = TRUE),
            
            # Time of day filter
            checkboxGroupInput("tod", "Select a Time of Day:",
                               c("AMPeak",
                                 "MidDay"),
                               inline = TRUE),
            
            # Signals filter
            shiny::selectInput("Intersection", "Select a SignalId", choices = c("All", select_signalId)),
            
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
           x = dfCorridors[[input$Xvar]],
           y = dfCorridors[[input$Yvar]],
           Cluster = dfCorridors[["scaled_cluster"]],
           Corridor = dfCorridors$Corrdior,
           TOD = ifelse(dfCorridors$AMPeak, "AMPeak", "MidDay"),
           Date = dfCorridors$BinStartTime,
           SignalId = dfCorridors$SignalId
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
   
    output$distPlot <- renderPlot({
        
        pd <- plotdata()
        
        if(input$Xvar == input$Yvar & input$Xvar == "TotalVolume") {
            p <- ggplot(pd, aes(x = x, fill = factor(Cluster))) +
                geom_histogram(aes(y = stat(width*density))) + xlab(input$Xvar) + ylab("Percentage") 
        } 
        else if(input$Xvar == input$Yvar & input$Xvar == "SFPerCycle") {
            p <- ggplot(pd, aes(x = x, fill = factor(Cluster))) +
                geom_histogram(bins = 100, aes(y = stat(width*density))) + xlab(input$Xvar) + ylab("Percentage") +
                geom_vline(xintercept = 0.05) + geom_vline(xintercept = 0.20) + geom_vline(xintercept = 0.40) + geom_vline(xintercept = 0.70)
        } 
        else if(input$Xvar == input$Yvar & input$Xvar == "PercentAOG") {
            p <- ggplot(pd, aes(x = x, fill = factor(Cluster))) +
                geom_histogram(bins = 100, aes(y = stat(width*density))) + xlab(input$Xvar) + ylab("Percentage") +
                geom_vline(xintercept = 0.2) + geom_vline(xintercept = 0.4) + geom_vline(xintercept = 0.6) + geom_vline(xintercept = 0.8)
        }
        else if(input$Xvar == input$Yvar & input$Xvar == "PlatoonRatio") {
            p <- ggplot(pd, aes(x = x, fill = factor(Cluster))) +
                geom_histogram(bins = 30, aes(y = stat(width*density))) + xlab(input$Xvar) + ylab("Percentage") +
                geom_vline(xintercept = 0.5) + geom_vline(xintercept = 0.85) + geom_vline(xintercept = 1.15) +
                geom_vline(xintercept = 1.5)
        }
        else if(input$Xvar == input$Yvar & input$Xvar == "TotalRedLightViolations") {
            p <- ggplot(pd, aes(x = x, fill = factor(Cluster))) +
                geom_histogram(bins = 100, aes(y = stat(width*density))) + xlab(input$Xvar) + ylab("Percentage") +
                geom_vline(xintercept = 0.5) + geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)
        }
        else if(input$Xvar == input$Yvar & input$Xvar == "PercentForceOffs") {
            p <- ggplot(pd, aes(x = x, fill = factor(Cluster))) +
                geom_histogram(bins =100, aes(y = stat(width*density))) + xlab(input$Xvar) + ylab("Percentage") +
                geom_vline(xintercept = 0.50) + geom_vline(xintercept = 0.80) + geom_vline(xintercept = 0.95) + geom_vline(xintercept = 0.2)
        }
        else {
            
            p <- ggplot(pd, aes(x = x, y = y, color = factor(Cluster))) +
                geom_point() + xlab(input$Xvar) + ylab(input$Yvar) 
        }

        # p +  facet_grid(TOD~Corridor) +
          p +  theme_bw() 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
