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

# Read corridor dataset
dfCorridors <- readRDS("~/ATSPM/data/dfCorridors.rds") 

optional_xaxis <- c("TotalVolume", "SplitFailures", "PercentAOG", "PlatoonRatio", "TotalRedLightViolations", "PercentForceOffs")


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
           Date = dfCorridors$BinStartTime
           ) 
        
        if (!is.null(input$Cor)){
          pd <- pd %>% filter(Corridor %in% input$Cor)
        } 
        
        if (!is.null(input$tod)){
          pd <- pd %>% filter(TOD %in% input$tod)
        }
        
        if (!is.null(input$daterange1)){
          pd <- pd %>% filter(Date >= input$daterange1[1], Date <= input$daterange1[2])
        }
        
        pd
    })
   
    output$distPlot <- renderPlot({

        p <- ggplot(plotdata(), aes(x = x, y = y, color = factor(Cluster))) +
            geom_point() + xlab(input$Xvar) + ylab(input$Yvar) +
            facet_grid(TOD~Corridor) +
            theme_bw()
       
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)