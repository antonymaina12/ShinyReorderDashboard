library(shiny)
library(rsconnect)
library(RMySQL)


shinyUI(
    fluidPage(
        # Input: Select a file ----
        fluidRow(column(2, fileInput("file", "Upload excel file", multiple = FALSE, accept = c(".xlsx")), downloadButton('download', 'Download')),
                 # Input: Enter the lead-time -----
                 column(2, sliderInput("leadtme", "Leadtime:", 7, min = 1, max =12, step=1)),
                 # Input : Enter the product index ------
                 column(1, numericInput("index", "Product Index:", 1, min = 1)),
                 # Input : Enter the minimum order quantity ------
                 column(1, numericInput("MOQ", "MOQ:", 100, min = 1)),
                 # Input : Enter the set-up/fixed cost ------
                 column(1, numericInput("fixedcost", "Set-up/fixed cost:", 2000, min = 1)),
                 # Input : Enter the carrying/storate cost ------
                 column(1, numericInput("carryingcost", "Storage/carrying cost(%):", 20, min = 1)),
                 # Input : Enter the service level ------
                 column(2, sliderInput("servicelevel", "Service level in %:", 95, min = 80, max=100, step = 1))
                 ),
        
        fluidRow(column(3, plotOutput("plot1")),
                 column(4, tableOutput("table1")),
                 column(3, plotOutput("plot2"))
                 ),
        
        fluidRow(column(10, dataTableOutput("table"))
                 )
        )
)
