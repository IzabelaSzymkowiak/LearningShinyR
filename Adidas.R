####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)
library(RKaggle)
library(Hmisc)
library(dplyr)
library(ggplot2)
dataset <- RKaggle::get_dataset("heemalichaudhari/adidas-sales-dataset")
head(dataset)
dataset = dataset[3:nrow(dataset), ]
head(dataset)

colnames(dataset) = dataset[1, ]
dataset = dataset[-1, ]
head(dataset)

describe(dataset)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sales"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "selectedRetailer", label = "select option", selected = "Walmart", choices = dataset$Retailer)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("chart")
      #verbatimTextOutput("chart")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$chart <- renderPlot({
    subset <- dataset[dataset$Retailer == input$selectedRetailer, ]
    p <- ggplot(data = subset) + geom_bar(mapping = aes(x = State))
    p
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

