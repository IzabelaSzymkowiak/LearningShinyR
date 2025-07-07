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
library(scales)

dataset <- RKaggle::get_dataset("heemalichaudhari/adidas-sales-dataset")
head(dataset)
dataset = dataset[3:nrow(dataset), ]
head(dataset)

colnames(dataset) = dataset[1, ]
dataset = dataset[-1, ]
head(dataset)

dataset[, c("Retailer", "Region", "State", "City", "Product", "Sales Method")] <- lapply(dataset[, c("Retailer", "Region", "State", "City", "Product", "Sales Method")], as.character)
dataset[, c("Retailer ID", "Price per Unit", "Units Sold", "Total Sales", "Operating Profit", "Operating Margin")] <- lapply(dataset[, c("Retailer ID", "Price per Unit", "Units Sold", "Total Sales", "Operating Profit", "Operating Margin")], as.numeric)
dataset$`Invoice Date` <- as.Date(as.numeric(dataset$`Invoice Date`), origin = "1899-12-30")

describe(dataset)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Adidas Sales Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "selectedRetailer", label = "Select Retailer", selected = "Walmart", choices = dataset$Retailer)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("chart")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$chart <- renderPlot({
    si_labels <- label_number(scale_cut = cut_si(unit = ""))
    subset <- dataset[dataset$Retailer == input$selectedRetailer, ]
    
    sales_per_region =  subset %>%
      group_by(Region) %>%
      summarise(totalSales = sum(`Total Sales`))
    
    p <- ggplot(sales_per_region, aes(x = Region, y = totalSales)) + geom_col(fill = "steelblue") + labs(title = paste("Total Sales by Region:", input$selectedRetailer), x = "Region", y = "Total Sales") + scale_y_continuous(labels = si_labels) + theme_minimal() + geom_text(aes(label = totalSales), vjust = -0.5, size = 3)
    p
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

