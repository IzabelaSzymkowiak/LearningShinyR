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
# Define UI 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Adidas Sales Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: 
      selectInput(inputId = "selectedRetailer", label = "Select Retailer", choices = dataset$Retailer),
      dateRangeInput(inputId = "dateRange", label = "Select date range:", start = min(dataset$`Invoice Date`), end = max(dataset$`Invoice Date`), startview = "month", separator = " to "),
    ),
    

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: column chart
      plotOutput("chart1"),
      plotOutput("chart2")
    )
  )
)


# Define server logic required to draw a column chart
server <- function(input, output) {
  
  
  output$chart1 <- renderPlot({
    
    subset1 <- dataset[dataset$Retailer == input$selectedRetailer, ]
    subset2 <- subset1 %>% filter(subset1$`Invoice Date` >= as.Date(input$dateRange[1]), subset1$`Invoice Date` <= as.Date(input$dateRange[2])) 
    
    sales_per_region =  subset2 %>%
      group_by(Region) %>%
      summarise(totalSales = sum(`Total Sales`))
    
    if (nrow(sales_per_region) == 0) {
      return(NULL)  
    }
    
    si_labels <- label_number(scale_cut = cut_si(unit = ""))
    
    p <- ggplot(sales_per_region, aes(x = Region, y = totalSales)) + geom_col(fill = "steelblue") + labs(title = paste("Total Sales by Region:", input$selectedRetailer), x = "Region", y = "Total Sales") + scale_y_continuous(labels = si_labels) + theme_minimal() + geom_text(aes(label = totalSales), vjust = -0.5, size = 3)
    p
  })
  
  output$chart2 <- renderPlot({
    
    subset1 <- dataset[dataset$Retailer == input$selectedRetailer, ]
    subset2 <- subset1 %>% filter(subset1$`Invoice Date` >= as.Date(input$dateRange[1]), subset1$`Invoice Date` <= as.Date(input$dateRange[2])) 
    
    sales_per_region = subset2 %>% group_by(Region) %>% # Variable to be transformed
      summarise(totalSales = sum(`Total Sales`)) %>% 
      mutate(perc = `totalSales` / sum(`totalSales`) * 100)  %>% 
      arrange(perc) %>%
      mutate(percentage = perc, label = paste0(round(percentage, 1), "%"))
    
    if (nrow(sales_per_region) == 0) {
      return(NULL)  
    }

    si_labels <- label_number(scale_cut = cut_si(unit = ""))
    
    p <- ggplot(sales_per_region, aes(x = "", y = perc, fill = Region)) + geom_bar(stat = "identity", width = 1, color = "white") + coord_polar(theta = "y", start = 0) + theme_void() + geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) + labs("Sales Percentage Distribution by Region")
    p
  })
  

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

