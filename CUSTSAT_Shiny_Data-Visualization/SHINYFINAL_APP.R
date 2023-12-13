#install.packages(c("shiny", "ggplot2", "readxl")) #make sure to install them if you have never used them
library(shiny)
library(ggplot2)
library(readxl)

# TODO: set path to file & read xlsx file
dataset <- read_excel("/Users/YOURPATH/QM_bereinigtersteVariablen.xlsx")


# Define the UI
ui <- 
  fluidPage(
    tags$head(
      tags$style(HTML("
      .custom-header {
        color: darkblue; /* Color of the title */
        text-align: center; /* Center align the title */
        font-size: 24px; /* Increase font size */
      }
    .custom-spacing {
        margin-top: 84px; /* Add space above the element */
      }
    "))
    ),    
    # Custom header panel
    headerPanel(
      tags$h1("Customer Satisfaction Analysis", class = "custom-header")
    ),
    
    # First row with sidebar and main panel
    fluidRow(
      sidebarLayout(
        
        sidebarPanel(
          helpText("Boxplot"),
          sliderInput("zoom", "Boxplot Zoom Level", min = 1, max = 100, value = 10, step = 5),
          
          helpText("Histogram"),
          sliderInput("zoomHistogram", "Histogram Zoom Level", min = 1, max = 100, value = 50, step = 5),
          checkboxInput("showMean", "Actual Mean", FALSE),
          checkboxInput("showEstimated", "Estimated Delivery Time Distribution", FALSE),
          
          helpText("Violin Plot"),
          sliderInput("violinZoom", "Violin Plot Zoom Level", min = 1, max = 100, value = 50, step = 5),
          checkboxInput("showRegression", "Regression Line", FALSE)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Boxplot", plotOutput("boxplot")),
            tabPanel("Histogram", plotOutput("histogram")) 
          )
        )
      )
    ),
    
    # create additional row for additional plots 
    fluidRow(
      column(12, plotOutput("violinPlot", height = "400px"),  
      tags$div(class = "custom-spacing"), #get some space
      column(12, plotOutput("correlationMatrix", height = "400px"))
             
      )
    )
  )



server <- function(input, output) {
  
 
  # pre calculations
  dataset$order_purchase_timestamp <- as.POSIXct(dataset$order_purchase_timestamp, format="%Y-%m-%d %H:%M:%S")
  dataset$order_delivered_customer_date <- as.POSIXct(dataset$order_delivered_customer_date, format="%Y-%m-%d %H:%M:%S")
  dataset$order_estimated_delivery_date <- as.POSIXct(dataset$order_estimated_delivery_date, format="%Y-%m-%d %H:%M:%S")
  
  # calculate additional variables
  dataset$estimated_delivery_time <- round(as.numeric(difftime(dataset$order_estimated_delivery_date, dataset$order_approved_at, units="days")), 2)
  dataset$delay_time <- round(as.numeric(difftime(dataset$order_delivered_customer_date, dataset$order_estimated_delivery_date, units="days")), 2)
  dataset$actual_delivery_time <- round(as.numeric(difftime(dataset$order_delivered_customer_date, dataset$order_approved_at, units="days")), 2) 
  dataset$processing_time <- round(as.numeric(difftime(dataset$order_delivered_customer_date, dataset$order_purchase_timestamp, units = "days")), 2)
  

  

  # Boxplot
  output$boxplot <- renderPlot({
    p <- ggplot(dataset, aes(y = processing_time)) +
      geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red") +
      labs(title = "Boxplot of Order Processing Times",
           y = "Processing Time (days)",
           x = "")
    
    # Adjust zoom based on slider input
    median_time <- median(dataset$processing_time, na.rm = TRUE)
    zoom_range <- input$zoom
    
    # Set lower limit with condition
    lower_limit <- ifelse((median_time - zoom_range) < -4, -4, median_time - zoom_range)
    
    p + coord_cartesian(ylim = c(lower_limit, median_time + zoom_range))
  })
  
  
  
  
  
  
  # Histogram
  output$histogram <- renderPlot({
    p <- ggplot() +
      geom_histogram(data = dataset, aes(x = processing_time), binwidth = 0.9, fill = "lightgreen", color = "darkblue") + 
      labs(title = "Histogram of Delivery Time Distribution ",
           x = "Time (days)",
           y = "Frequency")
    
    # Add mean line for processing time if checkbox is selected
    if (input$showMean) {
      mean_time <- mean(dataset$processing_time, na.rm = TRUE)
      p <- p + geom_vline(xintercept = mean_time, color = "red", linetype = "dashed")
    }
    
    # Add estimated delivery time distribution and its mean line if checkbox is selected
    if (input$showEstimated) {
      mean_estimated_time <- mean(dataset$estimated_delivery_time, na.rm = TRUE)
      p <- p + geom_histogram(data = dataset, aes(x = estimated_delivery_time), binwidth = 0.9, fill = "orange", color = "black", alpha = 0.5) +
        geom_vline(xintercept = mean_estimated_time, color = "purple", linetype = "dashed")
    }
    
    # Adjusting x-axis limits based on the histogram zoom level
    x_max <- max(dataset$processing_time, na.rm = TRUE)
    zoom_factor <- input$zoomHistogram / 100
    xlim_max <- x_max * zoom_factor
    p <- p + xlim(0, xlim_max)
    
    p
  })
  
  
  
  
  
 # ViolinPlot
output$violinPlot <- renderPlot({
  # Replace negative delivery times with 0
  dataset$actual_delivery_time <- ifelse(dataset$actual_delivery_time < 0, 0, dataset$actual_delivery_time)

  p <- ggplot(dataset, aes(x = factor(review_score), y = actual_delivery_time)) +
    geom_violin(trim = FALSE, fill = "lightblue", color = "darkblue") +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "white") +
    labs(title = "Violin Plot of Delivery Times by Review Score",
         x = "Review Score",
         y = "Actual Delivery Time (days)") +
    theme_minimal()
  
  # Adjust zoom based on slider input
  violin_zoom_range <- input$violinZoom
  median_time <- median(dataset$actual_delivery_time, na.rm = TRUE)
  lower_limit <- max(median_time - violin_zoom_range, min(dataset$actual_delivery_time))
  upper_limit <- min(median_time + violin_zoom_range, max(dataset$actual_delivery_time))
  
  p <- p + coord_cartesian(ylim = c(lower_limit, upper_limit))
  
  # Add regression line if checkbox is selected
  if (input$showRegression) {
    p <- p + geom_smooth(method = "lm", aes(group = 1), color = "darkgreen")
  }
  p
})

  
  
  
  
  
  # Correlation Matrix Plot
  output$correlationMatrix <- renderPlot({
    # Ensure all columns are numeric
    dataset$processing_time <- as.numeric(as.character(dataset$processing_time))
    dataset$delay_time <- as.numeric(as.character(dataset$delay_time))
    dataset$payment_value <- as.numeric(as.character(dataset$payment_value))
    dataset$freight_value <- as.numeric(as.character(dataset$freight_value))
    dataset$review_score <- as.numeric(as.character(dataset$review_score))
    
    # Selecting relevant numerical variables for the correlation matrix
    numerical_data <- dataset[, c("processing_time", "delay_time","payment_value",  "freight_value", "review_score")]  
    
    # Calculate the correlation matrix
    corr_matrix <- cor(numerical_data, use = "complete.obs")
    
    # Using corrplot package for visualization
    corrplot::corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", 
                       tl.col = "black", tl.srt = 45, 
                       addCoef.col = "black", cl.cex = 0.7, cl.ratio = 0.1)
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
