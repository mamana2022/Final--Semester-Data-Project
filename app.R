library(dplyr)
library(ggplot2)
library(plotly)
library(reactable)
library(shiny)
library(DT)
library(bslib)
library(shinythemes)
library(skimr)

bookings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

ui <- tagList(
  shinythemes::themeSelector(),
  ## UI code
  navbarPage(
    theme = shinytheme("cerulean"),
    "Hotel Bookings Data",
    tabPanel( "Home",
     
      mainPanel(
        h1("Hotel Booking Demand Datasets"),
        p("This data describes two datasets. Both datasets share the same structure, with 31 variables describing the 40,060 observations of H1 and 79,330 observations of H2. Each observation represents a hotel booking."),
        plotOutput("plot", click = "plot_click"),
        h4("ADR against Customer Type"),
        p("ADR here stands for Average Daily Rate, which is calculated by dividing the sum of all lodging transactions by the total number of staying nights."),
        p("This graph illustrates the various customer groups and their average lodging transactions in the hotel."),
        br(),
        DTOutput("data"),
        br(),
        plotOutput("timespent"),
        br()
      )
    ),
    tabPanel(
      "Hotel Types",
   
      selectInput(inputId = "HotelInput", label = "Hotel:",
                  choices = c("City Hotel", "Resort Hotel")),
      plotOutput("histogram"),
      numericInput(
        inputId = "binwidth",
        label = "Binwidth:",
        value = 10,
        min = 1,
        max = 100
      ),
      actionButton(inputId = "resetFilters", label = "Reset Filters"),
      br(),
      h4("Filtered Data Table"),
      DT::dataTableOutput(outputId = "filteredTable"),
      plotOutput("scatterplot ")
    ),
    tabPanel("Distribution Channel",
             sidebarPanel( 
             selectInput("deposit_type", "Deposit Type", choices = unique(bookings$deposit_type))),
     mainPanel(        
    plotOutput("plot4"),
      tableOutput("table4"),
    h3("Distribution Channels and the corresponding deposit type"),
    br(),
    p("In a Hotel, there are several types of deposits such as refundable deposits, Non-refundable deposits"),
    p("This graph shows the the deposits made for various distribution channels")
    )
             
             )
  ) )  
#beginning of server function.


server <- function(input, output, session) {
  # Plot 1
  output$plot <- renderPlot({
    ggplot(bookings, aes(lead_time, customer_type)) +
      geom_count(aes(colour = customer_type), size = 3, alpha = 0.6) +
      labs(x = "ADR (Average Daily Rate)", y = "Customer Type") +
      scale_colour_manual(values = c("All" = "gray", "Transient" = "blue", "Contract" = "green", "Transient-Party" = "purple")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$data <- renderDT({
    nearPoints(bookings, input$plot_click, xvar = "adr", yvar = "customer_type")
  })
  
  # Plot 2
  output$histogram <- renderPlot({
    df <- bookings
    
    if (as.character(input$HotelInput) != "Resort Hotel") {
      subset.df <- subset(df, hotel == as.character(input$HotelInput))
      g <- ggplot(subset.df, aes(x = total_of_special_requests, fill = customer_type))
    } else {
      g <- ggplot(df, aes(x = total_of_special_requests, fill = customer_type))
    }
    
    g + geom_histogram(binwidth = 0.5, color = "white") +
      theme_minimal() +
      labs(x = "Total Special Requests", y = "Count", fill = "Customer Type") +
      scale_fill_manual(values = c("All" = "gray", "Transient" = "blue", "Contract" = "green", "Transient-Party" = "purple")) +
      ggtitle("Histogram of Total Special Requests by Customer Type")
  })
  
  # Plot 3
  output$timespent <- renderPlot({
    bookings %>%
      mutate(total_stay = stays_in_week_nights + stays_in_week_nights) %>%
      ggplot() +
      geom_histogram(aes(x = total_stay, fill = customer_type), binwidth = 2, col = "white") +
      facet_wrap(. ~ customer_type, nrow = 1, scales = "free") +
      theme_bw() +
      theme(legend.position = "none") +
      labs(title = "Histogram of Total Night Stayed at Hotel", x = "Nights Stayed", y = "Count")
  })
  
  # Scatter plot
  output$scatterplot <- renderPlot({
    df <- filteredData()
    
    g <- ggplot(df, aes(x = arrival_date_week_number, y = lead_time, color = customer_type))
    g <- g + geom_point(alpha = 0.7)
    g <- g + theme_minimal()
    g <- g + labs(x = "Week Number", y = "Lead Time", color = "Customer Type")
    g <- g + ggtitle("Scatter Plot of Lead Time by Week Number and Customer Type")
    g <- g + theme(legend.position = "top", plot.title = element_text(hjust = 0.5))
    
    g
  })
  
  # Reset filters
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "HotelInput", selected = "City Hotel")
    updateNumericInput(session, "binwidth", value = 10)
  })
  filteredData <- reactive({
    df <- bookings
    
    if (input$HotelInput != "Resort Hotel") {
      df <- df %>% filter(hotel == input$HotelInput)
    }
    
    df
  })
  output$filteredTable <- DT::renderDataTable({
    datatable(filteredData(), options = list(pageLength = 10))
  })
  
  #plot4
  
  # Plot 5
  observe({
    # Filter data based on selected deposit type
    filtered_data <- subset(bookings, deposit_type == input$deposit_type)
    
    # Create ggplot code here
    output$plot4 <- renderPlot({
      ggplot(data = filtered_data) +
        geom_bar(mapping = aes(x = distribution_channel, fill = deposit_type)) +
        labs(
          title = "Deposit type with respect to No. of bookings",
          subtitle = paste0("Deposit Type: ", input$deposit_type)
        )
    })
    
    output$table4 <- renderTable({
      # Group by distribution channel and calculate count
      table_data <- table(filtered_data$distribution_channel)
      
      # Convert table to data frame
      table_df <- as.data.frame(table_data)
      colnames(table_df) <- c("Distribution Channel", "Count")
      
      table_df
    })
  })
  
  
  
  
  
}
  


# Run the application
shinyApp(ui = ui, server = server)
