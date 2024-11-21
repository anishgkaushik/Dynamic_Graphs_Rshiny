library(shiny)
library(plotly)
library(lubridate)
library(readxl)
library(shinythemes)
library(shinyBS)

# UI
ui <- fluidPage(
  theme = shinytheme("united"),  # Change theme to something more colorful and creative,  # Set a vibrant and visually appealing theme
  
  tags$head(
    tags$style(
      HTML("
        body {
          background-image: url('pic.png');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          background-attachment: fixed;
        }
      ")
    )
  ),
  
  titlePanel(div("Shiny Application for Plotting Dynamic Graphs")),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a CSV, TSV or Excel File", accept = c(".csv", ".xlsx", ".tsv")),
      selectInput("xvar", "Select X Variable", choices = NULL),
      selectInput("yvar", "Select Y Variable", choices = NULL),
      selectInput("plotType", "Select Plot Type", choices = c("Scatter Plot", "Line Plot", "Bar Plot", "Pie Chart", "Sankey Chart","Bubble plot", "Histogram", "Box Plot")),
      textInput("plotTitle", "Plot Title", value = "Your Plot Title"),
      selectInput("markerColor", "Select Marker Color", choices = c("blue", "red", "green", "orange", "purple"), selected = "blue"),
      bsTooltip("file", "Upload a CSV or Excel file to visualize data", "right", options = list(container = "body")),
      bsTooltip("xvar", "Select the variable for the X-axis", "right", options = list(container = "body")),
      bsTooltip("yvar", "Select the variable for the Y-axis", "right", options = list(container = "body")),
      bsTooltip("plotType", "Choose the type of plot to visualize data", "right", options = list(container = "body")),
      bsTooltip("plotTitle", "Enter a title for your plot", "right", options = list(container = "body")),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DT::dataTableOutput("dataTable")),
        tabPanel("Plot", plotlyOutput("dataPlot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive function to read uploaded file
  uploaded_data <- reactive({
    req(input$file)  # Ensure file is uploaded
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      data <- read.csv(input$file$datapath, fileEncoding = 'UTF-8-BOM')
    } else if (ext == "xlsx") {
      data <- read_excel(input$file$datapath)
    } else {
      stop("Invalid file type. Please upload a .csv or .xlsx file.")
    }
    data
  })
  
  # Update dropdowns with variable names
  observe({
    data <- uploaded_data()
    updateSelectInput(session, "xvar", choices = names(data))
    updateSelectInput(session, "yvar", choices = names(data))
  })
  
  # # Render the data table with pagination
  # output$dataTable <- DT::renderDataTable({
  #   req(uploaded_data())
  #   DT::datatable(uploaded_data(), options = list(pageLength = 10))
  # })
  # 
  
  
  # Render the data table with pagination
  output$dataTable <- DT::renderDataTable({
    req(uploaded_data())
    
    # Styling for DataTable
    DT::datatable(uploaded_data(), options = list(
      pageLength = 10,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      class = 'stripe hover row-border',
      autoWidth = TRUE,
      columnDefs = list(list(targets = '_all', className = 'dt-center')),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).css({'background-color': '#4c72b0', 'color': '#ffffff'});",
        "}")
      
    ))
  })
  # 
  # Render the plot using plotly
  output$dataPlot <- renderPlotly({
    req(uploaded_data(), input$xvar, input$yvar)
    data <- uploaded_data()
    
    # If xvar is a date column, convert it to Date type and extract month and year
    if (any(grepl("\\d{1,2}/\\d{1,2}/\\d{2,4}", as.character(data[[input$xvar]])))) {
      data[[input$xvar]] <- mdy(data[[input$xvar]])
      data$MonthYear <- format(data[[input$xvar]], "%Y-%m")
      x_data <- data$MonthYear
    } else {
      x_data <- data[[input$xvar]]
    }
    
    # Plot based on user selection
    if (input$plotType == "Scatter Plot") {
      plot_ly(data, x = ~x_data, y = ~get(input$yvar), type = 'scatter', mode = 'markers', marker = list(color = input$markerColor, size = 10)) %>%
        layout(title = input$plotTitle, xaxis = list(title = input$xvar), yaxis = list(title = input$yvar))
    } else if (input$plotType == "Line Plot") {
      # plot_ly(data, x = ~x_data, y = ~get(input$yvar), type = 'scatter', mode = 'lines', line = list(color = input$markerColor)) %>%
      #   layout(title = input$plotTitle, xaxis = list(title = input$xvar), yaxis = list(title = input$yvar))
      
      summarized_data <- data %>% group_by(across(all_of(input$xvar))) %>% summarise(mean_y = mean(get(input$yvar), na.rm = TRUE))
      plot_ly(summarized_data, x = ~get(input$xvar), y = ~mean_y, type = 'scatter', mode = 'lines', line = list(color = input$markerColor)) %>%
        layout(title = input$plotTitle, xaxis = list(title = input$xvar), yaxis = list(title = input$yvar))
      
    } else if (input$plotType == "Histogram") {
      plot_ly(data, x = ~get(input$xvar), type = 'histogram', marker = list(color = input$markerColor)) %>%
        layout(title = input$plotTitle, xaxis = list(title = input$xvar, tickangle = 45), yaxis = list(title = 'Frequency', tickangle = 45))
      # }else if (input$plotType == "Bar Plot") {
      #   summarized_data <- data %>% group_by(across(all_of(input$xvar))) %>% summarise(total = sum(get(input$yvar), na.rm = TRUE))
      #   plot_ly(summarized_data, x = ~get(input$xvar), y = ~total, type = 'bar', marker = list(color = input$markerColor)) %>%
      #     layout(title = input$plotTitle, xaxis = list(title = input$xvar), yaxis = list(title = input$yvar))
      
    } else if (input$plotType == "Bar Plot") {
      plot_ly(data, x = ~x_data, y = ~get(input$yvar), type = 'bar', marker = list(color = input$markerColor)) %>%
        layout(title = input$plotTitle, xaxis = list(title = input$xvar), yaxis = list(title = input$yvar))
      
    } else if (input$plotType == "Area Plot") {
      summarized_data <- data %>% group_by(across(all_of(input$xvar)), across(all_of(input$yvar))) %>% summarise(total = sum(get(input$yvar), na.rm = TRUE)) %>% ungroup()
      plot_ly(summarized_data, x = ~get(input$xvar), y = ~total, color = ~get(input$yvar), type = 'scatter', mode = 'none', fill = 'tonexty', stackgroup = 'one') %>%
        layout(title = input$plotTitle, xaxis = list(title = input$xvar), yaxis = list(title = input$yvar))
      
    } else if (input$plotType == "Bubble Plot") {
      req(input$sizevar)
      if (!input$sizevar %in% names(data) || !is.numeric(data[[input$sizevar]])) {
        output$errorMessage <- renderText({
          "Please select a valid numeric column for bubble size."
        })
        return(NULL)
      }
      plot_ly(data, x = ~x_data, y = ~get(input$yvar), type = 'scatter', mode = 'markers', marker = list(size = ~get(input$sizevar), color = input$markerColor, opacity = 0.6)) %>%
        layout(title = input$plotTitle, xaxis = list(title = input$xvar), yaxis = list(title = input$yvar))
      
    } else if (input$plotType == "Pie Chart") {
      plot_ly(data, labels = ~get(input$xvar), values = ~get(input$yvar), type = 'pie') %>%
        layout(title = input$plotTitle)
    } else if (input$plotType == "Box Plot") {
      plot_ly(data, y = ~get(input$yvar), type = 'box', marker = list(color = input$markerColor)) %>%
        layout(title = input$plotTitle, yaxis = list(title = input$yvar))
    }
    
    else if (input$plotType == "Sankey Chart") {
      unique_labels <- unique(unlist(data))
      source_target_pairs <- data.frame(
        source = as.character(data[[1]]),
        target = as.character(data[[2]]),
        value = as.numeric(data[[3]])
      )
      source_indices <- match(source_target_pairs$source, unique_labels) - 1
      target_indices <- match(source_target_pairs$target, unique_labels) - 1
      
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = unique_labels,
          pad = 15,
          thickness = 20,
          line = list(color = "black", width = 0.5)
        ),
        link = list(
          source = source_indices,
          target = target_indices,
          value = source_target_pairs$value
        )
      ) %>%
        layout(title = input$plotTitle)
    }
  })
}
# Run the application
shinyApp(ui = ui, server = server)