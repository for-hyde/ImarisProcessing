library(shiny)
library(umap)
library(Rtsne)
library(ggplot2)
library(dplyr)
library(stats)  # For PCA

# Define the UI
ui <- fluidPage(
  titlePanel("Dimensionality Reduction Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("dirPath", "Enter the path to the directory:", value = ""),
      selectInput("method", "Choose a reduction method:", 
                  choices = c("UMAP", "t-SNE", "PCA")),
      actionButton("process", "Process CSV Files"),
      textOutput("status")
    ),
    
    mainPanel(
      plotOutput("dimPlot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Reactive value to store the processed data
  data <- reactiveVal(NULL)
  
  observeEvent(input$process, {
    dirPath <- input$dirPath
    if (!dir.exists(dirPath)) {
      output$status <- renderText("Invalid directory path. Please check the input.")
      return(NULL)
    }
    
    # Get list of CSV files in the directory
    csv_files <- list.files(dirPath, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      output$status <- renderText("No CSV files found in the directory.")
      return(NULL)
    }
    
    # Combine and process CSV files
    combined_data <- do.call(rbind, lapply(csv_files, function(file) {
      df <- read.csv(file)
      # Custom processing: Standardize numeric columns
      df <- df %>%
        mutate(across(where(is.numeric), ~ scale(.)[, 1]))
      return(df)
    }))
    
    # Store processed data
    data(combined_data)
    output$status <- renderText("CSV files processed successfully. Choose a reduction method.")
  })
  
  # Generate dimensionality reduction plot based on selected method
  output$dimPlot <- renderPlot({
    combined_data <- data()
    if (is.null(combined_data)) return(NULL)
    
    method <- input$method
    numeric_data <- combined_data %>% select(where(is.numeric))
    
    if (ncol(numeric_data) < 2) {
      output$status <- renderText("Not enough numeric columns for dimensionality reduction.")
      return(NULL)
    }
    
    # Perform dimensionality reduction
    reduced_data <- switch(method,
                           "UMAP" = {
                             umap_result <- umap(numeric_data)
                             as.data.frame(umap_result$layout)
                           },
                           "t-SNE" = {
                             tsne_result <- Rtsne(numeric_data, perplexity = 30, check_duplicates = FALSE)
                             as.data.frame(tsne_result$Y)
                           },
                           "PCA" = {
                             pca_result <- prcomp(numeric_data, scale. = TRUE)
                             as.data.frame(pca_result$x[, 1:2])  # Select first 2 principal components
                           }
    )
    
    colnames(reduced_data) <- c("Dim1", "Dim2")
    
    # Create plot
    ggplot(reduced_data, aes(x = Dim1, y = Dim2)) +
      geom_point(alpha = 0.6) +
      labs(title = paste(method, "Visualization"), x = "Dimension 1", y = "Dimension 2") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
