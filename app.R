library(shiny)
library(umap)
library(Rtsne)
library(ggplot2)
library(dplyr)
library(stats)  # For PCA
library(tcltk)

# Define the UI
ui <- fluidPage(
  titlePanel("Dimensionality Reduction Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      #Select File(s)
      
      
      #Display files
      
      #List identified features
      
      #Select cell condition
      
      #Give overview for each condition identified. 
      
      #Choose plot to determine
    ),
    
    mainPanel(
      plotOutput("dimPlot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  #Function for 
    
}

# Run the application
shinyApp(ui = ui, server = server)
