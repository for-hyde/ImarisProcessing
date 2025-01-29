library(shiny)
library(umap)
library(Rtsne)
library(ggplot2)
library(dplyr)
library(stats)  # For PCA
library(tcltk)
library(shinyFiles)

# Define the UI
ui <- fluidPage(
  titlePanel("Dimensionality Reduction Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      
      #Select Folder
      
      #List identified features
      
      #Select cell condition
      
      #Give overview for each condition identification 
      
      #Choose plot to determine
    ),
    
    mainPanel(
      # Data table of compiled dataframe
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
