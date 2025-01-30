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
      shinyDirButton("folder", "Select Folder", "Choose a folder"),
      #Debugging
      textOutput("test"),
      #List identified features
      verbatimTextOutput("features"),
      #Select cell condition
      selectInput("condition_input", "Select text showing the codition", choices = NULL),
      #Select well documentation 
      selectInput("well_input","Select text showing well", choices = NULL),
      #Choose plot to determine
      selectInput("chart","Type of Dimensionality Reduction", c("PCA","t-SNE","UMAP"), selected = "PCA")
    ),
    
    mainPanel(
      # Data table of compiled dataframe
      tableOutput("data_table"),
      # Output plot
      plotOutput("dimPlot")
      
      
    )
  )
)

# Define the server
server <- function(input, output, session) {
  #Initialize values that are reactive and will change based on input (folder, condition_inputs, )
#-------------------------------------------------------------------------------  
  #Defines drives available to use. May need to be updated. 
  volumes <- c(Home = fs::path_home(), "C:" = "C:/", "D:" = "D:/", "Z:" = "Z:/")
  
  #Data for the dataframe, set to NULL until structured data generated. 
  structured_data <- reactiveVal(NULL)
  #Data for found features, Updated after initial parsing of csv files. 
  found_features <- reactiveVal(NULL)
  
  #Reactive value to be updated after creation of df
  wells <- reactiveVal(NULL)
  #Reavative value for conditions to be updated after creation of df
  conditions <- reactiveVal(NULL)
  
  test_text <- reactiveVal(NULL)
  
  shinyDirChoose(input, "folder", roots = volumes, session = session)

  #Reaction to selection of folder. 
#-------------------------------------------------------------------------------
  #Event for when an input$folder is generated with the folder input (shinyDirChoose)
  observeEvent(input$folder, {
    #Must have a valid input for req(input$folder)
    req(input$folder)
    #Generate a folder path to work with based on the value for input$folder.
    folder_path <- parseDirPath(volumes, input$folder)
    #Ensure the folder_path is valid
    if(!is.null(folder_path)){
    #Create subfolders list of all folders, including main directory.
      subfolders <- list.dirs(folder_path)
    }
    
    #Create empty character vector for of csv_files. 
    csv_files <- c()
    #Iterate through all directories from subfolders list. 
    for(subfolder in subfolders){
      #Append the csv_files list with the csv_files for the iteration of the subfolder. 
      csv_files <- c(csv_files, 
                     list.files(subfolder, pattern = "*.csv", full.names = TRUE)
      )}
    #Remove overview csv files. (different size than others)
    csv_files <- csv_files[!grepl("Cells_\\d+_Overall\\.csv", csv_files)]
    
    #Generate features based on csv files. 
    features <- gsub(".*/Cells_\\d+_(.*)\\.csv", "\\1", csv_files)
    unique(features)
    found_features(features)
    
    
    
    #test_text(features)
    
    
    
    
    }
    )

    #Extract feature values and cell ids
    #Append features and values to list
  #Generate Dataframe
  
  
  
  
  
  # Reaction to selection from condition input
#-------------------------------------------------------------------------------
  #Alter the condition column of the dataframe to be match the pattern shown. 

  
  # Reaction to selection of well input
#-------------------------------------------------------------------------------
 #Alter the well_id column of the dataframe to identify the cells individually. 
  
  # 
  output$features <- renderText({
    req(found_features())
    paste("Found feature:\n", found_features(), "\n")
  })
  output$data_table <- renderTable({
    req(structured_data)
  })
  output$test <- renderText({
    req(test_text())
    paste("Value from test:", test_text())
  })
  
  
  }

# Run the application
shinyApp(ui = ui, server = server)
