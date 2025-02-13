library(shiny)
library(umap)
library(Rtsne)
library(ggplot2)
library(dplyr)
library(stats)  # For PCA
library(tcltk)
library(shinyFiles)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Imaris Data Processing Application"),
  
  sidebarLayout(
    sidebarPanel(
      #Select Folder
      shinyDirButton("folder", "Select Folder", "Choose a folder"),
      #List identified features to select
      checkboxGroupInput("features", "Select Features to analyze:", choices = NULL),
      #Select cell condition
      textInput("condition_input_text", "Enter a condition:", ""),
      #actionButton for text input
      actionButton("add_condition","Add Condition"),
      uiOutput("condition_checkboxes"),
      #Select well documentation 
      selectInput("well_input","Select text showing well", choices = NULL),
      #Selection to analyze nuclei information or not. 
      actionButton("table_merge", "Merge Tables?"),
      
      #Choose plot to determine
      selectInput("chart","Type of Dimensionality Reduction", c("PCA","t-SNE","UMAP"), selected = "PCA"),
      #Debugging
      textOutput("test"),
      
      actionButton("confirm", "Confirm selection")
    ),
    
    mainPanel(
      # Data table of compiled dataframe
      DTOutput("data_table"),
      
      DTOutput("nuclear_table"), 
      # Output plot
      uiOutput("dynamicPlot")
      
      
    )
  )
)

server <- function(input, output, session) {
  #Initialize values that are reactive and will change based on input (folder, condition_inputs, )
#-------------------------------------------------------------------------------  
  #Defines drives available to use. May need to be updated. 
  volumes <- c(Home = fs::path_home(), "C:" = "C:/", "D:" = "D:/", "Z:" = "Z:/")
  
  #Create a master_data variable for the complete df that will not be after it is initialized. 
  master_data <- reactiveVal(NULL)
  #Data for the dataframe, set to NULL until structured data generated. 
  structured_data <- reactiveVal(NULL)
  
  #Create a data_frame for nuclear data.
  nuclear_data <- reactiveVal(NULL)
  
  #Data for found features, Updated after initial parsing of csv files. 
  found_features <- reactiveVal(NULL)
  
  #Reactive value to be updated after creation of df
  wells <- reactiveVal(NULL)
  
  #Reactive value to store selected conditions
  #Initialized as a character vector.
  selected_conditions <- reactiveVal(character())
  
  #Debugging tool. Intialize as null. 
  test_text <- reactiveVal(NULL)
  
  shinyDirChoose(input, "folder", roots = volumes, session = session)
  
  #Functions for use throughout application
#-------------------------------------------------------------------------------  
  iter_csv <- function(file_list, feature_list){
    #Empty list
    temp_list <- list()
    #Empty dataframe
    result_df <- data.frame()
    #Initialized counter. 
    counter <- 0
    
    for(i in seq_along(file_list)){
      #Read CSV, must skip first 3 lines
      temp_data <- read.csv(file_list[i], skip = 3)
      #Select feature from feature list.
      feature = feature_list[counter + 1]
      
      #Extract values from first column as values for feature
      temp_list[[feature]] <- temp_data[[1]]
      
      #IDcolumn 
      id_col <- temp_data[6]
      temp_list[["ID"]] <- id_col
      
      #pull original name column
      orig_name <- temp_data[['Original.Image.Name']]
      
      #Use the column to populate well and condition columns.
      wells <- orig_name
      condition <- orig_name
      temp_list[["Well_ID"]] <- wells
      temp_list[["Condition"]] <- condition
      
      #Increment counter
      counter <- counter + 1
      
      #Once counter has reached the length of the features list
      if(counter >= length(feature_list)){
        #populate temp_df with the temp_list
        temp_df <- as.data.frame(temp_list)
        #Add temp_df to result_df
        result_df <- rbind(result_df, temp_df)
        #Reset temp_list
        temp_list <- list()
        #Reset counter
        counter <- 0 
      }
    }
    if (identical(deparse(substitute(file_list)), "nucleus_files")){
      nuclear_data(result_df)
    }
    else{
      #Set structured_data and master_df to the generated df.  
      structured_data(result_df)
      master_data(result_df)
    }

  }


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
      )
    }
    #Remove overview csv files. (different size than others)
    csv_files <- csv_files[!grepl("Overall\\.csv", csv_files)]
    
    #Generate list of nucleus csv_files. 
    nucleus_files <- csv_files[grepl("Nucleus", csv_files)]
    
    #Remove nucleus csv_files from from list. 
    #Will be updated eventually to allow for nucleus data to also be observed. 
    csv_files <- csv_files[!grepl("Nucleus", csv_files)]
    
    #Generate features based on csv files. 
    features <- gsub(".*(Cell_[^/]+)\\.csv", "\\1", csv_files)
    features <- unique(features)

    #populate found_features with features parsed from csv files. 
    
    
    iter_csv(csv_files, features)
    
    if(!is.null(nucleus_files) && length(nucleus_files) > 0){
      nucleus_files <- nucleus_files[!grepl(".*Ratio.csv", nucleus_files)]
      nuc_features <- gsub(".*(Nucleus_[^/]+)\\.csv$", "\\1", nucleus_files)
      iter_csv(nucleus_files, nuc_features)
      features <- c(features, nuc_features)
    }
    found_features(features)

    
})
  
  #Checks for found features and populates the list of choices that can be used. 
  observe({
    req(found_features())
    
    updateCheckboxGroupInput(
      session, 
      "features", 
      choices = found_features(),
      selected = found_features())
    
  })
  #Reaction to merging files
#-------------------------------------------------------------------------------
  observeEvent(input$table_merge, {
    req(length(nuclear_data) > 0)
    comb_df <- merge(nuclear_data(), master_data(), by.x = c("CellID", "Condition"),
                     by.y = c("ID","Condition"), all.x = TRUE)
    
    structured_data(comb_df)
    master_data(comb_df)
    #Clear nuclear dataframe
    nuclear_data(data.frame())
    })
  
  # Reaction to selection from condition input
#-------------------------------------------------------------------------------
  
  # When "Add Condition" button is clicked
  observeEvent(input$add_condition, {
    req(input$condition_input_text)  # Ensure user entered a value
    
    # Get current selected conditions
    current_conditions <- selected_conditions()
    
    # Avoid adding duplicates
    if (!(input$condition_input_text %in% current_conditions)) {
      selected_conditions(c(current_conditions, input$condition_input_text))
    }
  })
  
  # Render dynamic checkboxes for selected conditions
  output$condition_checkboxes <- renderUI({
    req(selected_conditions())  # Ensure there are selected conditions
    checkboxGroupInput("condition_input", "Select conditions to use:", choices = selected_conditions(), selected = selected_conditions())
  })
  
  
  
  # Render dynamic checkboxes for selected conditions
  observeEvent(input$confirm, {
    req(master_data(), input$condition_input)  # Ensure data and selection exist
    
    df <- master_data()  # Retrieve current dataframe
    
    # Ensure "Condition" column exists
    if (!"Condition" %in% colnames(df)) {
      warning("Column 'Condition' not found in the dataframe!")
      return()
    }
    
    # Extract all condition strings
    conds <- df$Condition
    
    # Ensure there are valid conditions to process
    if (length(conds) == 0 || all(is.na(conds))) {
      warning("No valid conditions found in the dataframe!")
      return()
    }
    
    # Initialize new condition column
    extracted_conditions <- character(length(conds))
    
    for (i in seq_along(conds)) {
      if (is.na(conds[i])) next  # Skip NA values
      
      condition_parts <- unlist(strsplit(conds[i], "_"))  # Split by "_"
      
      # Look for the selected condition(s) in the split text
      match_index <- which(condition_parts %in% input$condition_input)
      
      if (length(match_index) > 0) {
        for (index in match_index) {
          # Find neighboring text
          prefix <- if (index > 1) condition_parts[index - 1] else ""
          suffix <- if (index < length(condition_parts)) condition_parts[index + 1] else ""
          
          # Construct regex pattern dynamically
          if (prefix != "" && suffix != "") {
            pattern <- paste0(".*_", prefix, "_(.*?)_", suffix, "_.*")
          } else if (prefix == "") {
            pattern <- paste0("^(.*?)_", suffix, "_.*")
          } else if (suffix == "") {
            pattern <- paste0(".*_", prefix, "_(.*?)$")
          } else {
            pattern <- paste0("^(.*?)$")
          }
          
          # Extract using regex
          extracted_value <- gsub(pattern, "\\1", conds[i])
          
          # Save extracted value
          extracted_conditions[i] <- extracted_value
        }
      }
    }
    
    # Update Condition column
    df$Condition <- extracted_conditions
    
    df <- df[df$Condition != "" & !is.na(df$Condition), ]
    
    structured_data(df)  # Update the dataframe
  })
  
  
  # Reaction to selection of well input
#-------------------------------------------------------------------------------
 #Alter the well_id column of the dataframe to identify the cells individually. 
  observe({
    req(structured_data())  # Ensure the dataframe exists
    
    well <- structured_data()$Well_ID  # Extract condition column
    
    if (length(well) == 0 || all(is.na(well))) {
      return()  # Exit if there are no valid Well_ID entries
    }
    
    # Extract first non-NA entry safely
    first_well <- na.omit(well)[1]
    
    if (!is.na(first_well) && nzchar(first_well)) {
      # Split by "_"
      well_list <- unique(unlist(strsplit(first_well, "_")))
      
      # Update the reactive conditions list
      wells(well_list)
    }
  })
  
  observe({
    req(wells())
    
    updateSelectInput(session, "well_input", choices = wells())
    
  })
  
  
  observe({
    req(structured_data())  # Ensure the dataframe exists
    
    # Extract "Well_ID" column
    well_col <- structured_data()$Well_ID
    
    # Ensure there are valid well IDs to process
    if (length(well_col) == 0 || all(is.na(well_col))) {
      return()
    }
    
    # Extract all well identifiers using regex pattern (Letter followed by a number)
    extracted_wells <- unique(unlist(regmatches(well_col, gregexpr("[A-Za-z]\\d+|\\d+[A-Za-z]", well_col))))
    
    
    # If wells were found, update the reactive value
    if (length(extracted_wells) > 0) {
      wells(sort(extracted_wells))  # Sort alphabetically (optional)
    }
  })
  
  # Update the select input for wells
  observe({
    req(wells())
    updateSelectInput(session, "well_input", choices = wells())
  })
  
  # Extract Well ID based on user input
  observeEvent(input$confirm, {
    req(structured_data(), input$well_input)  # Ensure data and selection exist
    
    df <- structured_data()  # Retrieve current dataframe
    
    if (!"Well_ID" %in% names(df)) return()  # Ensure Well_ID column exists
    
    # Apply regex substitution to extract only the well ID (letter + number)
    df$Well_ID <- gsub(".*?([A-Za-z]\\d+).*", "\\1", df$Well_ID)
    
    # Update the structured dataframe
    structured_data(df)
  })
  
  
  #Creation of the PCA, UMAP, and t-SNE graphs. 
#-------------------------------------------------------------------------------
  # Reactive trigger for generating the plot only when "Confirm" is clicked
  observeEvent(input$confirm, {
    #Requires data_frame
    req(structured_data())
    
    df <- structured_data()
    
    # Remove ID column if present
    if ("ID" %in% colnames(df)) {
      df <- df[, !(colnames(df) %in% "ID")]
    }
    
    # Keep only numeric columns
    numeric_cols <- sapply(df, is.numeric)
    df_numeric <- df[, numeric_cols, drop = FALSE]
    
    # Remove constant columns
    df_numeric <- df_numeric[, apply(df_numeric, 2, function(x) length(unique(x)) > 1), drop = FALSE]

    selected_features <- input$features  # Get selected features from the checkboxGroupInput
    df_numeric <- df_numeric[, colnames(df_numeric) %in% selected_features, drop = FALSE]
    
    # Ensure valid data
    if (ncol(df_numeric) < 2) {
      output$dynamicPlot <- renderUI({
        tags$p("Not enough numeric data for dimensionality reduction")
      })
      return()
    }
    
    # Perform dimensionality reduction
    result_df <- NULL
    if (input$chart == "PCA") {
      pca_result <- prcomp(df_numeric, center = TRUE, scale. = TRUE)
      result_df <- as.data.frame(pca_result$x[, 1:2])
    } else if (input$chart == "t-SNE") {
      df_numeric <- scale(df_numeric)
      tsne_result <- Rtsne(df_numeric, dims = 2, perplexity = 30, verbose = FALSE, max_iter = 500)
      result_df <- as.data.frame(tsne_result$Y)
    } else if (input$chart == "UMAP") {
      df_numeric <- scale(df_numeric)
      umap_result <- umap(df_numeric)
      result_df <- as.data.frame(umap_result$layout)
    }
    
    colnames(result_df) <- c("Dim1", "Dim2")
    result_df$Condition <- if ("Condition" %in% colnames(df)) df$Condition else "Unknown"
    
    # Dynamically render the plot output UI
    output$dynamicPlot <- renderUI({
      plotOutput("dimPlot", height = "100%", width = "100%")
    })
    
    # Render the plot
    output$dimPlot <- renderPlot({
      ggplot(result_df, aes(x = Dim1, y = Dim2, color = Condition)) +
        geom_point(size = 2, alpha = 0.7) +
        theme_minimal() +
        labs(title = paste(input$chart, "Projection"), x = "Dimension 1", y = "Dimension 2") +
        theme(legend.position = "right")
    }, height = function() session$clientData$output_dimPlot_width * 0.75)
  })
  
  #Outputs defined
#-------------------------------------------------------------------------------
  
  
  # 
  output$data_table <- renderDT({
    req(structured_data())
    datatable(structured_data(),options = list(pageLength= 10, autowidth = TRUE))
  })
  output$test <- renderText({
    req(test_text())
    paste("Value from test:", test_text())
  })
  
  output$nuclear_table <- renderDT({
    req(nuclear_data())
    datatable(nuclear_data(), options = list (pageLength = 10, autowidth = TRUE))
  })

  
  
  }

# Run the application
shinyApp(ui = ui, server = server)
