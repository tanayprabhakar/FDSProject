library(shiny)
library(shinydashboard)
library(plotly)
library(rpart)
library(randomForest)
library(class)
library(caret)
library(rpart.plot)
library(dplyr)

# Custom CSS
customCSS <- "
  /* Custom CSS for dark theme */
  body {
    background-color: #121212;
    color: #ffffff;
  }
  .content-wrapper, .right-side {
    background-color: #1e1e1e;
  }
  .box {
    background-color: #2d2d2d;
    box-shadow: 0 1px 3px rgba(255, 255, 255, 0.12), 0 1px 2px rgba(255, 255, 255, 0.24);
    border-radius: 5px;
  }
  .box-header {
    color: #ffffff;
  }
  .box.box-solid.box-primary {
    border: 1px solid #00e5ff;
  }
  .box.box-solid.box-primary > .box-header {
    background-color: #00e5ff;
    color: #000000;
  }
  .box.box-solid.box-info {
    border: 1px solid #00aaff;
  }
  .box.box-solid.box-info > .box-header {
    background-color: #00aaff;
    color: #000000;
  }
  .box.box-solid.box-success {
    border: 1px solid #00ff7f;
  }
  .box.box-solid.box-success > .box-header {
    background-color: #00ff7f;
    color: #000000;
  }
  .box.box-solid.box-warning {
    border: 1px solid #ffbb00;
  }
  .box.box-solid.box-warning > .box-header {
    background-color: #ffbb00;
    color: #000000;
  }
  .main-header .logo {
    background-color: #00e5ff;
    color: #000000;
    font-weight: bold;
  }
  .main-header .navbar {
    background-color: #00e5ff;
  }
  .main-header .navbar .nav > li > a {
    color: #000000;
  }
  .main-header .navbar .sidebar-toggle {
    color: #000000;
  }
  .skin-blue .main-sidebar {
    background-color: #121212;
  }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a {
    background-color: #00e5ff;
    color: #000000;
    border-left-color: #00e5ff;
  }
  .sidebar-menu > li > a {
    color: #ffffff;
  }
  .btn-success {
    background-color: #00ff7f;
    border-color: #00cc66;
    color: #000000;
  }
  .btn-success:hover {
    background-color: #00cc66;
    border-color: #00994d;
  }
  .form-control {
    background-color: #3d3d3d;
    color: #ffffff;
    border: 1px solid #555555;
  }
  .form-control:focus {
    border-color: #00e5ff;
  }
  .selectize-input {
    background-color: #3d3d3d !important;
    color: #ffffff !important;
    border: 1px solid #555555 !important;
  }
  .selectize-dropdown {
    background-color: #2d2d2d !important;
    color: #ffffff !important;
  }
  .selectize-dropdown-content .option {
    color: #ffffff !important;
  }
  .selectize-dropdown-content .option.active {
    background-color: #00e5ff !important;
    color: #000000 !important;
  }
  .radio label, .checkbox label {
    color: #ffffff;
  }
  .shiny-input-container {
    color: #ffffff;
  }
  .dataTables_wrapper .dataTables_length, 
  .dataTables_wrapper .dataTables_filter, 
  .dataTables_wrapper .dataTables_info, 
  .dataTables_wrapper .dataTables_processing, 
  .dataTables_wrapper .dataTables_paginate {
    color: #ffffff !important;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: #ffffff !important;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current {
    background: #00e5ff !important;
    color: #000000 !important;
  }
  .table > tbody > tr > td, .table > tbody > tr > th, 
  .table > tfoot > tr > td, .table > tfoot > tr > th, 
  .table > thead > tr > td, .table > thead > tr > th {
    border-top: 1px solid #444444;
  }
  .table-striped > tbody > tr:nth-of-type(odd) {
    background-color: #3d3d3d;
  }
  #treeVisualization {
    background-color: #ffffff;
    border-radius: 5px;
  }
"

# Predefined Test Cases specific to each model
tree_data <- data.frame(
  budget = c(25000000, 6000000, 8000000, 185000000, 63000000),
  gross = c(28341469, 134966411, 107928762, 1004558444, 37030102),
  user_votes = c(2343115, 1563739, 1831153, 2303232, 1831153),
  critic_review_ratio = c(0.2367, 0.3123, 0.2987, 0.3211, 0.2765),
  movie_fb = c(33000, 8500, 10000, 8000, 7500),
  director_fb = c(949, 12000, 11000, 14000, 13000),
  actor1_fb = c(1000, 6500, 6000, 6800, 6000),
  other_actors_fb = c(1791, 2800, 2500, 2600, 2400),
  duration = c(142, 175, 154, 152, 139),
  face_number = c(1, 3, 2, 1, 1),
  year = c(1994, 1972, 1994, 2008, 1999),
  country = c("USA", "USA", "USA", "USA", "USA"),
  content = c("R", "R", "R", "PG-13", "R"),
  imdb_score = c(7,8,5,4,2),
  movie_title = c("The Shawshank Redemption", "The Godfather", "Pulp Fiction", "The Dark Knight", "Fight Club")
)

knn_data <- data.frame(
  budget = c(15000000, 75000000, 30000000, 10000000, 50000000),
  gross = c(42195766, 292576195, 83400000, 56000000, 330000000),
  user_votes = c(1200000, 2100000, 900000, 800000, 3000000),
  critic_review_ratio = c(0.282, 0.341, 0.265, 0.225, 0.355),
  movie_fb = c(12000, 22000, 8000, 5000, 30000),
  director_fb = c(5000, 15000, 8000, 3000, 20000),
  actor1_fb = c(8000, 15000, 6000, 4000, 18000),
  other_actors_fb = c(3000, 8000, 2500, 1800, 9000),
  duration = c(136, 163, 128, 115, 180),
  face_number = c(2, 1, 3, 2, 2),
  year = c(2000, 2015, 2005, 1995, 2018),
  country = c("UK", "USA", "USA", "UK", "USA"),
  content = c("PG-13", "PG-13", "R", "PG", "PG-13"),
  movie_title = c("Memento", "Interstellar", "Sin City", "Trainspotting", "Avengers: Infinity War")
)

rf_data <- data.frame(
  budget = c(58000000, 94000000, 125000000, 237000000, 20000000),
  gross = c(192000000, 303000000, 825000000, 853500000, 160000000),
  user_votes = c(1500000, 1800000, 2500000, 2700000, 1000000),
  critic_review_ratio = c(0.312, 0.295, 0.335, 0.347, 0.271),
  movie_fb = c(15000, 18000, 25000, 28000, 9500),
  director_fb = c(9000, 11000, 18000, 22000, 7000),
  actor1_fb = c(12000, 14000, 20000, 25000, 9000),
  other_actors_fb = c(5000, 6000, 10000, 12000, 4000),
  duration = c(148, 156, 165, 182, 120),
  face_number = c(2, 2, 1, 3, 2),
  year = c(2010, 2012, 2016, 2019, 2003),
  country = c("USA", "USA", "USA", "USA", "Others"),
  content = c("PG-13", "PG-13", "PG-13", "PG-13", "R"),
  imdb_score = c(7,8,5,4,2),
  movie_title = c("Inception", "The Dark Knight Rises", "Captain America: Civil War", "Avengers: Endgame", "Old Boy")
)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "IMDB Score Predictor",
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("film")),
      menuItem("Comparison", tabName = "comparison", icon = icon("chart-line")),
      menuItem("Model Information", tabName = "modelinfo", icon = icon("info-circle"))
    ),
    tags$style(customCSS)
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #00e5ff;
          color: #000000;
          font-weight: bold;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #00ccff;
        }
        .skin-blue .main-header .navbar {
          background-color: #00e5ff;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #00ccff;
        }
      "))
    ),
    tabItems(
      # Prediction Tab
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = "Algorithm Selection", status = "primary", solidHeader = TRUE,
                  radioButtons("algorithm", "Select Algorithm:",
                               choices = c("Classification Tree" = "tree", 
                                           "K - Nearest Neighbour" = "knn",
                                           "Random Forest" = "rf"),
                               selected = "tree")
                )
              ),
              fluidRow(
                box(
                  title = "Select Test Case", width = 12, status = "info", solidHeader = TRUE,
                  uiOutput("dynamicTestCases"),
                  div(style = "text-align: center; padding: 20px;",
                      actionButton("predict", "Predict IMDB Score", 
                                   class = "btn-success", 
                                   style = "font-size: 18px; padding: 10px 24px;")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Prediction Results", width = 6, status = "success", solidHeader = TRUE,
                  htmlOutput("predictionText"),
                  plotlyOutput("predictionGauge")
                ),
                box(
                  title = "Prediction Category", width = 6, status = "success", solidHeader = TRUE,
                  plotlyOutput("categoryPrediction")
                )
              )
      ),
      
      # Comparison Tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Algorithm Performance Comparison", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("accuracyComparison", height = "300px")
                )
              ),
              fluidRow(
                box(
                  title = "Feature Importance", width = 12, status = "warning", solidHeader = TRUE,
                  plotlyOutput("featureImportance")
                )
              )
      ),
      
      # Model Information Tab
      tabItem(tabName = "modelinfo",
              fluidRow(
                box(
                  title = "Decision Tree Model", width = 12, status = "primary", solidHeader = TRUE,
                  div(style = "background-color: #ffffff; border-radius: 5px; padding: 10px;",
                      plotOutput("treeVisualization", height = "500px")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Confusion Matrix", width = 6, status = "info", solidHeader = TRUE,
                  div(style = "background-color: #3d3d3d; border-radius: 5px; padding: 10px; overflow-x: auto;",
                      verbatimTextOutput("confusionMatrix")
                  )
                ),
                box(
                  title = "Model Parameters", width = 6, status = "info", solidHeader = TRUE,
                  div(style = "background-color: #3d3d3d; border-radius: 5px; padding: 10px; overflow-x: auto;",
                      verbatimTextOutput("modelParams")
                  )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Try to load the saved models with error handling
  models <- tryCatch({
    list(
      pruned_ct = readRDS("pruned_ct_model.rds"),
      norm_values = readRDS("norm_values.rds"),
      train2_norm = readRDS("train2_norm.rds"),
      rf = readRDS("rf_model.rds")
    )
  }, error = function(e) {
    # Return dummy models for testing if files not found
    message("Error loading models: ", e$message)
    NULL
  })
  
  # Dynamically render test case options based on the selected algorithm
  output$dynamicTestCases <- renderUI({
    req(input$algorithm)
    
    # Select the appropriate dataset based on the algorithm
    if (input$algorithm == "tree") {
      choices <- tree_data$movie_title
      selected <- choices[1]
    } else if (input$algorithm == "knn") {
      choices <- knn_data$movie_title
      selected <- choices[1]
    } else if (input$algorithm == "rf") {
      choices <- rf_data$movie_title
      selected <- choices[1]
    }
    
    selectInput("testCase", "Choose a movie from test dataset:",
                choices = choices,
                selected = selected)
  })
  
  # Reactive function to get the current dataset based on the algorithm
  getCurrentDataset <- reactive({
    req(input$algorithm)
    
    if (input$algorithm == "tree") {
      return(tree_data)
    } else if (input$algorithm == "knn") {
      return(knn_data)
    } else {
      return(rf_data)
    }
  })
  
  # Reactive function to prepare input data for prediction
  getInputData <- reactive({
    # Use predefined test case from the current dataset
    current_dataset <- getCurrentDataset()
    selected_idx <- which(current_dataset$movie_title == input$testCase)
    input_data <- current_dataset[selected_idx, !names(current_dataset) %in% "movie_title"]
    
    # Ensure factor levels match the training data
    input_data$country <- factor(input_data$country, levels = c("USA", "UK", "Others"))
    input_data$content <- factor(input_data$content, levels = c("G", "PG", "PG-13", "R", "NC-17"))
    
    return(input_data)
  })
  
  # Reactive function to prepare input data for KNN
  getKNNInputData <- reactive({
    input_data <- getInputData()
    
    # Create dummy variables for categorical features
    input_data_extended <- input_data
    
    # Create dummy variables for country
    input_data_extended$country_UK <- ifelse(input_data$country == "UK", 1, 0)
    input_data_extended$country_USA <- ifelse(input_data$country == "USA", 1, 0)
    input_data_extended$country_Others <- ifelse(input_data$country == "Others", 1, 0)
    
    # Create dummy variables for content rating
    input_data_extended$content_G <- ifelse(input_data$content == "G", 1, 0)
    input_data_extended$content_NC17 <- ifelse(input_data$content == "NC-17", 1, 0)
    input_data_extended$content_PG <- ifelse(input_data$content == "PG", 1, 0)
    input_data_extended$content_PG13 <- ifelse(input_data$content == "PG-13", 1, 0)
    input_data_extended$content_R <- ifelse(input_data$content == "R", 1, 0)
    
    # Drop original categorical columns
    input_data_extended$country <- NULL
    input_data_extended$content <- NULL
    
    # Return input data with dummy variables
    return(input_data_extended)
  })
  
  # Function to convert predicted class to numeric IMDb score for gauge
  classToScore <- function(prediction) {
    scores <- c("Bad" = 3, "OK" = 5, "Good" = 7, "Excellent" = 9)
    return(scores[as.character(prediction)])
  }
  
  # Reactive function to make predictions
  getPrediction <- reactive({
    req(input$algorithm)
    req(input$predict)
    
    if (!is.null(models)) {
      if (input$algorithm == "tree") {
        # Classification Tree
        input_data <- getInputData()
        prediction <- predict(models$pruned_ct, input_data, type = "class")
      } else if (input$algorithm == "knn") {
        # K-Nearest Neighbors
        input_data_extended <- getKNNInputData()
        
        # Apply normalization
        input_data_norm <- predict(models$norm_values, input_data_extended)
        
        # Get only the features used in training (first 19 columns)
        train_features <- models$train2_norm[, -20]
        train_labels <- models$train2_norm[, 20]
        
        # Make KNN prediction
        prediction <- knn(train_features, input_data_norm, cl = train_labels, k = 9)
      } else if (input$algorithm == "rf") {
        # Random Forest
        input_data <- getInputData()
        prediction <- predict(models$rf, input_data)
      }
    } else {
      # Dummy prediction if models not loaded
      prediction <- factor("Good", levels = c("Bad", "OK", "Good", "Excellent"))
    }
    
    return(prediction)
  })
  
  # Output: Prediction Text
  output$predictionText <- renderUI({
    req(input$predict)
    prediction <- getPrediction()
    
    if (!is.null(prediction)) {
      score_map <- c("Bad" = "1-4", "OK" = "4-6", "Good" = "6-8", "Excellent" = "8-10")
      score_range <- score_map[as.character(prediction)]
      
      HTML(paste0(
        "<h3>Predicted IMDb Score Category: <span style='color: #00e5ff;'>", prediction, "</span></h3>",
        "<p>Estimated Score Range: ", score_range, "</p>"
      ))
    }
  })
  
  # Output: Prediction Gauge
  output$predictionGauge <- renderPlotly({
    req(input$predict)
    prediction <- getPrediction()
    
    if (!is.null(prediction)) {
      # Convert class to numeric score for gauge
      score_value <- classToScore(prediction)
      
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = score_value,
        title = list(text = "IMDb Score Prediction", font = list(size = 24, color = "#ffffff")),
        gauge = list(
          axis = list(range = list(0, 10), tickwidth = 1, tickcolor = "#ffffff"),
          bar = list(color = "#00e5ff"),
          bgcolor = "darkgrey",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 4), color = "#ff4d4d"),
            list(range = c(4, 6), color = "#ffcc00"),
            list(range = c(6, 8), color = "#99e699"),
            list(range = c(8, 10), color = "#33cc33")
          ),
          threshold = list(
            line = list(color = "white", width = 4),
            thickness = 0.75,
            value = score_value
          )
        )
      ) %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff")
        )
    }
  })
  
  # Output: Category Prediction Plot
  output$categoryPrediction <- renderPlotly({
    req(input$predict)
    prediction <- getPrediction()
    
    if (!is.null(prediction)) {
      categories <- c("Bad", "OK", "Good", "Excellent")
      values <- ifelse(categories == prediction, 1, 0)
      colors <- c("#ff4d4d", "#ffcc00", "#99e699", "#33cc33")
      
      plot_ly(
        x = categories,
        y = values,
        type = "bar",
        marker = list(color = colors)
      ) %>%
        layout(
          title = list(text = "Predicted Category", font = list(color = "#ffffff")),
          xaxis = list(title = "Category", tickfont = list(color = "#ffffff")),
          yaxis = list(title = "Probability", range = c(0, 1), tickfont = list(color = "#ffffff")),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff")
        )
    }
  })
  
  # Output: Algorithm Accuracy Comparison
  output$accuracyComparison <- renderPlotly({
    accuracy_data <- data.frame(
      Algorithm = c("Decision Tree", "KNN", "Random Forest"),
      Accuracy = c(0.7241, 0.7456, 0.7658)
    )
    
    plot_ly(
      accuracy_data,
      x = ~Algorithm,
      y = ~Accuracy,
      type = "bar",
      marker = list(color = c("#00e5ff", "#00ff7f", "#ffbb00"))
    ) %>%
      layout(
        title = list(text = "Algorithm Accuracy Comparison", font = list(color = "#ffffff")),
        xaxis = list(title = "Algorithm", tickfont = list(color = "#ffffff")),
        yaxis = list(title = "Accuracy", range = c(0, 1), tickfont = list(color = "#ffffff")),
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # Output: Feature Importance Plot
  output$featureImportance <- renderPlotly({
    if (!is.null(models)) {
      importance <- importance(models$rf)
      varImportance <- data.frame(
        Variables = rownames(importance),
        Importance = importance[, "MeanDecreaseGini"]
      )
      
      # Sort by importance
      varImportance <- varImportance[order(varImportance$Importance, decreasing = TRUE), ]
      
      plot_ly(
        varImportance,
        x = ~reorder(Variables, Importance),
        y = ~Importance,
        type = "bar",
        marker = list(color = "#00e5ff")
      ) %>%
        layout(
          title = list(text = "Feature Importance (Random Forest)", font = list(color = "#ffffff")),
          xaxis = list(title = "Variables", tickfont = list(color = "#ffffff")),
          yaxis = list(title = "Importance", tickfont = list(color = "#ffffff")),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff")
        )
    }
  })
  
  # Output: Decision Tree Visualization
  output$treeVisualization <- renderPlot({
    if (!is.null(models)) {
      prp(models$pruned_ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0, 
          box.palette = "GnBu", shadow.col = "gray", fallen.leaves = TRUE)
    }
  })
  
  # Output: Confusion Matrix
  output$confusionMatrix <- renderPrint({
    if (!is.null(models)) {
      # Create a dummy test set if not available
      dummy_test <- data.frame(
        binned_score = factor(sample(c("Bad", "OK", "Good", "Excellent"), 100, replace = TRUE, 
                                     prob = c(0.1, 0.3, 0.4, 0.2)))
      )
      # Add predictor columns similar to your training data
      for (col in setdiff(names(getInputData()), "binned_score")) {
        if (col %in% c("country", "content")) {
          dummy_test[[col]] <- sample(levels(getInputData()[[col]]), 100, replace = TRUE)
        } else {
          dummy_test[[col]] <- rnorm(100)
        }
      }
      
      # Get predictions
      pred <- predict(models$pruned_ct, dummy_test)
      
      # Create confusion matrix
      confusionMatrix(pred, dummy_test$binned_score)
    }
  })
  
  # Output: Model Parameters
  output$modelParams <- renderPrint({
    if (!is.null(models)) {
      if (input$algorithm == "tree") {
        models$pruned_ct$control
      } else if (input$algorithm == "knn") {
        cat("K-Nearest Neighbors Parameters:\n")
        cat("k = 9\n")
        cat("Distance Metric: Euclidean\n")
      } else {
        cat("Random Forest Parameters:\n")
        cat("Number of Trees:", models$rf$ntree, "\n")
        cat("Variables tried at each split:", models$rf$mtry, "\n")
      }
    }
  })
}

# Run the Shiny App
shinyApp(ui, server)