
library(shiny)
library(randomForest)

# Load the saved model bundle
bundle_path <- "road_accident_model_bundle.rds"

if (!file.exists(bundle_path)) {
  stop("Model bundle not found. Knit Road_Accident_Severity_fixed.Rmd first.")
}

bundle <- readRDS(bundle_path)

model <- bundle$model
train_data <- bundle$train_data
selected_columns <- bundle$selected_columns

# Get choices from the training dataset
get_choices <- function(column) {
  x <- train_data[[column]]
  
  if (is.factor(x)) {
    levels(x)
  } else {
    sort(unique(as.character(x)))
  }
}

# User interface
ui <- fluidPage(
  
  titlePanel("Road Accident Severity Prediction"),
  
  p(
    "Enter the driver and road conditions to estimate accident severity."
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "age",
        "Driver age group",
        choices = get_choices("Age_band_of_driver")
      ),
      
      selectInput(
        "experience",
        "Driving experience",
        choices = get_choices("Driving_experience")
      ),
      
      selectInput(
        "weather",
        "Weather conditions",
        choices = get_choices("Weather_conditions")
      ),
      
      selectInput(
        "surface",
        "Road surface conditions",
        choices = get_choices("Road_surface_conditions")
      ),
      
      selectInput(
        "light",
        "Lighting conditions",
        choices = get_choices("Light_conditions")
      ),
      
      actionButton("predict", "Predict Severity")
    ),
    
    mainPanel(
      
      h3("Prediction Result"),
      verbatimTextOutput("prediction"),
      
      h4("Estimated Class Probabilities"),
      verbatimTextOutput("probabilities"),
      
      hr(),
      
      h3("High-Risk Conditions Alert"),
      verbatimTextOutput("risk_alert"),
      
      p(
        style = "font-size: 12px; color: gray;",
        "Note: The alert is based on demonstration rules, not a
        separately trained fatal-injury prediction model. No alert
        does not mean the conditions are safe."
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Predict only when the button is clicked
  prediction_result <- eventReactive(input$predict, {
    
    # Create input data
    new_data <- data.frame(
      Age_band_of_driver = input$age,
      Driving_experience = input$experience,
      Weather_conditions = input$weather,
      Road_surface_conditions = input$surface,
      Light_conditions = input$light
    )
    
    # Match factor levels used during model training
    for (column in selected_columns) {
      
      if (column %in% names(new_data)) {
        
        new_data[[column]] <- factor(
          new_data[[column]],
          levels = levels(train_data[[column]])
        )
      }
    }
    
    # Keep predictors in the required order
    new_data <- new_data[, selected_columns, drop = FALSE]
    
    # Predict accident severity
    predicted_class <- predict(
      model,
      newdata = new_data
    )
    
    # Get estimated class probabilities
    probabilities <- predict(
      model,
      newdata = new_data,
      type = "prob"
    )
    
    list(
      class = as.character(predicted_class),
      probabilities = probabilities
    )
  })
  
  # Display the model prediction
  output$prediction <- renderText({
    
    req(input$predict > 0)
    
    paste(
      "Predicted accident severity:",
      prediction_result()$class
    )
  })
  
  # Display probabilities for all model classes
  
  output$probabilities <- renderText({
    
    req(input$predict > 0)
    
    probs <- prediction_result()$probabilities
    probability_values <- as.numeric(probs[1, ]) * 100
    
    # Keep only classes with probabilities greater than zero
    keep <- probability_values > 0
    
    if (!any(keep)) {
      return("No positive class probabilities available.")
    }
    
    paste(
      paste0(
        colnames(probs)[keep],
        ": ",
        sprintf("%.2f%%", probability_values[keep])
      ),
      collapse = "\n"
    )
  })
  
  
  # Additional rule-based warning
  output$risk_alert <- renderText({
    
    req(input$predict > 0)
    
    # Check for poor lighting
    poor_lighting <- input$light %in% c(
      "Darkness - no lighting",
      "Darkness - lights unlit"
    )
    
    # Check for adverse weather
    bad_weather <- input$weather %in% c(
      "Raining",
      "Raining and Windy",
      "Fog or mist"
    )
    
    # Check for wet or flooded roads
    wet_road <- input$surface %in% c(
      "Wet or damp",
      "Flood over 3cm. deep"
    )
    
    # Generate alert
    if (poor_lighting && (bad_weather || wet_road)) {
      
      paste(
        "HIGH-RISK CONDITIONS ALERT",
        "Poor lighting is combined with adverse weather",
        "or wet road conditions.",
        "Exercise caution and consider delaying travel",
        "if conditions are unsafe.",
        "This rule does not predict that a fatal accident will occur.",
        sep = "\n"
      )
      
    } else {
      
      paste(
        "No alert triggered by the demonstration rule.",
        "This does not mean the conditions are safe.",
        sep = "\n"
      )
    }
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
