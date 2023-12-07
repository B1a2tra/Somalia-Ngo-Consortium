library(shiny)
library(caret)
library(randomForest)
library(ggplot2)

shinyServer(function(input, output, session) {
  data <- reactive({
    req(input$data_file)
    data <- read.csv(input$data_file$datapath)
    data
  })
  
  model <- eventReactive(input$train_model, {
    set.seed(42)
    train_indices <- sample(1:nrow(data()), 0.7 * nrow(data()))
    train_data <- data()[train_indices, ]
    test_data <- data()[-train_indices, ]
    
    predictors <- c(input$predictor1, input$predictor2, input$predictor3)
    target <- input$target
    
    predictor_data <- train_data[, predictors]
    target_data <- train_data[, target]
    
    model <- randomForest(x = predictor_data, y = target_data, ntree = 500)
    model
  })
  
  predictions <- eventReactive(model(), {
    predict(model(), newdata = test_data)
  })
  
  mae <- eventReactive(predictions(), {
    mean(abs(predictions() - test_data[, target]))
  })
  
  rmse <- eventReactive(predictions(), {
    sqrt(mean((predictions() - test_data[, target])^2))
  })
  
  observeEvent(input$train_model, {
    updateTabsetPanel(session, "tabs", "Visualizations")
  })
  
  output$boxplot <- renderPlot({
    data_subset <- data()  # Assuming you want to use the entire dataset
    
    # Convert the selected predictor to numeric
    data_subset$numeric_predictor <- as.numeric(as.character(data_subset[, input$predictor1]))
    
    # Create a factor variable based on the numeric predictor
    data_subset$cut_var <- cut(data_subset$numeric_predictor, breaks = seq(0, 45, by = 5), right = FALSE)
    
    # Create the boxplot
    ggplot(data_subset, aes(x = cut_var, y = data_subset[, input$target])) +
      geom_boxplot(fill = "green") +
      labs(title = "NGO somalia type", x = "REGN_NO", y = "District")
  })
  
  
  
  
  output$scatterplot <- renderPlot({
    ggplot(data(), aes(x = data()[, input$predictor1], y = data()[, input$target])) +
      geom_point(color = "blue") +
      labs(title = "NGO somalia type", x = "region_name", y = "District")
  })
  
  output$mae <- renderText({
    paste("Mean Absolute Error:", mae())
  })
  
  output$rmse <- renderText({
    paste("Root Mean Squared Error:", rmse())
  })
})

