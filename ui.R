library(shiny)
library(ggplot2)

shinyUI(
  fluidPage(
    titlePanel("NGO somalia type"),
    tags$style(HTML(
      "
      /* Custom CSS styles */
      body {
        font-family: 'Arial', sans-serif;
        #background-color: #f0f0f0;
        color: #333;
        margin: 0;
        padding: 0;
        background-color: yellow;
        background-size: cover;
      }
      .container {
        max-width: 1200px;
        margin: 0 auto;
        background-color: rgba(255, 255, 255, 0.8);
        padding: 20px;
        border: 1px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      .panel {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
      }
      select {
        width: 100%;
        padding: 10px;
        border: 1px solid #ccc;
        border-radius: 5px;
        background-color: #fff;
      }
      #train_model {
        background-color: #007BFF;
        color: #fff;
        border: none;
        border-radius: 5px;
        padding: 10px 20px;
        cursor: pointer;
      }
      #train_model:hover {
        background-color: #0056b3;
      }
      .tab-content {
        padding: 15px;
      }
      .navbar {
        background-color: cyan;
        color: #fff;
        font-size: 18px;
        padding: 10px 0;
      }
      .navbar a {
        color: #fff;
        text-decoration: none;
        margin: 0 15px;
      }
      .data-upload-panel {
        background-color: rgba(255, 255, 255, 0.9);
        border: 1px solid #ccc;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        text-align: center;
        margin: 0 auto;
      }
      "
    )),
    navbarPage(
      "NGO type",
      tabPanel("Data Upload",
               sidebarLayout(
                 sidebarPanel(
                   div(class = "data-upload-panel",
                       fileInput("data_file", "Upload CSV Data"),
                       selectInput("predictor1", "Select Predictor 1", choices = c("Region_name", "District", "HR_info_district_name")),
                       selectInput("predictor2", "Select Predictor 2", choices = c("Region_name", "District", "HR_info_district_name")),
                       selectInput("predictor3", "Select Predictor 3", choices = c("Region_name", "District", "HR_info_district_name")),
                       selectInput("target", "Select Target Variable", choices = c("REGN_NO")),
                       actionButton("train_model", "Train Model")
                   )
                 ),
                 mainPanel(
                   h1("References: "),
                   tags$a(href = "https://shiny.posit.co/r/getstarted/build-an-app/hello-shiny/user-interface.html", "Shiny documenation")
                 )
               )
      ),
      tabPanel("Visualizations",
               plotOutput("boxplot"),
               plotOutput("scatterplot")
      ),
      tabPanel("Model Performance",
               verbatimTextOutput("mae"),
               verbatimTextOutput("rmse")
      )
    )
  )
)
