library(shiny)
library(bslib)

default_theme <- bs_theme(version = 4, bootswatch = "flatly")

shinyUI(
  fluidPage(
    theme = default_theme,
    
    tags$head(
      tags$style(HTML("
        .header-info {
          background-color: #f8f9fa;
          padding: 20px;
          text-align: center;
          font-size: 20px;
          font-weight: bold;
          color: #2c3e50;
          border-bottom: 2px solid #dee2e6;
        }
        .highlight {
          background-color: #ffcccc;
          font-weight: bold;
        }
      "))
    ),
    
    div(class = "header-info",
        HTML("Nour Ahmed Elbanna &nbsp; | &nbsp; 2305395 &nbsp; | &nbsp; Data Science Tools &nbsp; | &nbsp; CS AI and Data Science")
    ),
    
    titlePanel("Word Prediction App with Autocorrect"),
    
    sidebarLayout(
      sidebarPanel(
        textInput("sentence", "Enter your text:", ""),
        
        selectInput("theme", "Choose Theme:",
                    choices = c("Flatly", "Cosmo", "Darkly", "Sandstone"),
                    selected = "Flatly"),
        
        checkboxInput("threeOutputs", "Show 3 Predictions", value = TRUE),
        
        sliderInput("predictionSpeed", "Prediction Refresh Rate (ms)",
                    min = 500, max = 5000, value = 1000, step = 500),
        
        actionButton("clearBtn", "Clear Text"),
        actionButton("undoBtn", "Undo"),
        
        br(), br(),
        verbatimTextOutput("predictionHistory"),
        br(),
        uiOutput("correctionBtnUI")
      ),
      
      mainPanel(
        uiOutput("wordButtons")
      )
    )
  )
)
