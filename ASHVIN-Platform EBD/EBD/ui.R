library(shiny)
library(htmltools)

# source("typology.R")
source("util.R")
source("recommendations.R")

shinyUI(fluidPage(
  
  titlePanel(title = span(img(src = "ebd_logo.png", height = 75), "Evidence Based Design Assistant")),
  
  # Sidebar with a slider input for bridge length
  sidebarLayout(
    sidebarPanel(
      h2("Parameter Input"),
      # textInput("id", "Project ID", value = 1),
      sliderInput(
        "length",
        "bridge length:",
        min = 20,
        max = 200,
        value = 30,
        post = " m"
      ),
      sliderInput(
        "width",
        "bridge width:",
        min = 2,
        max = 10,
        value = 4,
        post = " m"
      ),
      checkboxGroupInput(
        "prefab",
        label = "Prefabrication",
        choices = list(
          "prefabricaton space available" = 1,
          "ability to deliver big parts" = 2
        )
      ),
      selectInput("type",
                  "bridge typology",
                  choices = getTypologies()),
      # plotOutput("model", width = "400px", height = "100px"),
      plotOutput("landscape", width = "400px", height = "400px"),
      htmlOutput("predSource")
      # actionButton(inputId = "export", label = "Export")
      
    ),
    
    # Show a plot of the kpi forecast
    mainPanel(tabsetPanel(
      tabPanel(
        title = "Predictions",
        fluidPage(
          h2("Predictions"),
          h3("Construction Cost"),
          predictionUI("predUI_cost"),
          h3("Construction Time"),
          predictionUI("predUI_time"),
          h3("Carbon Footprint"),
          predictionUI("predUI_carb")
        )
      ),
      tabPanel(
        title = "Warnings",
        fluidPage(
          h2("Warnings"),
          htmlOutput("Warnings1"),
          htmlOutput("Warnings2"),
          h2("Selected similar projects"),
          splitLayout(
            projectUI("no1"),
            projectUI("no2"),
            projectUI("no3"),
            projectUI("no4"),
            projectUI("no5"),
            cellWidths = "20%",
            cellArgs = list(style = "padding: 5px")
          )
        )
      ), 
      tabPanel(
        title = "Recommendations",
        fluidPage(
          h2("Dimension Recommendations"),
          recommendationUI("rec1")
        )
      )
      
    ))
  )
))
