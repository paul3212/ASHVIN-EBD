library(shiny)

recommendationUI <- function(id){
  wellPanel(
    style = "background-color: white",
    tags$span(htmlOutput(NS(id, "dim"))), 
    tags$span(
      tags$div(sliderInput(
        NS(id, "pred_in"),
        "Predictor",
        min = 30,
        max = 150,
        value = 20,
        post = " m", 
        width = 500
      ), style = "display: inline-block;"), 
      tags$div(textOutput(NS(id, "pred_out")), style = "display: inline-block;")
  ))
}

recommendationServer <- function(id, bridge_typo){
  moduleServer(id, function(input, output, session){
    reco <- read_data("typology_to_dimensions.xlsx")
    reco <- filter(reco, bridge_type==bridge_typo)
    
    print(reco)
    print(reco[1,2])
    
    output$dim <- renderText(as.character(reco[1,2]))
    updateSliderInput(inputId = "pred_in", label = as.character(reco[1,3]))
    
    observeEvent(input$pred_in, {
      pred_in   <- input$pred_in
      intercept <- as.numeric(reco[1,4])
      gradient  <- as.numeric(reco[1,5])
      
      pred_out <- 1/gradient*(pred_in-intercept)
      
      output$pred_out <- renderText(pred_out)
    })



  })
}