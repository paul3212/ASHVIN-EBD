library(readxl)
library(shiny)
library(ggplot2)
library(tidyr)
library(htmltools)



read_data <- function(file_name){
  directory <- get_data_directory()
  path <- paste(directory, file_name, sep = "/")
  data <- read_xlsx(path)
  return(data)
}

get_data_directory <- function(){
  directory <- "../data_sbp"
  return(directory)
}

getTypologies <- function(){
    struc <- read_data("footbridge_database.xlsx")
    struc <- drop_na(struc, bridge_type)
    typologies <- unique((struc$bridge_type))
    typologies <- append(typologies, "no selection", after = 0)
    return(typologies)
}

formatValue <- function(number, unit){
  string <- paste(format(round(as.numeric(number), 0), nsmall=0, big.mark=","), unit)
  return(string)
}

formatCost <- function(number){
  string <- paste(format(round(as.numeric(number), 0), nsmall=0, big.mark=","), "€")
  return(string)
}

formatTime <- function(number){
  string <- paste(format(round(as.numeric(number), 0), nsmall=0, big.mark=","), "Days")
  return(string)
}

formatCarb <- function(number){
  string <- paste(format(round(as.numeric(number), 0), nsmall=0, big.mark=","), "kg Co2")
  return(string)
}

# getSafety <- function(nearest){
#   safety  <- read_data("fb_12_constr_safety.xlsx")
#   data    <- left_join(nearest, safety, by="proj_nr")
# }

# Projects wrappers
# UI function to display single projects
projectUI <- function(id){
  tagList(
    wellPanel(
    htmlOutput(NS(id, "ranking")),
    textOutput(NS(id, "no")),
    textOutput(NS(id, "cost")),
    textOutput(NS(id, "time")),
    textOutput(NS(id, "carb")),
    imageOutput(NS(id, "image"), width = "auto", height = "200px"),
    htmlOutput(NS(id, "safety")),
    style = "padding: 5px;"
    )
  )
}

# Server function to display single projects
projectServer <- function(id, rank, no, cost, time, carb, safety){
  moduleServer(id, function(input, output, session){
    if (!is.na(no)) {
      output$ranking <- renderText(
        HTML(paste("<h2 style = \"margin-top: 0px;\">", rank, ". nearest</h2>", sep=''))
        )
      output$no <- renderText(no)
      output$cost <- renderText(paste("Cost: ", formatCost(cost)))
      output$time <- renderText(paste("Construction Time: ", formatTime(time)))
      output$carb <- renderText(paste("Embodied Carbon: ", formatCarb(carb)))
      
      output$image <- renderImage({
        path <- paste(get_data_directory(), "pictures", sep = "/")
        filename <- normalizePath(file.path(path, paste(no,'.png', sep='')))
        list(src = filename, width = "200")
      }, deleteFile = FALSE)
      
      if (safety$accidents_fatal>0) {
        string <- paste("<p style=\"font-size:20px;color:red;\">", "Safety incidents: ",safety$cause_short, "</p>")
        output$safety <- renderText(
          HTML(string))
      } else {
        output$safety <- renderText(paste("Safety incidents: ", "None"))
      }
    } else {
      output$ranking <- renderText(
      HTML("<h2 style = \"margin-top: 0px;\">Not enough</h2>
            <h2 style = \"margin-top: 0px;\">projects</h2>")
      )
      output$no <- renderText("")
      output$cost <- renderText("")
      output$time <- renderText("")
      output$carb <- renderText("")
      output$image <- NULL
      output$safety <- renderText("")
    }
  })
}

#Predictions Wrappers
# UI function to display single prediction
predictionUI <- function(id){
  tagList(
    wellPanel(
      style = "background-color: white",
      tags$span(plotOutput(NS(id, "plot"), width = "800%", height = "100px"), style = "display: inline-block;"),
      tags$span(
        tags$div(textOutput(NS(id, "pred_per_area"))), 
        tags$div(textOutput(NS(id, "pred"))), style = "display: inline-block;"
      )
    )
  )
}

predictionServer <- function(id, nearest, pred, length, width, min, max, unit, name){
  moduleServer(id, function(input, output, session){
  output$plot <- renderPlot({
    options(scipen=10000)
    ggplot() +
      geom_point(aes(x = nearest$pred, y = "Dummy", size=1-nearest$dist_norm), show.legend = FALSE) +
      geom_point(aes(x = pred, y = "Dummy", colour = "RED", size=1), show.legend = FALSE) +
      labs(x = paste(unit, "/m²", sep = "")) + 
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ","), limits = c(min, max))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  }, height = 100, width = 600 )
  
  output$pred_per_area <- renderText(paste("Predicted ", name, " per area: ", formatValue(pred, unit), "/m²", sep = ""))
  output$pred          <- renderText(paste("Predicted total ", name, ": ",formatValue(pred*length*width, unit), sep = ""))
  })
}

#Prefabrication safety
getPrefabSafety <- function(prefab_possible, bridge_type){
  if (bridge_type =="no selection") {
    return("No warnings concerning Prefabrication. First choose a bridge type")
  }
  safety <- read_data("typology_to_safety.xlsx")
  text <- "No warnings concerning Prefabrication"
  
  
  
  
  if (any(is.null(prefab_possible))) {
    benefits <- safety[safety$bridge_type==bridge_type,2]
    if (benefits =="TRUE") {
      text <- paste("<p style=\"color:red;\">", "A bridge of type ", bridge_type, " would benefit from prefabrication, which is not possible on site. Maybe consider a different Bridge typology.", "</p>")
    }
  }
  return(text)
}

#Safety of the similar projects
getProjectSafety <- function(safety){
  safety <- drop_na(safety, accidents_fatal)
  
  fatal <- sum(safety$accidents_fatal)
  non_fatal <- sum(safety$accidents_non_fatal)
  
  text <- "No accidents were recorded in the similar projects."
  
  if (fatal>0 | non_fatal>0) {
    text <- paste("In the similar bridge projects ", fatal, " fatal and ", non_fatal, " non fatal accidents were recorded. See the specific projects below for details.")
  }
  
  return(text)
}



