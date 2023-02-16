# EBD Tool

library(shiny)
library(ggplot2)
library(ggalt)
library(plotrix)
library(scales)
library(writexl)

# source("typology.R") 
source("knn.R")
source("util.R")



shinyServer(function(input, output) {
  
    
    toListen <- reactive({
        list(input$length,input$width,input$type, input$prefab)
    })
    
    
    pred_fin <- 0
    pred_carb <- 0
    pred_time <- 0
    max_thick <- 0
    min_thick <- 0
    
    
    observeEvent(toListen(), {
      
        
        
        data <- createData(input$type)
    
        length <- input$length
        width <- input$width
        
        # compute the nearest neighbors based on length and width
        nearest <- kNN(length, width, data, 5)
        
        
        
        
        #--------------------------------------------------------------------------
        # Financial Prediction Computation
        #--------------------------------------------------------------------------
        
        # find the cost data for the nearest neighbors
        nearest_fin <- nearest %>%
          select(proj_nr, costs_tot, bridge_type, deck_area, length, deck_width, dist_norm) %>%
          mutate(cost_per_area = costs_tot/deck_area) %>%
          rename(pred = cost_per_area)
        
        #compute financial prediction
        pred_fin <<- computePrediction(nearest_fin)
        
        
        #--------------------------------------------------------------------------
        # Construction Time Prediction Computation
        #--------------------------------------------------------------------------
        
        # find the time data for the nearest neighbors
        nearest_time <- nearest %>%
          select(proj_nr, constr_duration, bridge_type, deck_area, length, deck_width, dist_norm) %>%
          mutate(time_per_area = constr_duration/deck_area) %>%
          rename(pred = time_per_area)
        
        #compute time prediction
        pred_time <<- computePrediction(nearest_time)
        
        
        #--------------------------------------------------------------------------
        # Carbon Footprint Prediction Computation
        #--------------------------------------------------------------------------

        # find the co2 data for the nearest neighbors
        nearest_carb <- nearest %>%
          select(proj_nr, CO2_Structure, bridge_type, deck_area, length, deck_width, dist_norm) %>%
          mutate(carb_per_area = CO2_Structure/deck_area) %>%
          rename(pred = carb_per_area)
        
        #compute co2 prediction
        pred_carb <<- computePrediction(nearest_carb)
        
        #Prediction Plots
        predictionServer("predUI_cost", nearest_fin, pred_fin, input$length, input$width, 0, 30000, "€", "Cost")
        predictionServer("predUI_time", nearest_time, pred_time, input$length, input$width, 0, 10, "days", "Time")
        predictionServer("predUI_carb", nearest_carb, pred_carb, input$length, input$width, 0, 5000, "kg CO2", "Carbon Footprint")
        
        #--------------------------------------------------------------------------
        # Evidence Landscape
        #--------------------------------------------------------------------------
        output$landscape <- renderPlot(
          ggplot() +
            geom_point(data=data, aes(x=length, y=deck_width, color=data$bridge_type), size=2) +
            geom_point(data=nearest, aes(x=length, y=deck_width), size=5) +
            geom_point(aes(x=input$length, y=input$width), color="RED", size=5) +
            geom_label(data=nearest, aes(x=length, y=deck_width), label = nearest$proj_nr, nudge_x = 30, nudge_y = 0.2) +
            xlim(0,500) +
            ylim(0,10) + 
            guides(color=guide_legend(title="Bridge Typologies"))
        )
        
        
        num_nearest <- nrow(nearest)
        num_projects <- nrow(data)
        
        output$predSource <- renderText(paste("Predictions, Recommendations and Warnings based on the ", num_nearest, " most similar bridge projects from a total of ", num_projects, " projects."))

        nearest_safety <- nearest %>%
          select(proj_nr, accidents_fatal, accidents_non_fatal, cause_short, cause_long) 
        
        output$Warnings1 <- renderText(getPrefabSafety(input$prefab, input$type))
        output$Warnings2 <- renderText(getProjectSafety(nearest_safety))
        
        recommendationServer("rec1", input$type)
        
        # Display the five selected projects which are most similar
        projectServer("no1", 1, nearest[1,"proj_nr"][[1]], nearest_fin[1,"pred"][[1]], nearest_time[1,"pred"][[1]], nearest_carb[1,"pred"][[1]], nearest_safety[1,])
        projectServer("no2", 2, nearest[2,"proj_nr"][[1]], nearest_fin[2,"pred"][[1]], nearest_time[2,"pred"][[1]], nearest_carb[2,"pred"][[1]], nearest_safety[2,])
        projectServer("no3", 3, nearest[3,"proj_nr"][[1]], nearest_fin[3,"pred"][[1]], nearest_time[3,"pred"][[1]], nearest_carb[3,"pred"][[1]], nearest_safety[3,])
        projectServer("no4", 4, nearest[4,"proj_nr"][[1]], nearest_fin[4,"pred"][[1]], nearest_time[4,"pred"][[1]], nearest_carb[4,"pred"][[1]], nearest_safety[4,])
        projectServer("no5", 5, nearest[5,"proj_nr"][[1]], nearest_fin[5,"pred"][[1]], nearest_time[5,"pred"][[1]], nearest_carb[5,"pred"][[1]], nearest_safety[5,])
        
        
        # geom <- read_data("fb_3_basic_geometry.xlsx")
        # geom <- geom %>% select(proj_nr, deck_thickness)
        # geom$proj_nr <- as.numeric(geom$proj_nr)
        # 
        # # find the financial data for the nearest neighbors
        # nearest_geom <- left_join(nearest, geom, by="proj_nr")
        # 
        # max_thick <<- max(nearest_geom$deck_thickness, na.rm = TRUE)
        # min_thick <<- min(nearest_geom$deck_thickness, na.rm = TRUE)
        # 
        # print(max_thick)
        # print(min_thick)

    })
    
    
    observeEvent(input$export, {
      #-------------------------------------------------------------------------
      # Export
      #-------------------------------------------------------------------------
      
      ex_id     <- input$id
      ex_length <- input$length
      ex_width  <- input$width
      ex_type   <- input$type
      ex_cost   <- pred_fin
      ex_time   <- pred_time
      ex_carb   <- pred_carb

      export_df <- data.frame(ex_id, ex_length, ex_width, ex_type, ex_cost, ex_time, ex_carb, max_thick, min_thick)

      col_names <- c("id", "length", "width", "typology", "cost", "time", "carbon_footprint", "max_deck_thickness", "min_deck_thickness")
      
      colnames(export_df) <- col_names
      
      print(export_df)

      path = "./export/export.csv"
      
      if (file.exists(path)) {
        write.table(export_df, file = path, append = TRUE, col.names = FALSE, sep = ",", row.names = FALSE)
      } else {
        write.table(export_df, file = path, append = TRUE, col.names = TRUE, sep = ",", row.names = FALSE)
      }
      
      
    })
})
