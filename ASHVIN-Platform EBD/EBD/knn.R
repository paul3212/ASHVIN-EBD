library(readxl)
library(tidyverse)


createData <- function(type){

  # status <- read_data("status.xlsx")
  # fin    <- read_data("fb_7_financial_aspects.xlsx")
  # struc  <- read_data("fb_2_basic_struct_prop.xlsx")
  # geom   <- read_data("fb_3_basic_geometry.xlsx")
  # time   <- read_data("fb_4_timeline.xlsx")
  # carb   <- read_data("fb_10_env_imp.xlsx")
  # 
  # 
  # status_built <- filter(status, status == "built")
  # 
  # time <- time %>% select(proj_nr, constr_duration)
  # carb <- carb %>% select(proj_nr, CO2_Structure)
  # 
  # data <- left_join(status_built, struc, by="proj_nr")
  # data <- left_join(data, fin, by="proj_nr")
  # data <- left_join(data, time, by="proj_nr")
  # data <- left_join(data, carb, by="proj_nr")
  # 
  # if (!(type == "no selection")) {
  #   data <- data[data$bridge_type == type, ]
  # }
  # 
  # data$proj_nr <- as.integer(data$proj_nr)
  # geom$proj_nr <- as.integer(geom$proj_nr)
  # 
  # data <- left_join(data, geom, by="proj_nr")
  
  data <- read_data("footbridge_database.xlsx")
  
  data <- data %>%
    select(proj_nr, bridge_type, deck_area, length, deck_width,  
           costs_tot, constr_duration, CO2_Structure, accidents_fatal, 
           accidents_non_fatal, cause_short, cause_long)
  
  data$deck_area <- as.numeric(data$deck_area)
  data$length <- as.numeric(data$length)
  data$deck_width <- as.numeric(data$deck_width)
  
  data <- data %>% drop_na(length, deck_width)


  return(data)
}




distance <- function(width1, length1, width2, length2, dist_tune, max_length, max_width){
    return(sqrt(((width1-width2)/max_width)^2+((length1-length2)/max_length)^2)^dist_tune)
  
    
}




kNN <- function(length, width, data, k, dist_tune = 1){
  
  max_length = max(data$length)
  max_width =max(data$deck_width)
  

  data$dist <- mapply(distance, data$deck_width, data$length, width2 = width, length2 = length, dist_tune=dist_tune, max_length=max_length, max_width= max_width)

  
  
  data$dist_norm <- normalize(data$dist)
  
  
  ordered <- data[order(data$dist_norm),] 
  nearest <- ordered[1:k,]
  
  return(nearest)
  
}

# compute the Prediction based on the given list of nearest 
computePrediction <- function(nearest){
  nearest <- nearest %>% drop_na(pred)

  pred <- sum(nearest$pred/nearest$dist_norm)/sum(1/nearest$dist_norm)
  return(pred)
}


# Normalize between 0 and 1. 0 only if distance = 0
normalize <- function(x) {
  return ((x - 0) / (max(x) - 0))
}


computeConvidence <- function(dist){
  if (dist < 1) {
    return(1)
  } else {
    return(1/dist )
  }
}




