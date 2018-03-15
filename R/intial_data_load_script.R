#' @title Generate offline database for 2017
#'
#' @description An util script generate offline database from Web API for all locations for 2017. uncomment and source the script
#' @return a dataframe with 2017 weather data for all locations
#' and metrics: ("p_official","rh_std","solarad","t_max","t_min","t_official","windspd","ws_max")
#'
# library(tidyverse)
# library(data.table)
# library(zoo)
# library(lubridate)
# library(RCurl)
# library(jsonlite)
#
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# options(scipen = 999)
#
# Locations <- getAllLocations()
#
# location_ids <- Locations%>%
#   dplyr::select(id)
#
# ## params chage to jan-jun
# from <- as.Date("2017-01-01")
# to <- as.Date("2017-12-31")
#
# ## progress
# pb <- txtProgressBar(min = 0, max = nrow(location_ids), style = 3)
#
#
# ## make API call
# for (i in 1:nrow(location_ids)){
#   loc_id<-location_ids$id[i]
#   API_URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites/"
#   API_URL_final<- paste0(API_URL,loc_id,
#                          "/data?start=",from,"T00:00Z&end=",to,
#                          "T00:00Z&metric=t_official&metric=t_max&metric=t_min&metric=ws_max",
#                          "&metric=windspd&metric=rh_std&metric=solarad&metric=p_official")
#
#
#   data <- fromJSON(RCurl::getURL(API_URL_final))
#
#   if(is.data.frame(data)){
#     data <- data[,c("start","value","metric","flag")]
#     names(data) <- c("time","value","metric","flag")
#     data$id <- loc_id
#     if(exists("weather_data")){
#       weather_data <- rbind(weather_data,data)
#     } else
#       weather_data <- data
#   }
#
#   setTxtProgressBar(pb, i)
#
# }
#
# close(pb)
#
# weather_data <- unique(weather_data)
#
# save(weather_data,file='weather_data.rda')
