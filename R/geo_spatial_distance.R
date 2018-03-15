#' @title Calculates Spatial distance
#' @description Generates a dataframe with spatial distance between the all possible combinations of weather stations
#' Please run the following script if there is a new location added
#'
#'
# library(RCurl)
# library(jsonlite)
# library(geosphere)
#
# URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites"
#
# data("Locations")
#
# for(i in 1:nrow(Locations)){
#   ## get current location
#   Current_location <- Locations[c(i),]
#   ## get rest of the location
#   Rest_of_location <- Locations[-c(i),]
#
#   for(j in 1:nrow(Rest_of_location)){
#     Rest_of_location$Distance[j] <- distm(c(Current_location$longitude, Current_location$latitude),
#                                           c(Rest_of_location$longitude[j], Rest_of_location$latitude[j]),
#                                           fun = distHaversine)
#
#   }
#   names(Rest_of_location) <- paste0("TO_",names(Rest_of_location))
#
#   Distance_data <- merge(as.data.frame(Current_location),
#                          as.data.frame(Rest_of_location),
#                          by=NULL)
#
#   if(exists("Distance_data_master")){
#     Distance_data_master <- rbind(Distance_data_master,Distance_data)
#   } else
#     Distance_data_master <- Distance_data
# }
#
# ## save distances (lot of TO_columns can be dropped later)
# save(Distance_data_master,file="Distance_data_master.rda")
