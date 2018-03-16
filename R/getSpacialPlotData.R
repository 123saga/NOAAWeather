#' @title generate data for plot_spatial()
#'
#' @description generate data for plot_spatial()
#'
#' @param online if TRUE(default),realtime data is pulled from the API. if FALSE, data is pulled from sample offline database
#' @param date date for which spatial plot is needed. Default: 2017-01-01
#' @param metric metric for which spatial plot is needed. Default: "t_official"
#' @return a dataframe of values for the given metric and date
getSpatialPlotData  <- function(online=TRUE,
                                date="2017-01-01",
                                metric= "t_official"
)
{

  # format inputs
  from <- as.Date(date)
  to <- as.Date(date)+days(1)
  mtr <- as.character(metric)
  location_ids <- NA
  measures <- c("p_official","rh_std","solarad","t_max","t_min","t_official","windspd","ws_max")

  if(!(metric %in% measures)){
    print(paste0("Please enter a valid measure from: ",paste0(measures,collapse = ",")))

  } else {


    data("Distance_data_master",envir = environment())
    Locations <- Distance_data_master

    #get list of location id for API call
    locations <- unique(Locations%>%
                          dplyr::select(id,latitude, longitude,state,location))

    location_ids <- locations$id

    # check offline flag
    if(online==FALSE){
      ## connect to rda file
      data("weather_data",envir = environment())
      weather_data_master<- weather_data

      weather_data <- weather_data_master%>%
        filter(metric==mtr)%>%
        filter(as.Date(time) == from)%>%
        filter(flag==0)%>%
        group_by(id) %>%
        summarize(value=max(value)) %>%
        inner_join(locations, by=c("id"))

      ## return data
      if(nrow(weather_data)==0){
        print("No data avaiable for given input parameters, please check the values once again")
      }
      weather_data

    }else {

      ## make API call
      for (i in 1:length(location_ids)){
        loc_id<-location_ids[i]
        API_URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites/"
        API_URL_final<- paste0(API_URL,loc_id,
                               "/data?start=",from,"T00:00Z&end=",to,
                               "T00:00Z&metric=",mtr)


        data <- fromJSON(RCurl::getURL(API_URL_final))

        if(is.data.frame(data)){
          data <- data[,c("start","value","metric","flag")]
          names(data) <- c("time","value","metric","flag")
          data$id <- loc_id
          if(exists("weather_data_online")){
            weather_data_online <- rbind(weather_data_online,data)
          } else{
            weather_data_online <- data
          }
        }


      }
      ## return data
      if(!exists("weather_data_online")){
        print("No data avaiable for given input parameters, please check the values once again")
      }
      weather_data_online <- weather_data_online%>%
        filter(flag==0)%>%
        group_by(id) %>%
        summarize(value=max(value)) %>%
        inner_join(locations, by=c("id"))


      weather_data_online
    }
  }# end of input verification
}
