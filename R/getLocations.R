#' @title Get All locations of NOAA Weather Stations
#'
#' @description get all locations information using API call
#'
#' @param online if TRUE location data is pulled from the API, if FALSE, data is pulled from offiline database
#' @param state  filter the final output based on the state code specified, deafault to all US states
#' @export
#' @return Dataframe with Location ID, Location name and State
#' @examples
#' locations <- getAllLocations ()
#' locations <- getAllLocations (FALSE)
#' locations <- getAllLocations (state="CA")
#' locations <- getAllLocations (online=FALSE,state="CA")
getAllLocations <- function(online=TRUE,state=NA){

  st <- state
  # save locations information

  if(online==TRUE){
    URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites"
    Locations <- fromJSON(RCurl::getURL(URL))
  } else{

    data("Locations")
  }

  if(st %in% (unique(Locations$state)) | is.na(st)){
    Locations <- Locations%>%
      dplyr::select(state,location,id)

    if(!is.na(st)){
      Locations <- Locations%>%
        filter(state==st)
    }

    Locations <- unique(Locations)

    #return
    Locations
  }else{
    print("Invalid State code, Please enter a valid state code")
  }


}
