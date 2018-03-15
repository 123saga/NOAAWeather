#' @title get all Avaialble Metrics
#'
#' @description get all Avaialble Metrics from NOAA Web API along with descriptions and units
#'
#' @param online if TRUE metrics data is pulled from the API, if FALSE, data is pulled from offiline database
#'
#' @export
#'
#' @return All metrics Information
getMetrics <- function(online=TRUE){

  if(online==TRUE){
    URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/metrics"
    Metrics <- fromJSON(RCurl::getURL(URL))
  } else{

    data("Metrics")
  }

  #return
  Metrics


}
