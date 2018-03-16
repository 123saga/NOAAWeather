#' @title geo-spatial heat map of weather data
#'
#' @description generate geo-spatial heat map for a given date and measure across all NOAA weather stations
#'
#' @param online if TRUE(default),realtime data is pulled from the API. if FALSE, data is pulled from sample offline database
#' @param Date Beginning of date range. Default: 2017-01-01
#' @param measure metric for which geo-spatial heat map is needed. Default: "t_max"
#' @export
#' @examples
#' \dontrun{
#' plot_spatial(online=FALSE,
#' Date="2017-01-10",
#' measure="t_max")
#' plot_spatial(online=TRUE,
#' Date="2017-01-10",
#' measure="t_official")
#' }
#' @return geo-spatial heat map for a given date and measure across all NOAA weather stations
plot_spatial <- function(online=TRUE,Date="2017-01-01", measure="t_max") {

  onl<- online
  if(onl==TRUE){
    print("Retrieving data for all the avaialble Locations takes time, please wait!")
  }
  Date<- as.Date(Date)
  measure<- as.character(measure)

  # prepare lables for plot
  metrics_desc_map <- getMetrics()
  metrics_desc_map <- metrics_desc_map[c("id","description","units")]
  measures <- c("p_official","rh_std","solarad","t_max","t_min","t_official","windspd","ws_max")
  metrics_desc_map <- subset(metrics_desc_map,id %in% measures)
  metrics_desc_map$display_text <- paste0(toupper(metrics_desc_map$id)," [",metrics_desc_map$description,"]")
  plot_title <- paste0("Spatial plot:",metrics_desc_map[which(metrics_desc_map$id==measure),c("display_text")])

  data <-  getSpatialPlotData(online=onl,date = Date, metric = measure)

  out <- tryCatch({
    #Continental States
    suppressMessages(map <- get_map(location='united states', zoom=4, maptype = "terrain",
                                    source='google', color='bw'))

    CS<-ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=0.9, size=5) +
      theme_minimal(base_size = 20) + scale_color_gradientn(colours =
                                                              c("#313695","#4575b4", "#74add1","#abd9e9","#fdae61", "#f46d43","#d73027","#a50026"))

    #Alaska
    suppressMessages(map<-get_map(location='Alaska', zoom=4, maptype = "terrain",
                                  source='google',color='bw'))

    AK <- ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=1, size=5) +
      theme_minimal(base_size = 20) + scale_color_gradientn(colours =
                                                              c("#313695","#4575b4", "#74add1","#abd9e9","#fdae61", "#f46d43","#d73027","#a50026"))+
      theme(legend.position = 'none')

    #Hawaii
    suppressMessages(map<-get_map(location='Hawaii', zoom=8, maptype = "terrain",
                                  source='google',color='bw'))
    HI <- ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=1, size=5) +
      theme_minimal(base_size = 20) + scale_color_gradientn(colours =
                                                              c("#313695","#4575b4", "#74add1","#abd9e9","#fdae61", "#f46d43","#d73027","#a50026"))+
      theme(legend.position = 'none')

    #plotting on map
    options(warn=-1)
    return(grid.arrange(CS,AK, HI,layout_matrix = rbind(c(2,2,1,1,1,1), c(3,3,1,1,1,1)),
                        top = textGrob(plot_title,
                                       gp=gpar(fontsize=20))))
    options(warn=0)
  },error = function(err) {
    return(cat("Following Error is because of Google Maps API, please re-try again after sometime: \n\n",
               paste0(err,collapse = ";")))

  },Warning = function(war) {
    return(cat("Following Warning is because of Google Maps API, please re-try again after sometime: \n\n",
               paste0(war,collapse = ";")))

  }
  )
  #return(out)

}
