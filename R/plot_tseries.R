#' @title time-series plot of weather data
#'
#' @description generate time series plot for a given time period, measure, location, and state
#'
#' @param online if TRUE(default),realtime data is pulled from the API. if FALSE, data is pulled from sample offline database
#' @param from Beginning of date range. Default: 2017-01-01
#' @param to End of date range. Default: 2017-01-31
#' @param measure metric for which time series plot is needed. Default: "t_max"
#' @param location Valid location supported by NOAA, use \code{\link{getAllLocations}} for view avaialble locations. Default: "Wolf Point"
#' @param state Valid two letter code of US state. Default: "MT"
#' @export
#' @examples
#' plot_tseries(online=FALSE,from="2017-01-01",
#' to="2017-01-03", measure="t_max",
#' location='Austin', state='TX')
#' @return time series plot for a given time period, measure, location, and state
plot_tseries <- function(online=TRUE,from='2017-01-01', to='2017-01-31', measure='t_max', location='Wolf Point',state='MT'){


  sdate <- as.Date(from)
  edate <- as.Date(to)
  loc <- as.character(location)
  st <- as.character(state)
  measure <- as.character(measure)
  onl <- online

  measures_list <- c("p_official","rh_std","solarad","t_max","t_min","t_official","windspd","ws_max")

  if(!(measure %in% measures_list)){
    print(paste0("Please enter a valid measure from: ",paste0(measures_list,collapse = ", ")))

  } else {

    # prepare lables for plot
    metrics_desc_map <- getMetrics()
    metrics_desc_map <- metrics_desc_map[c("id","description","units")]
    metrics_desc_map <- subset(metrics_desc_map,id %in% measures_list)
    metrics_desc_map$display_text <- paste0(toupper(metrics_desc_map$id)," [",metrics_desc_map$description,"]")

    plot_title <- paste0("Time Series plot:",metrics_desc_map[which(metrics_desc_map$id==measure),c("display_text")])

    ## function to get weather a data by location, all other params are optional
    data <- getWeatherData(online=onl,
                           location=loc,state=st,
                           from=sdate,
                           to=edate)


    if(is.data.frame(data)){

      data <-   data%>%
        filter(metric==measure) %>%
        separate(col=time, into=c("date","time"), sep="T") %>%
        group_by(date) %>%
        summarize(value=max(value))

      data$date <- as.Date(data$date)


      # Print units on y axis depending on measure
      if (measure=="t_official" | measure=="t_max" | measure=="tmin"){
        y_axis_label = "Temperature (Celcius)"
      } else if (measure=="p_official") {
        y_axis_label = "Precipitation (mm)"
      } else if (measure=="windspd" | measure=="ws_max") {
        y_axis_label = "Wind Speed (m/s)"
      } else if (measure=="rh_std") {
        y_axis_label = "Relative Humidity %"
      } else if (measure=="solarad"){
        y_axis_label = "Solar Radiation (W/m^2)"
      }


      plot <-  ggplot(data) +
        aes(x=date, y=value) +
        geom_line(colour="blue", size=1) +
        removeGrid() +
        scale_x_date(date_labels="%d%b'%y") +
        theme_minimal(base_size = 20) +
        rotateTextX() +
        ggtitle(label=plot_title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x=NULL, y=y_axis_label)

      plot
    }
  }
}
