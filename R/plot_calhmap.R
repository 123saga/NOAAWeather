#' @title calendar heat map of weather data
#'
#' @description generate calendar heat map for a given time period, measure, location, and state
#'
#' @param online if TRUE(default),realtime data is pulled from the API. if FALSE, data is pulled from sample offline database
#' @param from Beginning of date range. Default: 2017-01-01
#' @param to End of date range. Default: 2017-01-31
#' @param measure metric for which time series plot is needed. Default: "t_max"
#' @param location Valid location supported by NOAA, use \code{\link{getAllLocations}} for view avaialble locations. Default: "Wolf Point"
#' @param state Valid two letter code of US state. Default: "MT"
#' @param text if TRUE, print the values of each day on heatmap, if FALSE values are not printed
#' @export
#' @examples
#' plot_calhmap(online=FALSE,from="2017-01-01",
#' to="2017-01-03", measure="t_max",
#' location='Austin', state='TX')
#' @return calendar heat map for a given time period, measure, location, and state
plot_calhmap <- function(online=TRUE,from='2017-01-01', to='2017-01-31', measure='t_max', location='Wolf Point',state='MT', text=TRUE){


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

  print("Advice: Pass text=FALSE if difference between start and end date is more than 2 months")

  # prepare lables for plot
  metrics_desc_map <- getMetrics()
  metrics_desc_map <- metrics_desc_map[c("id","description","units")]
  metrics_desc_map <- subset(metrics_desc_map,id %in% measures_list)

  metrics_desc_map$display_text <- paste0(toupper(metrics_desc_map$id)," [",metrics_desc_map$description,"]")

  plot_title <- paste0("Heat Map for:",metrics_desc_map[which(metrics_desc_map$id==measure),c("display_text")])

  ## function to get weather a data by location, all other params are optional
  data <- getWeatherData(online=onl,
                         location=loc,state=st,
                         from=sdate,
                         to=edate)

  if(is.data.frame(data)){

  # filter for selected params
  data <- separate(data, col=time, into=c("date","time"), sep="T") %>%
    select(date, metric, value) %>%
    group_by(metric, date) %>%
    summarize(value=max(value)) %>%
    filter(metric==measure)


  # prepating data fro plotting
  data$date <- as.Date(data$date)
  data$metric <- as.factor(data$metric)
  data$value <- as.numeric(data$value)

  data <- select(data, date, metric, value)

  #getting data ready for plot
  data$day = lubridate::wday(data$date,label=TRUE)
  data$day = with(data, factor(day, levels = rev(levels(day))))
  data$weekStart = data$date - as.POSIXlt(data$date)$wday

  #rescaling the value
  data = data %>% group_by(metric)%>%
    mutate(rescaled_value = scales::rescale(value))

  # plotting the heatmap
  plot<-  ggplot(data,aes(x=weekStart, y=day, fill=value))+
    geom_tile(colour="white",size=.1) +
    scale_fill_gradient(high="red",low="yellow") +
    scale_x_date(breaks=unique(data$weekStart),date_labels="%d-%b-%y")+
    theme_minimal(base_size = 20)+
    removeGrid()+
    rotateTextX()+
    ggtitle(label=plot_title)+
    labs(x="Calender Week Day (Sunday)", y=NULL) +
    theme(
      legend.position="none",
      plot.title = element_text(hjust = 0.5)
    )

  if(text==TRUE){
    plot+
      geom_text(data=data,aes(weekStart,day,label=value),colour="black",size=6)
  } else {
    plot
  }
  }
  }
}
