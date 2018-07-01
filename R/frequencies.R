#' for a trips df and a stop_times df, count the number of trips a bus takes through a given stop within a given time period
#' @param gtfsr object
#' @param start_hour (optional) an integer indicating the start hour (default 7)
#' @param end_hour (optional) an integer indicating the end hour (default 20)
#' @param dow (optional) integer vector indicating which days of week to calculate for. default is weekday, e.g. c(1,1,1,1,1,0,0)
#' @param byroute default TRUE, if FALSE then calculate headway for any line coming through the stop in the same direction on the same schedule. 
#' @param wide (optional) if true, then return a wide rather than tidy data frame
#' @param service_ids (optional) to calculate for a specific service id
#' @export
#' @return a dataframe of stops with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
stop_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            dow=c(1,1,1,1,1,0,0),
                            by_route=TRUE,
                            wide=FALSE) {
  trips <- gtfs_obj$trips_df 
  stop_times <- gtfs_obj$stop_times_df
  calendar <- gtfs_obj$calendar_df

  stop_times <- filter_stop_times_by_hour(stop_times, 
                                          start_hour, 
                                          end_hour)

  service_ids <- service_by_dow(calendar,dow)
  
  trips <- trips %>% 
    filter(service_id %in% service_ids) %>%
      count_service_trips(.)
  
  stop_time_trips <- inner_join(stop_times,
                                trips, 
                                by="trip_id")
  if(by_route==FALSE){
    stop_time_trips <- stop_time_trips %>%
      group_by(direction_id,
               stop_id,
               service_id) %>%
      most_frequent_service(.) %>%
      summarise(departures = n())
  } 
  else if(by_route==TRUE) {
  stop_time_trips <- stop_time_trips %>%
    group_by(route_id,
             direction_id,
             stop_id,
             service_id) %>%
    most_frequent_service(.) %>%
      summarise(departures = n())
  }
  t1 <- end_hour - start_hour
  minutes1 <- 60*t1
  stop_time_trips$headway <- minutes1/stop_time_trips$departures
  
  if(wide==TRUE){
    stops_frequencies <- stops_frequencies %>%
      select(-departures,-service) %>%
      tibble::rowid_to_column() %>%
      tidyr::spread(direction, headway, sep="_")
  }
  return(stop_time_trips)
}

#' Get stop frequency for buses aggregated up to routes
#' 
#' should take: 
#' @param gtfs_obj a standard gtfsr object
#' @param start_time, 
#' @param end_time, 
#' @param service e.g. "weekend" or "saturday"
#' @return route_headways a dataframe of route headways
#' @export
route_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            dow=c(1,1,1,1,1,0,0)) {
  stop_frequency_df <- stop_frequency(gtfs_obj,
                                      start_hour, 
                                      end_hour,
                                      dow)  
  
  if (dim(stop_frequency_df)[[1]]!=0) {
    route_headways <- stop_frequency_df %>%
      group_by(route_id) %>%
      summarise(median_headways = as.integer(round(median(headway),0)),
                mean_headways = as.integer(round(mean(headway),0)),
                std_dev_headways = round(sd(headway),2),
                stop_count = n())
  } else
  {
    warning("agency gtfs has no published service for the specified period")
  }
  return(route_headways)
}

