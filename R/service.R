#' Get the most frequent service for a set of trips. 
#' 
#' @param trips gtfs dataframe with a service_trips count column
#' @return the trips that are in the most frequent service window
#' @export
most_frequent_service <- function(trips) {
  trips %>%
    top_n(1, service_trips)
}

#' Filter a gtfs calendar dataframe to service ids for specific days of the week.
#' 
#' @param gtfs_object object made by join_all_gtfs_tables
#' @param dow default to "weekday" (1,1,1,1,1,0,0)
#' @return service ids that match the schedule specified
#' @export 
service_by_dow <- function(calendar_df,
                           dow=c(1,1,1,1,1,0,0)){
  calendar_df <- subset(calendar_df, 
                        calendar_df$monday == dow[1] & 
                        calendar_df$tuesday == dow[2] & 
                        calendar_df$wednesday == dow[3] & 
                        calendar_df$thursday == dow[4] & 
                        calendar_df$friday == dow[5] &
                        calendar_df$saturday == dow[6] &
                        calendar_df$sunday == dow[7])
  return(calendar_df$service_id)
}

#' Summarise the number of trips per service
#' 
#' @param gtfsr object
#' @return count of service by id
#' @export
count_service_trips <- function(trips) {
  trips %>%
    group_by(service_id) %>% 
      mutate(service_trips = n()) %>%
        as_tibble()
}

#' Get a set of stops for a given set of service ids
#' 
#' @param g1 gtfsr object
#' @param service_ids the service for which to get stops 
#' @return stops for a given service
#' @keywords internal
stops_for_service <- function(g1, select_service_id) {
  some_trips <- g1$trips_df %>%
    filter(service_id %in% select_service_id)
  
  some_stop_times <- g1$stop_times_df %>% 
    filter(trip_id %in% some_trips$trip_id) 
  
  some_stops <- g1$stops_df %>%
    filter(stop_id %in% some_stop_times$stop_id)
  
  return(some_stops)
}