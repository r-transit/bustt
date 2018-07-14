#' Get the most frequent service for a set of trips. 
#' 
#' @param trips gtfs dataframe with a service_trips count column grouped by the service_id, route, etc of interest
#' @return trips gtfs data frame within groups that are in the most frequent service window
#' @export
#' @importFrom dplyr %>%
#' @examples 
#' library(dplyr)
#' g1 <- count_service_trips(gtfs_obj$trips) %>% group_by(service_id)
#' most_frequent_service(g1)
most_frequent_service <- function(trips) {
  trips %>%
    dplyr::top_n(1, .data$service_trips)
}

#' Get the most frequent service id for a gtfs feed
#' 
#' @param trips gtfs dataframe with a service_trips count column grouped by the service_id, route, etc of interest
#' @return trips gtfs data frame within groups that are in the most frequent service window
#' @export
#' @importFrom dplyr %>%
#' @examples 
#' library(dplyr)
#' most_frequent_service_ids(gtfs_obj)
most_frequent_service_id <- function(g1, service_ids=NULL) {
  service_trip_counts <- count_service_trips(g1$trips_df)
  most_frequent_service_id <- service_trip_counts %>%
    dplyr::top_n(1,service_trips) %>%
    dplyr::pull(service_id) 
  
  #this seems reasonable though not perfect
  most_frequent_service_id[1]
}

#' Filter a gtfs calendar dataframe to service ids for specific days of the week.
#' 
#' @param gtfs_object object made by join_all_gtfs_tables
#' @param dow default to "weekday" (1,1,1,1,1,0,0)
#' @return service ids that match the schedule specified
#' @keywords internal
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
#' @keywords internal
count_service_trips <- function(trips) {
  trips %>%
    dplyr::group_by(.data$service_id) %>% 
      dplyr::mutate(service_trips = dplyr::n()) %>%
        tibble::as_tibble()
}

#' Get a set of stops for a given set of service ids
#' 
#' @param g1 gtfsr object
#' @param service_ids the service for which to get stops 
#' @return stops for a given service
#' @keywords internal
stops_for_service <- function(g1, select_service_id) {
  some_trips <- g1$trips_df %>%
    dplyr::filter(.data$service_id %in% select_service_id)
  
  some_stop_times <- g1$stop_times_df %>% 
    dplyr::filter(.data$trip_id %in% some_trips$trip_id) 
  
  some_stops <- g1$stops_df %>%
    dplyr::filter(.data$stop_id %in% some_stop_times$stop_id)
  
  return(some_stops)
}