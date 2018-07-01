#' Get a dataframe with lubridate dates for the gtfs stop_times_df 
#' 
#' @param stop_times_df a gtfsr$stop_times_df dataframe
#' @return an dataframe with arrival and departure time set to lubridate types
#' @examples 
#' gt_as_dt(stop_times_df)
#' stop_times_dt <- stop_times_df_as_dt(some_stops)
#' #plot the histogram of departure times by hour
#' hist(hour(stop_times_dt$departure_time))
gt_as_dt <- function(stop_times_df) {
  stop_times_dt <- stop_times_df %>% 
    dplyr::mutate(
      departure_time = lubridate::hms(departure_time),
      arrival_time = lubridate::hms(arrival_time)
    )
  return(stop_times_dt)
}

#' Filter stop times by hour of the day
#' 
#' @param stop_times_df a gtfsr$stop_times_df dataframe with lubridate arrival_time and departure_time
#' @return dataframe with only stop times within the hours specified, with time columns as lubridate periods
#' @examples 
#' filtered_stop_times <- filter_stop_times_by_hour(stop_times_df,start_hour=7,end_hour=10)
filter_stop_times_by_hour <- function(stop_times, 
                                      start_hour, 
                                      end_hour) {
  stop_times_dt <- gt_as_dt(stop_times)
  stop_times <- stop_times[lubridate::hour(stop_times_dt$arrival_time) > start_hour &
                     lubridate::hour(stop_times_dt$departure_time) < end_hour,]
  return(stop_times)
}
