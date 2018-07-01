
## Description

`bust` is a package for working with time in transit schedule data. In
particular, it is focused on questions like:

  - What are the headway of train lines at 59-Street Columbus Circle on
    Saturdays?
  - What are the typical headway characteristics of the A Train on
    weekdays?

## Installation

You can install this package from GitHub using the devtools package:

    if (!require(devtools)) {
        install.packages('devtools')
    }
    devtools::install_github('r-gtfs/gtschedule')

## Example Usage

``` r
library(dplyr)
library(tread)
library(bust)
```

``` r
NYC <- import_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")
#> [1] "agency.txt"         "calendar_dates.txt" "calendar.txt"      
#> [4] "routes.txt"         "shapes.txt"         "stop_times.txt"    
#> [7] "stops.txt"          "transfers.txt"      "trips.txt"
```

### Route Headways

List the routes with the shortest median headways.

``` r
route_frequency_summary <- route_frequency(NYC) %>%
  arrange(median_headways)

head(route_frequency_summary)
#> # A tibble: 6 x 5
#>   route_id median_headways mean_headways std_dev_headways stop_count
#>   <chr>              <int>         <int>            <dbl>      <int>
#> 1 GS                     4             4            0.01           4
#> 2 L                      4             4            0.13          48
#> 3 1                      5             5            0.14          76
#> 4 7                      5             5            0.290         44
#> 5 6                      6             7            2.84          76
#> 6 E                      6            23           53.0           48
```

### Stop Headways

List the stops with the shortest headways in the system.

``` r
stop_frequency_summary <- stop_frequency(NYC, by_route=FALSE) %>%
  inner_join(NYC$stops_df) %>%
    select(stop_name, headway) %>%
      arrange(headway)

head(stop_frequency_summary)
#> # A tibble: 6 x 4
#> # Groups:   direction_id, stop_id [6]
#>   direction_id stop_id stop_name             headway
#>          <int> <chr>   <chr>                   <dbl>
#> 1            0 902N    Times Sq - 42 St         3.60
#> 2            1 901S    Grand Central - 42 St    3.60
#> 3            1 902S    Times Sq - 42 St         3.60
#> 4            0 901N    Grand Central - 42 St    3.61
#> 5            0 702N    Mets - Willets Point     3.72
#> 6            0 707N    Junction Blvd            3.72
```
