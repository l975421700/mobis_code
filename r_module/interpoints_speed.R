
# Function to calculate speed and acceleration from GPS points ------------

suppressPackageStartupMessages(library('sf'))
library('geosphere')


interpoints_speed <- function(trip_waypoints){
    
    # Input ----
    # 
    # trip_waypoints: trip GPS points info
    # $tracked_at
    # $geom
    # 
    # Output ----
    # 
    # trip_info: quantiles of speed and acceleration
    # 
    # Function ----
    
    # make sure the data is ordered by date and no 
    trip_waypoints <- trip_waypoints[order(tracked_at)]
    trip_waypoints$duration <- c(99, as.numeric(
        difftime(trip_waypoints$tracked_at[-1], 
                 trip_waypoints$tracked_at[-nrow(trip_waypoints)], 
                 units = 'secs')
    ))
    
    trip_waypoints <- trip_waypoints[duration >= 2, ]
    
    if(dim(trip_waypoints)[1] < 2){
        # In case there is no trip_waypoints
        trip_info <- as.data.frame(t(as.matrix( rep(0, 20) )))
    }else {
        # Calculation of speed
        trip_duration <- as.numeric(
            difftime(trip_waypoints$tracked_at[-1], 
                     trip_waypoints$tracked_at[-nrow(trip_waypoints)], 
                     units = 'secs')
        )
        trip_distance <- geosphere::distGeo(
            p1 = trip_waypoints[-1, .(longitude, latitude)], 
            p2 = trip_waypoints[-nrow(trip_waypoints), .(longitude, latitude)])
        
        trip_speed <- trip_distance/trip_duration
        
        if(length(trip_speed) > 1){
            trip_speed_std <- sd(trip_speed)
        } else{
            trip_speed_std <- 0
        }
        
        
        
        # Calculation of acceleration
        if(length(trip_speed) < 2){
            trip_acceleration = 0
        }else{
            trip_interval <- (trip_duration[-1] + trip_duration[-length(trip_duration)])/2
            trip_speedchange <- (trip_speed[-1] - trip_speed[-length(trip_speed)])
            trip_acceleration <- trip_speedchange/trip_interval
        }
        
        if(length(trip_acceleration) > 1){
            trip_acceleration_std <- sd(trip_acceleration)
        } else{
            trip_acceleration_std <- 0
        }

        # Calculation of quantiles of speed and acceleration
        trip_info <- as.data.frame (t(as.matrix( c(
            quantile(trip_speed, 
                     c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1), 
                     na.rm = TRUE), 
            trip_speed_std,
            quantile(trip_acceleration, 
                     c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1), 
                     na.rm = TRUE), 
            trip_acceleration_std
        ) ) ) )
    }
    
    colnames(trip_info) <- c(
        paste('speed_', c(0, 5, 10, 25, 50, 75, 90, 95, 100), '%', sep = ''),
        'speed_std',
        paste('acceleration_', c(0, 5, 10, 25, 50, 75, 90, 95, 100), 
              '%', sep = ''),
        'acceleration_std'
    )
    
    return(trip_info)
}


# sql_info <- list(
#     dbname = "mobis_study",
#     host = "id-hdb-psgr-cp50.ethz.ch",
#     user = "mobis_i",
#     password = "mobis_i")
# conn <- dbConnect(PostgreSQL(),
#                   dbname = sql_info$dbname,
#                   host = sql_info$host,
#                   user = sql_info$user,
#                   password = sql_info$password)
# trip_id <- 14279544
# 
# mapview(trip_waypoints)

# trip_distance <- sf::st_distance(
#     x = trip_waypoints$geom[-1], 
#     y = trip_waypoints$geom[-nrow(trip_waypoints)],
#     by_element = TRUE)


# trip_waypoints <- sf::st_read(
#     conn, 
#     query = sprintf(
#         "select * from motion_tag_waypoint where trip_id IN (%s)", 
#         paste(paste(
#             '\'', 
#             '13456512', 
#             '\'', sep = ''), collapse=", "
#         )
#     )
# )
# trip_waypoints <- as.data.table(trip_waypoints)
# trip_waypoints <- trip_waypoints[
#     trip_waypoints$trip_id %in% 
#         names(which(table(trip_waypoints$trip_id) > 1)),
#     ]
# 


