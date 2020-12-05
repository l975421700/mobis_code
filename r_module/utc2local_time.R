
# Function to transform global time to local time -------------------------

library('lutz')
library('parallel')

utc2local_time <- function(utctime, sf_location){
    
    # Input ----
    # 
    # utctime: utc time
    # sf_location: spatial_feature, either an sfc or sf points or 
    # SpatialPoints(DataFrame) spatial_feature: 
    # 
    # 
    # Output ----
    # 
    # local_time: a vector of local time
    # 
    # 
    # Function ----
    
    time_zone <- lutz::tz_lookup(sf_location, warn = FALSE)
    
    local_time <- as.POSIXct(
        parallel::mcmapply(function(x, y){format(x, tz = y, usetz = TRUE)},
                           utctime, 
                           time_zone, 
                           mc.cores = 2), 
        format = '%Y-%m-%d %H:%M:%OS')
    
    return(local_time)
}



