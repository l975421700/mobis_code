

# Function to count point in spatial grid ---------------------------------

suppressPackageStartupMessages(library('raster'))


point_count <- function(
    lon, lat, lon_range, lat_range, n = c(100, 100)){
    
    # Input description ----
    # lon: vector of longitude
    # lat: vector of latitude
    # lon_range: range of longitude used for gird
    # lat_range: range of latitude used for gird
    # n: number of grid
    # 
    # 
    # Value ----
    # point_count_res: a raster contaning counts of points in each grid
    
    # xmn = 17;xmx = 23;ymn=42;ymx=49
    # 
    # r = raster(matrix(1:12,3,4), xmx=xmx, xmn=xmn,ymx=ymx,ymn=ymn)
    
    if(length(n) == 1){
        n = rep(n , 2)
    }
    
    point_count_res <- raster::raster(
        matrix(0, n[1], n[2]), 
        xmn = lon_range[1], xmx = lon_range[2],
        ymn = lat_range[1], ymx = lat_range[2]
    )
    
    points <- data.frame(x = lon, y = lat)
    
    counts <- table(raster::cellFromXY(point_count_res, points))
    
    point_count_res[as.numeric(names(counts))] <- counts
    
    return(point_count_res)
}


# Example
# lon <- unclassified_swiss$lon
# lat <- unclassified_swiss$lat
# lon_range <- c(5.9, 10.55)
# lat_range <- c(45.78, 47.85)
# res <- point_count(
#     lon, lat, lon_range, lat_range, n = c(100, 100))
# plot(res)

