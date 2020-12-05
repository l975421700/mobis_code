
# Function to transform wkb geometry to longitude and latitude ------------


suppressPackageStartupMessages(library('sf'))
suppressPackageStartupMessages(library('dplyr'))

wkb2lonlat <- function(wkb_character, 
                       suffix = NULL, 
                       col_names = c('sf', 'lon', 'lat')){
    
    # Input ----
    # 
    # wkb_character: WKB form of geometry
    # 
    # suffix: suffix used to add to col_names
    # 
    # col_names: colnames used for output
    # 
    # Output ----
    # 
    # wkb_character_res: a data frame contaning three columns
    # $sf: 
    # $lon: 
    # $lat: 
    # 
    # Function ----
    
    sf <- sf::st_transform(sf::st_as_sfc(structure(
        wkb_character, class ="WKB"), EWKB=TRUE), crs = "+proj=longlat")
    
    lon <- sapply(sf, function(x) x[1])
    lat <- sapply(sf, function(x) x[2])
    
    wkb_character_res <- data.frame(sf = sf, lon = lon, lat = lat)
    
    if(!is.null(suffix)){
        col_names <- paste(suffix, col_names, sep = '_')
    }
    colnames(wkb_character_res) <- col_names
    
    return(wkb_character_res)
}








