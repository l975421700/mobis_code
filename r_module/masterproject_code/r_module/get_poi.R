
# Function to return poi name&types from google places api ----------------


library(googleway)

get_poi <- function(latitude, longitude, radius, api_key, 
                    info = c('name', 'types'), ID = NULL){
    
    # Input description ----
    # 
    # latitude / longitude: location information
    # 
    # radius: spatial scope in m.
    # 
    # api_key: grant access to google API
    # 
    # info: extract information
    # 
    # ID: if not null, combine ID information with returned data.frame
    # 
    # Output ----
    # 
    # nearby_poi
    # 
    # 
    # Function ----
    
    poi <- googleway::google_places(
        location = c(latitude, longitude), radius = radius, key = api_key)
    
    if(length(poi$results) == 0){
        nearby_poi <- data.frame(name = NA, types = NA)
    }else {
        nearby_poi <- poi$results[info]
    }
    
    while(! is.null(poi$next_page_token)){
        Sys.sleep(1.51)
        poi <- googleway::google_places(
            location = c(latitude, longitude),
            radius = radius, key = api_key,
            page_token = poi$next_page_token)
        nearby_poi <- rbind(nearby_poi, poi$results[info] )
    }
    
    if(!is.null(ID)){
        nearby_poi <- cbind(activitiy_id = ID, nearby_poi)
    }
    
    return(nearby_poi)
}

# e.g.
# latitude <- 30.526529
# longitude <- 114.356518
# radius <- 50
# api_key <- 'AIzaSyDEA__8d-hxOJEsA7dbYxU9RpqDpqFqJMI'
# nearby_poi <- get_poi(latitude, longitude, radius, api_key)




