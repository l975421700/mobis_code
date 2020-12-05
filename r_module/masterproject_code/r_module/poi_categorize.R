
# Function to categorize poi returned by 'get_poi' ------------------------

poi_types <- c('Leisure_p', 'Food_p', 'Store_p', 'Transport_p', 'Religion_p', 
               'Education_p', 'Health_p', 'Civic_p', 'Business_p', 'Unknown_p')

poi_places <- list(
    Leisure_p = c('art_gallery', 'gym', 'movie_rental', 'movie_theater', 
                  'museum', 'amusement_park', 'aquarium', 'bowling_alley', 
                  'casino', 'park', 'stadium', 'tourist_attraction', 'zoo', 
                  'beauty_salon', 'spa', 'bar', 'night_club', 'rv_park', 
                  'lodging', 'campground', 'cemetery'), 
    Food_p = c('cafe', 'restaurant', 'meal_delivery', 'meal_takeaway', 'food'),
    Store_p = c('book_store', 'clothing_store', 'grocery_or_supermarket', 
                'store', 'supermarket', 'bicycle_store', 'convenience_store',
                'department_store', 'florist', 'furniture_store', 
                'hardware_store', 'home_goods_store', 'drugstore', 
                'electronics_store', 'jewelry_store', 'liquor_store', 
                'pet_store', 'shoe_store', 'shopping_mall', 'car_dealer', 
                'bakery'),
    Transport_p = c('bus_station', 'subway_station', 'transit_station', 
                    'train_station', 'taxi_stand', 'airport', 
                    'light_rail_station', 'intersection', 'route', 
                    'street_address', 'street_number', 'parking', 
                    'gas_station'),
    Religion_p = c('church', 'funeral_home', 'mosque', 'hindu_temple', 
                   'synagogue', 'place_of_worship'),
    Education_p = c('library', 'school', 'university', 'primary_school', 
                    'secondary_school'),
    Health_p = c('dentist', 'doctor', 'hospital', 'pharmacy', 
                 'physiotherapist', 'veterinary_care', 'health'),
    Civic_p = c('courthouse', 'lawyer', 'police', 'fire_station', 'city_hall',
                'embassy', 'local_government_office'),
    Business_p = c('accounting', 'atm', 'bank', 'post_office', 'hair_care',
                   'car_repair', 'car_wash', 'car_rental', 'electrician',
                   'locksmith', 'painter', 'real_estate_agency', 'plumber',
                   'moving_company', 'roofing_contractor', 'laundry', 
                   'insurance_agency', 'storage', 'travel_agency'),
    Unknown_p = c('administrative_area_level_1', 'administrative_area_level_2',
                  'administrative_area_level_3', 'administrative_area_level_4',
                  'administrative_area_level_5', 'archipelago', 
                  'colloquial_area', 'continent', 'country', 'establishment',
                  'point_of_interest', 'floor', 'general_contractor', 'geocode',
                  'locality', 'natural_feature', 'neighborhood', 'political',
                  'post_box', 'postal_code', 'postal_code_prefix', 
                  'postal_code_suffix', 'postal_town', 'premise', 'room',
                  'sublocality', 'sublocality_level_1', 'sublocality_level_2',
                  'sublocality_level_3', 'sublocality_level_4', 
                  'sublocality_level_5', 'town_square', 'subpremise')
)

poi_categorize <- function(places_list){
    
    # Input description ----
    # 
    # places_list: a list returned by function 'get_poi.R' for one location
    # 
    # Output ----
    # 
    # poi_categories: a data frame, each column represents the number of 
    # poi types defined in 'poi_types'
    # 
    # 
    # Function ----
    
    poi_categories <- data.frame(t(rep(0, length(poi_types))))
    colnames(poi_categories) <- poi_types
    
    if(!is.na(places_list[1,1])){
        for(i in 1:dim(places_list)[1]){
            for(j in 1:length(poi_types)){
                if(sum(places_list$types[[i]] %in% 
                       poi_places[[poi_types[j]]]) > 0) {
                    poi_categories[, poi_types[j]] <- 
                        poi_categories[, poi_types[j]] + 1
                }
            }
        }
    }
    
    return(poi_categories)
}

# load('3_output/28_Apr/purpose_zuerich_subpoi.RData')
# places_list <- purpose_zuerich_subpoi[[37]][1000][[1]]
# poi_categories <- poi_categorize(places_list)

# poi_categories <- as.data.frame(t(sapply(
#     purpose_zuerich_subpoi$poi[1:5], poi_categorize) ))
# 
# poi_categories[, 'Leisure_p'] <- as.data.frame(poi_categories[, 'Leisure_p'])




