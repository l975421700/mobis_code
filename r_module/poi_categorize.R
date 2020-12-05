

# Function to categorize OpenStreetMap poi from geofabrik -----------------


osmpoi_category <- c(
    'public_p', 'health_p', 'leisure_p', 'catering_p', 'accommodation_p',
    'shopping_p', 'money_p', 'tourism_p', 'miscpoi_p')

osmpoi_details <- list(
    public_p = c(
        'police', 'fire_station', 'post_box', 'post_office', 'telephone',
        'library', 'town_hall', 'courthouse', 'prison', 'embassy',
        'community_centre', 'nursing_home', 'arts_centre', 'graveyard',
        'market_place', 'recycling', 'recycling_glass', 'recycling_paper',
        'recycling_clothes', 'recycling_metal', 'university', 'school',
        'kindergarten', 'college', 'public_building'
    ), 
    health_p = c('pharmacy', 'hospital', 'doctors', 'dentist', 'veterinary'), 
    leisure_p = c(
        'theatre', 'nightclub', 'cinema', 'park', 'playground', 'dog_park',
        'sports_centre', 'pitch', 'swimming_pool', 'tennis_court', 
        'golf_course', 'stadium', 'ice_rink', 'track'
    ), 
    catering_p = c(
        'restaurant', 'fast_food', 'cafe', 'pub', 'bar', 'food_court',
        'biergarten'
    ), 
    accommodation_p = c(
        'hotel', 'motel', 'bed_and_breakfast', 'guesthouse', 'hostel', 
        'chalet', 'shelter', 'camp_site', 'alpine_hut', 'caravan_site'
    ), 
    shopping_p = c(
        'supermarket', 'bakery', 'kiosk', 'mall', 'department_store',
        'convenience', 'clothes', 'florist', 'chemist', 'bookshop', 'butcher',
        'shoe_shop', 'beverages', 'optician', 'jeweller', 'gift_shop',
        'sports_shop', 'stationery', 'outdoor_shop', 'mobile_phone_shop',
        'toy_shop', 'newsagent', 'greengrocer', 'beauty_shop', 'video_shop',
        'car_dealership', 'bicycle_shop', 'doityourself', 'furniture_shop',
        'computer_shop', 'garden_centre', 'hairdresser', 'car_repair',
        'car_rental', 'car_wash', 'car_sharing', 'bicycle_rental',
        'travel_agent', 'laundry', 'vending_machine', 'vending_cigarette',
        'vending_parking', 'vending_any'
    ), 
    money_p = c('bank', 'atm'), 
    tourism_p = c(
        'tourist_info', 'tourist_map', 'tourist_board','tourist_guidepost',
        'attraction', 'museum', 'monument', 'memorial', 'art', 'castle', 
        'ruins', 'archaeological', 'wayside_cross', 'wayside_shrine',
        'battlefield', 'fort', 'picnic_site', 'viewpoint', 'zoo', 
        'theme_park', 'artwork'
    ), 
    miscpoi_p = c(
        'toilet', 'bench', 'drinking_water', 'fountain', 'hunting_stand',
        'waste_basket', 'camera_surveillance', 'emergency_phone', 
        'fire_hydrant', 'emergency_access', 'tower', 'comms_tower',
        'water_tower', 'observation_tower', 'windmill', 'lighthouse',
        'wastewater_plant', 'water_well', 'water_mill', 'water_works'
    )
)


points_in_box <- function(coords, bbox){
    # Input description ----
    # 
    # coords: 2-column dataframe: [longitude, latitude]
    # 
    # bbox: bbox: [left, bottom, right, top]
    # 
    # Output ----
    # 
    # icontained: a vector of TRUE or FALSE indicate if the points are 
    # contained within the box or not
    # 
    # 
    # Function ----
    
    icontained <- (coords[, 1] >= bbox[1]) & (coords[, 1] <= bbox[3]) & 
        (coords[, 2] >= bbox[2]) & (coords[, 2] <= bbox[4])
    
    return(icontained)
}

nearest_poi_dis <- function(
    lon, lat, osm_poi_shp, distance = 400){
    # Input description ----
    # 
    # lon: longitude of the points
    # 
    # lat: latitude of the points
    # 
    # osm_poi_shp: a poi shapefile from OSM created by geofabrik, 
    # !!!with @coords
    # 
    # distance: used in osmar::center_bbox to create a bbox to search for POI
    # 
    # 
    # Output ----
    # 
    # nearest_dis
    # 
    # 
    # Function ----
    
    # Find the points in the bbox
    poi_coords <- osm_poi_shp[
        points_in_box(
            coords = osm_poi_shp, 
            bbox = osmar::center_bbox(
                lon, lat, distance, distance
            )
        ), 
    ]
    
    # calculate the smallest distance to POI
    if(length(poi_coords) > 0){
        poi_dis <- geosphere::distGeo(
            p1 = c(lon, lat), 
            p2 = poi_coords
        )
        nearest_poi <- min(poi_dis)
    }else{
        nearest_poi <- NA
    }
    
    return(nearest_poi)
} 

# osm_poi_shp <- osm_poi_swiss
# distance <- 100
# lon <- all_activities$lon[1]; lat <- all_activities$lat[1]
# lon <- 8.518702; lat <- 47.381040
# lon <- 8.540340; lat <- 47.378034

# nearest_poi_dis(
#     lon = 8.540340,
#     lat = 47.378034, 
#     osm_poi_shp = osm_poi_swiss
# )

# near_poi <- osm_poi_shp[
#     points_in_box(
#         coords = osm_poi_shp@coords, 
#         bbox = osmar::center_bbox(
#             lon, lat, distance, distance
#         )
#     ), 
# ]


load('3_output/all_activities.RData')
osm_poi_swiss <- rgdal::readOGR(
    '3_output/geo/switzerland-latest-free.shp/gis_osm_pois_free_1.shp')
osm_poia_swiss <- rgdal::readOGR(
    '3_output/geo/switzerland-latest-free.shp/gis_osm_pois_a_free_1.shp')


# ddd <- sp::coordinates(osm_poia_swiss)
# ccc <- osm_poi_swiss@coords
# eee <- rbind(osm_poi_swiss@coords, sp::coordinates(osm_poia_swiss))


swiss_poi <- rbind(osm_poi_swiss@coords, sp::coordinates(osm_poia_swiss))


system.time(
nearest_poi <- mapply(
    function(var1, var2){
        nearest_poi_dis(
            lon = var1, lat = var2, 
            osm_poi_shp = swiss_poi
        )
    },
    all_activities$lon[1:100], all_activities$lat[1:100])
)

system.time(
    nearest_poi <- mapply(
        function(var1, var2){
            nearest_poi_dis(
                lon = var1, lat = var2, 
                osm_poi_shp = osm_poi_swiss@coords
            )
        },
        all_activities$lon[1:100], all_activities$lat[1:100])
)

for(i in 1:10){
    # i <- 1
    lon = all_activities$lon[i]
    lat = all_activities$lat[i]
    print(paste(i, nearest_poi_dis(lon, lat, osm_poi_shp = osm_poi_swiss)))
}















# Function to categorize poi returned by 'get_poi' from Google -----------

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




