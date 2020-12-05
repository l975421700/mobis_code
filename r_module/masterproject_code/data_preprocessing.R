
# Code to do data pre processing ------------------------------------------
# All subsequent sections depend on this section, following each section 
# should run separately but in that order.

library('readr')
suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('stringi'))
suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('VIM'))
suppressPackageStartupMessages(library('rgdal'))
library('sp') 
library(parallel)

source('2_code/r_module/install_packages.R', chdir = TRUE) 
source('2_code/r_module/wkb2lonlat.R', chdir = TRUE) 
source('2_code/r_module/utc2local_time.R', chdir = TRUE) 
source('2_code/r_module/hierarchical_clustering.R', chdir = TRUE) 
source('2_code/r_module/namelist.R', chdir = TRUE) 
source('2_code/r_module/get_poi.R', chdir = TRUE) 




# data 'activities' cleaning ----

# data.table will be extremely faster than tbl for large group analysis
# Firstly we can use readr to automatically identify column class
activities <- readr::read_delim('1_data/mobis/activities.csv', delim = ',')
activities <- as.data.table(activities)
colnames(activities)[which(colnames(activities) == "activity_id")] <- 'ID'

# remove sub activities
activities <- activities[which(nchar(activities$ID) == 8 ), ]

# transform trip purpose to our classification
# names(sort(table(activities$labels), decreasing = TRUE))
activities$labels <- sapply(activities$purpose, function(x) {
    if(x %in% c('unknown', "leisure", "shopping", "home", "work", "errand", 
                "assistance")){
        return(stringi::stri_trans_totitle(x))
    } else if(x == 'homeoffice'){
        return('Home')
    } else if(x == 'coworking'){
        return('Work')
    } else if(x == 'study'){
        return('Education')
    } else if(x == 'wait'){
        return('Other')
    }
})

# transform wkb geometry data into longitude+latitude
activities <- cbind(activities, wkb2lonlat(activities$geometry))

# transform UTC time to local time, and infer time info.
activities$started_at_local <- utc2local_time(activities$started_at, 
                                              activities$sf)
activities$finished_at_local <- utc2local_time(activities$finished_at, 
                                               activities$sf)

activities$starttime <- lubridate::hour(activities$started_at_local)
activities$endtime <- lubridate::hour(activities$finished_at_local)
activities$weekday <- lubridate::wday(activities$started_at_local, 
                                      week_start = 1)
activities$yday <- lubridate::yday(activities$started_at_local)
activities$inside_day <- ifelse(activities$yday == 
                              lubridate::yday(activities$finished_at_local), 
                              1, 0)
activities$duration_min <- round(activities$duration/60, 1)


################################ extract cluster based info

# conduct spatial hierarchical clustering

activities <- cbind(
    activities, activities[, h_clustering(.SD), by = 'user_id', 
                           .SDcols = c('lon', 'lat')][, -1]
)

# infer how many legs for each participants

activities <- data.table::merge.data.table(
    activities, activities[, .(activities_per_user = length(ID), 
                               surveyed_day = length(unique(yday))), 
                           by = 'user_id'], 
    by = c('user_id'), sort = FALSE
)

activities <- data.table::merge.data.table(
    activities, activities[, .(
        activities_per_cluster = length(ID),
        mean_duration = mean(duration_min), 
        mean_starttime = mean(starttime), 
        mean_endtime = mean(endtime),
        sd_duration = sd(duration_min), 
        sd_starttime = sd(starttime),
        sd_endtime = sd(endtime),
        weekday_freq = length(which(weekday < 6)) / length(weekday)),
        by = c('user_id', 'hc_cat')], 
    by = c('user_id', 'hc_cat'), sort = FALSE
)

activities$cluster_freq <- activities$activities_per_cluster / 
    activities$activities_per_user

activities$daily_cluster_freq <- activities$activities_per_cluster / 
    activities$surveyed_day

activities$daily_activities <- activities$activities_per_user / 
    activities$surveyed_day


################################ Handling missing value
# apply(activities, 2, function(x) length(which(is.na(x))))
activities[is.na(activities)] = 0


################################ Code categorical variables as factor

activities[, c('labels', 'weekday', 'inside_day')] <- 
    lapply(activities[,  c('labels', 'weekday', 'inside_day')], factor)

save(activities,file = '3_output/28_Apr/activities.RData')




# data 'participants' cleaning ----

load('3_output/28_Apr/activities.RData')
participants <- readr::read_delim('1_data/mobis/participants.csv', delim = ';', 
                                  guess_max = 100000, n_max = 100000)

# remove duplicate participants info
# sum(duplicated(participants$participant_ID))
participants <- participants %>% dplyr::group_by(participant_ID) %>% 
    filter(row_number()==1)

# remove participants without record: #95502 to #3689
participants <- participants[
    participants$participant_ID %in% activities$user_id, ]

# Re-code surveyed results
participants$income[which(participants$income == '99')] <- NA
participants$own_car[which(participants$own_car == 9)] <- 0
participants$own_motorbike[which(participants$own_motorbike == 9)] <- 0
participants$own_bike_ebike[which(participants$own_bike_ebike == 9)] <- 0

participants <- participants[, c('participant_ID', participants_features)]

participants[, participants_factorfeatures] <- lapply(
        participants[, participants_factorfeatures], factor) 

participants[, -1] <- VIM::kNN(participants[, -1], k = 20, imp_var = FALSE)

participants <- as.data.table(participants)

save(participants,file = '3_output/28_Apr/participants.RData')




# data 'legs' cleaning ----

legs <- readr::read_delim('1_data/mobis/legs.csv', delim = ',')
legs <- as.data.table(legs)
colnames(legs)[which(colnames(legs) == "trip_id")] <- 'ID'


# load('3_output/28_Apr/legs.RData')
# legs$labels1 <- legs$labels

# change unconfirmed trip mode to 'unknown'
legs$labels <- mapply(function(x, y) {
    if(y == FALSE){
        return('Unknown')
    } else if(x %in% c("Mode::Walk")){
        return('Walk')
    } else if(x %in% c("Mode::Car", "Mode::CarsharingMobility", 
                       "Mode::TaxiUber") ){
        return('Car')
    } else if(x %in% c("Mode::Bus")){
        return('Bus')
    } else if(x %in% c("Mode::Tram")){
        return('Tram')
    }else if(x %in% c("Mode::Bicycle", "Mode::MotorbikeScooter")){
        return('Bicycle')
    } else if(x %in% c("Mode::Subway", "Mode::LightRail", "Mode::Train", 
                       "Mode::RegionalTrain")){
        return('Train')
    } else if(x %in% c("Mode::Airplane", "Mode::Boat", 
                       "Mode::Ferry", "Mode::Aerialway")){
        return('Others')
    }
}, legs$mode, legs$was_confirmed)


# transform wkb geometry data into longitude+latitude

legs <- cbind(legs, wkb2lonlat(legs$start_point, suffix = 'start'))
legs <- cbind(legs, wkb2lonlat(legs$end_point, suffix = 'end'))


# transform UTC time to local time, and infer time info.
legs$started_at_local <- utc2local_time(legs$started_at, legs$start_sf)
legs$finished_at_local <- utc2local_time(legs$finished_at, legs$end_sf)

legs$starttime <- lubridate::hour(legs$started_at_local)
legs$endtime <- lubridate::hour(legs$finished_at_local)
legs$weekday <- lubridate::wday(legs$started_at_local, week_start = 1)
legs$yday <- lubridate::yday(legs$started_at_local)
legs$inside_day <- ifelse(lubridate::yday(legs$started_at_local) == 
                              lubridate::yday(legs$finished_at_local), 1, 0)
legs$duration_min <- round(legs$duration/60, 1)
legs$speed <- legs$length / legs$duration


################################ extract cluster based info

# conduct spatial hierarchical clustering for start and end location

legs <- cbind(
    legs, legs[, h_clustering(.SD, suffix = 'start'), by = 'user_id', 
               .SDcols = c('start_lon', 'start_lat')][, -1]
)

legs <- cbind(
    legs, legs[, h_clustering(.SD, suffix = 'end'), by = 'user_id', 
               .SDcols = c('end_lon', 'end_lat')][, -1]
)

# infer how many legs for each participants
legs <- data.table::merge.data.table(
    legs, legs[, .(legs_per_user = length(ID), 
                   surveyed_day = length(unique(yday))),
               by = 'user_id'], 
    by = c('user_id'), sort = FALSE
)

legs <- data.table::merge.data.table(
    legs, legs[, .(
        legs_per_startcluster = length(ID),
        mean_duration_startcluster = mean(duration_min), 
        mean_starttime_startcluster = mean(starttime),
        mean_endtime_startcluster = mean(endtime),
        mean_speed_startcluster = mean(speed), 
        sd_duration_startcluster = sd(duration_min), 
        sd_starttime_startcluster = sd(starttime),
        sd_endtime_startcluster = sd(endtime),
        sd_speed_startcluster = sd(speed), 
        weekday_freq = length(which(weekday < 6)) / length(weekday)),
        by = c('user_id', 'start_hc_cat')], 
    by = c('user_id', 'start_hc_cat'), sort = FALSE
)

legs <- data.table::merge.data.table(
    legs, legs[, .(
        legs_per_endcluster = length(ID),
        mean_duration_endcluster = mean(duration_min), 
        mean_starttime_endcluster = mean(starttime),
        mean_endtime_endcluster = mean(endtime),
        mean_speed_endcluster = mean(speed), 
        sd_duration_endcluster = sd(duration_min), 
        sd_starttime_endcluster = sd(starttime),
        sd_endtime_endcluster = sd(endtime),
        sd_speed_endcluster = sd(speed)),
        by = c('user_id', 'end_hc_cat')], 
    by = c('user_id', 'end_hc_cat'), sort = FALSE
)

# infer how many legs for each participants
legs <- data.table::merge.data.table(
    legs, legs[, .(total_duration = sum(duration_min)),
               by = 'next_activity_id'], 
    by = c('next_activity_id'), sort = FALSE
)

legs$startcluster_freq <- legs$legs_per_startcluster / legs$legs_per_user
legs$endcluster_freq <- legs$legs_per_endcluster / legs$legs_per_user

legs$daily_freq_start <- legs$legs_per_startcluster / legs$surveyed_day
legs$daily_freq_end <- legs$legs_per_endcluster / legs$surveyed_day

################################ Handling missing value
# apply(legs, 2, function(x) length(which(is.na(x))))
legs[is.na(legs)] = 0

################################ Code categorical variables as factor

legs[, c('labels', 'weekday', 'inside_day')] <- 
    lapply(legs[,  c('labels', 'weekday', 'inside_day')], factor)

save(legs, file = '3_output/28_Apr/legs.RData')




# Add GIS info ----

# Each code can be run separately in the following order

################################ extract spatial feature of activities to
# "SpatialPointsDataFrame" and save to .shp file

load('3_output/28_Apr/activities.RData')
activities_sf <- activities[, .(ID, lon, lat)]
sp::coordinates(activities_sf) <- ~ lon + lat
sp::proj4string(activities_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'

rgdal::writeOGR(activities_sf, dsn = '3_output/28_Apr/geo',
                layer = 'activities_sf', driver = "ESRI Shapefile")


################################ transform projections of current shapefile

swiss_kanton <- rgdal::readOGR(
    '1_data/geo_data/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp')
swiss_kanton <- sp::spTransform(
    swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )

rgdal::writeOGR(swiss_kanton, dsn = '3_output/28_Apr/geo',
                layer = 'swiss_kanton', driver = "ESRI Shapefile")

swiss_bauzonen <- rgdal::readOGR(
    '1_data/geo_data/swiss/landuse/ch_are_bauzonen.shp')
swiss_bauzonen <- sp::spTransform(
    swiss_bauzonen, CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )
rgdal::writeOGR(swiss_bauzonen, dsn = '3_output/28_Apr/geo',
                layer = 'swiss_bauzonen', driver = "ESRI Shapefile")


################################ Infer in which kanton is the activities

load('3_output/28_Apr/activities.RData')
activities_sf <- rgdal::readOGR('3_output/28_Apr/geo/activities_sf.shp')
swiss_kanton <- rgdal::readOGR('3_output/28_Apr/geo/swiss_kanton.shp')

# infer in which kanton the activities occur 
point_polygon <- sp::over(activities_sf, swiss_kanton)
activities$inkanton <- as.character(point_polygon$NAME)

save(activities,file = '3_output/28_Apr/activities.RData')


################################ Infer in which building zone the activities is.

load('3_output/28_Apr/activities.RData')

# Apply spatial join function in ARCGIS to 'activities_sf' and 'swiss_bauzonen'
# we get 'activities_sf_join':
activities_sf_join <- rgdal::readOGR(
    '3_output/28_Apr/geo/activities_sf_join.shp')

building_zone <- as.data.table(
    activities_sf_join@data[, c('actvty_', 'CH_BEZ_D')])
building_zone <- unique(building_zone, by = 'actvty_')

activities <- data.table::merge.data.table(
    activities, building_zone, by.x = 'ID', by.y = 'actvty_', all.x = TRUE)


activities$building_zone <- mapply(function(x, y) {
    if(is.na(x) & y == FALSE){
        return('Foreign')
    }else if(is.na(x) & y == TRUE){
        return('Else')
    }else if(x == "Arbeitszonen"){
        return("Work")
    }else if(x == "eingeschr鋘kte Bauzonen"){
        return('Controlled')
    }else if(x == "Mischzonen"){
        return('Mixed')
    }else if(x == "Tourismus- und Freizeitzonen"){
        return('Leisure')
    }else if(x == "Verkehrszonen innerhalb der Bauzonen"){
        return('Transport')
    }else if (x == "weitere Bauzonen"){
        return('Other')
    }else if (x == "Wohnzonen"){
        return('Residential')
    }else if (x == "Zentrumszonen"){
        return('Center')
    }else if (x == "Zonen f黵 鰂fentliche Nutzungen"){
        return('Public_usage')
    }
}, activities$CH_BEZ_D, activities$in_switzerland)

activities$building_zone <- factor(activities$building_zone)

save(activities, file = '3_output/28_Apr/activities.RData')




# Extract features for trip purpose imputation ----

load('3_output/28_Apr/activities.RData')
load('3_output/28_Apr/participants.RData')

purpose_data <- data.table::merge.data.table(
    activities, participants, 
    by.x = "user_id", by.y = "participant_ID", all.x = TRUE
)

purpose_data <- purpose_data[
    , .SD , .SDcols = c("user_id", "ID", "labels", "inkanton", 
                        activities_features, participants_features, 
                        activities_clusterfeatures)
    ]

save(purpose_data, file = '3_output/28_Apr/purpose_data.RData')




# Extract features for trip mode imputation ----

load('3_output/28_Apr/legs.RData')
load('3_output/28_Apr/participants.RData')

mode_data <- data.table::merge.data.table(
    legs, participants, 
    by.x = "user_id", by.y = "participant_ID"
)

mode_data <- mode_data[
    , .SD , .SDcols = c("user_id", "ID", "labels", 
                        "next_activity_id", "total_duration",
                        participants_features, legs_features, 
                        legs_clusterfeatures)
    ]

save(mode_data, file = '3_output/28_Apr/mode_data.RData')


# Extract 10000 activities in Zurich for POI pre analysis----

load('3_output/28_Apr/purpose_data.RData')
load('3_output/28_Apr/activities.RData')

purpose_zuerich <- droplevels(purpose_data[
    which(purpose_data$labels != 'Unknown' & purpose_data$inkanton == "Zürich"), ]
)

purpose_zuerich_sub <- purpose_zuerich[
    which(purpose_zuerich$user_id %in% 
              names(sort(table(purpose_zuerich$user_id), 
                         decreasing = TRUE)[1:10] )), ]

purpose_zuerich_sub <- data.table::merge.data.table(
    purpose_zuerich_sub, activities[, .SD, .SDcol = c("ID", "lon", "lat")], 
    by.x = "ID", by.y = "ID", all.x = TRUE
)

save(purpose_zuerich_sub, file = '3_output/28_Apr/purpose_zuerich_sub.RData')

# Extract users with most labeled activities ----

#### Extract (user, day), where all activities are labelled

load('3_output/28_Apr/activities.RData')

user_alllabelled_days <- activities[
    , .(Unlabelled_activities = length(which(labels == 'Unknown'))), 
    by = .(user_id, yday)]

user_alllabelled_days <- user_alllabelled_days[
    which(user_alllabelled_days$Unlabelled_activities == 0),
]

# length(which(table(user_alllabelled_days$user_id) >= 30))

save(user_alllabelled_days, file = '3_output/28_Apr/user_alllabelled_days.RData')

#### Extract (user, day, activities), where all activities are labelled

load('3_output/28_Apr/user_alllabelled_days.RData')
load('3_output/28_Apr/purpose_data.RData')
load('3_output/28_Apr/activities.RData')

alllabelled_activities <- data.table::merge.data.table(
    activities, user_alllabelled_days, by = c("user_id", "yday")
)

alllabelled_purpose <- data.table::merge.data.table(
    purpose_data, activities[, .SD, .SDcols = c('ID', 'yday')], by = 'ID'
)

alllabelled_purpose <- alllabelled_purpose[
    alllabelled_purpose$ID %in% alllabelled_activities$ID, ]

save(alllabelled_purpose, file = '3_output/28_Apr/alllabelled_purpose.RData')

# POI information through Google places api ----

# use ten persons' data, each generates around 1000 activities in Zuerich,
# investigate benefits of POI info from google places API

load('3_output/28_Apr/purpose_zuerich_sub.RData')
purpose_zuerich_subpoi <- dplyr::as.tbl(purpose_zuerich_sub)

purpose_zuerich_subpoi$poi <- NA

poi_50 <- parallel::mcmapply(function(var1, var2){
    get_poi(latitude = var1, longitude = var2, radius = 50, 
            api_key = api_key)},
    purpose_zuerich_subpoi$lat, purpose_zuerich_subpoi$lon, 
    SIMPLIFY = FALSE, mc.cores = 2)
purpose_zuerich_subpoi$poi <- poi_50

save(purpose_zuerich_subpoi, 
     file = '3_output/28_Apr/purpose_zuerich_subpoi.RData')




# PCA for feature reduction in purpose data ----

load('3_output/28_Apr/purpose_data.RData')

dim(purpose_data[, which(sapply(purpose_data, class) %in% 
                             c("numeric", "integer"))])

pca_purpose_data <- prcomp(purpose_data[
    , which(sapply(purpose_data, class) %in% c("numeric", "integer"))], 
    scale. = TRUE)

purpose_data_pca <- cbind(purpose_data, pca_purpose_data$x)

save(purpose_data_pca, file = '3_output/28_Apr/purpose_data_pca.RData')

