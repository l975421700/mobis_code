

# This code is used to do automatic trip mode imputation
# 
# It can be run based on original data on Rserver or local data in this folder

i_rserver = 0 # If it need to be run on R server, set this value to 1
i_rworkspace = 0 # If load workspace from Mr. Molly, set this value to 1
n_max = Inf # 10000 for test, Inf for all

# Input packages, functions, and namelist for all subsequent sections ----

library('readr')
suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('stringi'))
suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('VIM'))
suppressPackageStartupMessages(library('rgdal'))
suppressPackageStartupMessages(library('caret'))
library('DBI')
suppressPackageStartupMessages(library('sf'))
library('RPostgreSQL')
library('mapview')
library('sp') 
library('parallel')

source('mobis_code/r_module/install_packages.R', chdir = TRUE) 
source('mobis_code/r_module/wkb2lonlat.R', chdir = TRUE) 
source('mobis_code/r_module/utc2local_time.R', chdir = TRUE) 
source('mobis_code/r_module/hierarchical_clustering.R', chdir = TRUE) 
source('mobis_code/r_module/namelist.R', chdir = TRUE) 
source('mobis_code/r_module/stepwise_classification.R', chdir = TRUE) 
source('mobis_code/r_module/interpoints_speed.R', chdir = TRUE) 
source('mobis_code/r_module/adasyn.R', chdir = TRUE) 
source('mobis_code/r_module/multi_classification_kfold_cv.R', chdir = TRUE) 


# Download swiss OSM poi shapefile
# download.file(
#     "https://download.geofabrik.de/europe/switzerland-latest-free.shp.zip",
#     '3_output/switzerland-latest-free.shp.zip')
# utils::unzip(
#     '3_output/switzerland-latest-free.shp.zip',
#     exdir = '3_output/geo/switzerland-latest-free.shp')
# file.remove('3_output/switzerland-latest-free.shp.zip')


# data 'legs' cleaning ----

load('3_output/participants.RData')

# data.table will be extremely faster than tbl for large group analysis
# Firstly we can use readr to automatically identify column class
if(i_rserver == 0){
    if(i_rworkspace == 1){
        load('1_data/mobis/mobis_r_workspace.RData')
    }else if(i_rworkspace == 0){
        legs <- readr::read_delim(
            '1_data/mobis/csv/legs.csv', delim = ',', n_max = n_max)
        covid_legs <- readr::read_delim(
            '1_data/mobis/covid/covid_legs.csv', delim = ',', n_max = n_max)
    }
}else {
    if(i_rworkspace == 1){
        load('/data/mobis/data/mobis_r_workspace.RData')
    }else if(i_rworkspace == 0){
        legs <- readr::read_delim(
            '/data/mobis/data/csv/legs.csv', delim = ',', n_max = n_max)
        covid_legs <- readr::read_delim(
            '/data/mobis/data/covid/covid_legs.csv', delim = ',', n_max = n_max)
    }
}


legs$i_covid <- 0
covid_legs$i_covid <- 1

# all_legs <- legs
all_legs <- rbind(legs, covid_legs)
rm(legs, covid_legs)

all_legs <- as.data.table(all_legs)
colnames(all_legs)[colnames(all_legs) == "trip_id"] <- 'ID'

# change unconfirmed trip mode to 'unknown'
all_legs$labels <- mapply(function(x, y) {
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
}, all_legs$mode, all_legs$was_confirmed)

# transform wkb geometry data into longitude+latitude
all_legs <- cbind(all_legs, wkb2lonlat(all_legs$start_point, suffix = 'start'))
all_legs <- cbind(all_legs, wkb2lonlat(all_legs$end_point, suffix = 'end'))

# transform UTC time to local time, and infer time info.
all_legs$started_at_local <- utc2local_time(
    all_legs$started_at, all_legs$start_sf)
all_legs$finished_at_local <- utc2local_time(
    all_legs$finished_at, all_legs$end_sf)

all_legs$starttime <- lubridate::hour(all_legs$started_at_local)
all_legs$endtime <- lubridate::hour(all_legs$finished_at_local)
all_legs$weekday <- lubridate::wday(all_legs$started_at_local, week_start = 1)
all_legs$yday <- lubridate::yday(all_legs$started_at_local)
all_legs$inside_day <- ifelse(
    lubridate::yday(all_legs$started_at_local) == 
        lubridate::yday(all_legs$finished_at_local), 1, 0)
all_legs$duration_min <- round(all_legs$duration/60, 1)
all_legs$speed <- all_legs$length / all_legs$duration

# conduct spatial hierarchical clustering for start and end location

all_legs <- data.table::merge.data.table(
    all_legs, all_legs[, h_clustering(.SD, suffix = 'start'), by = 'user_id', 
                       .SDcols = c('ID', 'start_lon', 'start_lat')][, -1], 
    by.x = 'ID', by.y = 'start_ID', sort = FALSE
)


all_legs <- data.table::merge.data.table(
    all_legs, all_legs[, h_clustering(.SD, suffix = 'end'), by = 'user_id', 
                       .SDcols = c('ID', 'end_lon', 'end_lat')][, -1], 
    by.x = 'ID', by.y = 'end_ID', sort = FALSE
)

# infer how many legs for each participants
all_legs <- data.table::merge.data.table(
    all_legs, all_legs[, .(legs_per_user = length(ID), 
                   surveyed_day = length(unique(yday))),
               by = 'user_id'], 
    by = c('user_id'), sort = FALSE
)

all_legs <- data.table::merge.data.table(
    all_legs, all_legs[, .(
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

all_legs <- data.table::merge.data.table(
    all_legs, all_legs[, .(
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

# infer how many all_legs for each participants
all_legs <- data.table::merge.data.table(
    all_legs, all_legs[, .(total_duration = sum(duration_min)),
               by = 'next_activity_id'], 
    by = c('next_activity_id'), sort = FALSE
)

all_legs$startcluster_freq <- all_legs$legs_per_startcluster / 
    all_legs$legs_per_user
all_legs$endcluster_freq <- all_legs$legs_per_endcluster / 
    all_legs$legs_per_user

all_legs$daily_freq_start <- all_legs$legs_per_startcluster / 
    all_legs$surveyed_day
all_legs$daily_freq_end <- all_legs$legs_per_endcluster / all_legs$surveyed_day

# Handling missing value
# apply(all_legs, 2, function(x) length(which(is.na(x))))
all_legs[is.na(all_legs)] = 0

# Code categorical variables as factor
all_legs[, c('labels', 'weekday', 'inside_day')] <- 
    lapply(all_legs[,  c('labels', 'weekday', 'inside_day')], factor)

all_legs <- data.table::merge.data.table(
    all_legs, participants, 
    by.x = "user_id", by.y = "participant_ID"
)

# calculate distance to home
all_legs$startpoint_dist2home <- geosphere::distGeo(
    p1 = all_legs[, .(start_lon, start_lat)],
    p2 = all_legs[, .(home_lon, home_lat)]
)
all_legs$endpoint_dist2home <- geosphere::distGeo(
    p1 = all_legs[, .(end_lon, end_lat)],
    p2 = all_legs[, .(home_lon, home_lat)]
)

save(all_legs, file = '3_output/all_legs.RData')


# unrealistic data cleaning
load('3_output/all_legs.RData')
all_legs$euclidean_length <- geosphere::distGeo(
    p1 = all_legs[, .(start_lon, start_lat)],
    p2 = all_legs[, .(end_lon, end_lat)]
)

all_legs$trip_duration <- as.numeric(difftime(
    all_legs$finished_at,
    all_legs$started_at,
    units = 'secs'
))

all_legs$euclidean_speed <- all_legs$euclidean_length / all_legs$trip_duration

# summary(all_legs$speed)
# summary(all_legs$euclidean_speed)
# length(which(all_legs$euclidean_speed > 100))
# all_legs[euclidean_speed > 110]
# hist(all_legs$euclidean_speed[all_legs$euclidean_speed > 40])
# hist(all_legs$trip_duration[all_legs$euclidean_speed > 40])

all_legs$shorttrip <- ifelse(all_legs$duration < 60, 1, 0)
# table(all_legs$shorttrip)

all_legs$mislabeled <- 0
all_legs$mislabeled[which(all_legs$shorttrip == 0 &
                        all_legs$labels == 'Walk' & 
                        all_legs$euclidean_speed > 3)] <- 1

all_legs$mislabeled[which(all_legs$shorttrip == 0 &
                              all_legs$labels == 'Walk' & 
                              all_legs$duration_min > 120)] <- 1

all_legs$mislabeled[which(all_legs$shorttrip == 0 &
                              all_legs$labels == 'Car' & 
                              all_legs$duration_min < 2)] <- 1

all_legs$mislabeled[which(all_legs$shorttrip == 0 &
                              all_legs$labels == 'Train' & 
                              all_legs$duration_min < 2)] <- 1

all_legs$mislabeled[which(all_legs$shorttrip == 0 &
                              all_legs$labels == 'Train' & 
                              all_legs$euclidean_speed < 2)] <- 1

all_legs$mislabeled[which(all_legs$shorttrip == 0 &
                              all_legs$mode == 'Mode::Airplane' &
                              all_legs$was_confirmed == TRUE &
                              all_legs$duration_min < 20)] <- 1

all_legs$mislabeled[which(all_legs$shorttrip == 0 &
                              all_legs$mode == 'Mode::Airplane' &
                              all_legs$was_confirmed == TRUE &
                              all_legs$euclidean_speed < 20)] <- 1

save(all_legs, file = '3_output/all_legs.RData')




# Interpoints speed calculation ----

load('3_output/all_legs.RData')

# Connection to SQL database
dbname = "mobis_study"
host = "id-hdb-psgr-cp50.ethz.ch"
user = "mobis_i"
password = "mobis_i"
conn <- dbConnect(PostgreSQL(), dbname = dbname, host=host, 
                  user=user, password=password)

for(i in 1 : ceiling(length(all_legs$ID) / 1000)){
    if(i == 1){
        trip_info <- NULL
    }
    
    trip_waypoints <- sf::st_read(
        conn, 
        query = sprintf(
            "select * from motion_tag_waypoint where trip_id IN (%s)", 
            paste(paste(
                '\'', 
                all_legs$ID[((i-1)*1000 + 1 ) : 
                                     min(length(all_legs$ID), i * 1000)], 
                '\'', sep = ''), collapse=", "
            )
        )
    )
    trip_waypoints <- as.data.table(trip_waypoints)
    
    if(dim(trip_waypoints)[1] > 0){
        trip_waypoints <- trip_waypoints[
            trip_waypoints$trip_id %in% 
                names(which(table(trip_waypoints$trip_id) > 1)),
            ]
        trip_info <- rbind(
            trip_info, 
            trip_waypoints[
                , interpoints_speed(.SD), by = 'trip_id', 
                .SDcols = c('tracked_at', 'longitude', 'latitude')])
    }
    
    print(paste(i, ceiling(length(all_legs$ID) / 1000),  Sys.time(), sep = '-'))
    
    if(i %% 50 == 0 | i == ceiling(length(all_legs$ID) / 1000)){
        save(trip_info, file = '3_output/trip_info.RData')
        Sys.sleep(2)
    }
    rm(trip_waypoints)
}

# load('3_output/trip_info.RData')


# trip_mode_imputation with interpoints speed and acceleration ------------

load('3_output/all_legs.RData')
load('3_output/trip_info.RData')

# length(all_legs$ID[all_legs$yday > 244 & 
#                        all_legs$shorttrip == 0 &
#                        all_legs$mislabeled == 0])
# sum(all_legs$ID[all_legs$yday > 244 & all_legs$shorttrip == 0 &
#                     all_legs$mislabeled == 0] %in% trip_info$trip_id)


labeled_mode <- droplevels(all_legs[labels != 'Unknown'])
labeled_mode$labels <- factor(labeled_mode$labels, 
                              levels = mode_categories)
labeled_mode$ID <- as.character(labeled_mode$ID)

# sum(labeled_mode$ID %in% trip_info$trip_id)
# sum(trip_info$trip_id %in% labeled_mode$ID)

labeled_mode <- data.table::merge.data.table(
    labeled_mode, 
    trip_info, 
    by.x = 'ID', by.y = 'trip_id', all.x = TRUE
)

save(labeled_mode, file = '3_output/labeled_mode.RData')
load('3_output/labeled_mode.RData')

clf_tm_fs <- list(
    original = 0, 
    cleaned = 0,
    add_personal = 0, 
    add_trippoints = 0, 
    add_personal_trippoints = 0
)

clf_tm_fs$original <- randomForest::randomForest(
    # 1589301 cases
    x = labeled_mode[, .SD, .SDcol = tm_sf_all],
    y = labeled_mode$labels, 
    ntree = 100, 
    mtry = ceiling(log2(length(tm_sf_all))+1 ), 
    importance = FALSE, 
    do.trace = 1, 
    keep.forest = FALSE
)

clf_tm_fs$cleaned <- randomForest::randomForest(
    # 1430270 cases
    x = labeled_mode[shorttrip == 0 & mislabeled == 0, .SD, .SDcol = tm_sf_all],
    y = labeled_mode$labels[which(labeled_mode$shorttrip == 0 & 
                                      labeled_mode$mislabeled == 0)], 
    ntree = 100, 
    mtry = ceiling(log2(length(tm_sf_all))+1 ), 
    importance = FALSE, 
    do.trace = 1, 
    keep.forest = FALSE
)

clf_tm_fs$cleaned_trippoints <- randomForest::randomForest(
    x = labeled_mode[shorttrip == 0 & mislabeled == 0 & 
                         !is.na(labeled_mode$`speed_50%`), 
                     .SD, .SDcol = tm_sf_all],
    y = labeled_mode$labels[which(labeled_mode$shorttrip == 0 & 
                                      labeled_mode$mislabeled == 0 & 
                                      !is.na(labeled_mode$`speed_50%`))], 
    ntree = 100, 
    mtry = ceiling(log2(length(tm_sf_all))+1 ), 
    importance = FALSE, 
    do.trace = 1, 
    keep.forest = FALSE
)




save(clf_tm_fs, file = '3_output/clf_tm_fs.RData')



clf_mode_pointsinfo$pointsinfo <- randomForest::randomForest(
    x = labeled_mode_points_info[, .SD, .SDcol = c(tm_sf_all, legs_pointsinfo)],
    y = labeled_mode_points_info$labels,
    ntree = 100, 
    mtry = ceiling(log2(length(c(tm_sf_all, legs_pointsinfo)))+1 ), 
    importance = TRUE, 
    do.trace = 1, 
    keep.forest = TRUE
)

save(clf_mode_pointsinfo,file = '3_output/clf_mode_pointsinfo.RData')


# Initial trip mode imputation ----

# load('3_output/all_legs.RData')

labeled_mode <- droplevels(all_legs[all_legs$labels != 'Unknown', ])
labeled_mode$labels <- factor(labeled_mode$labels, 
                              levels = mode_categories)

mode_rf_res <- randomForest::randomForest(
    x = labeled_mode[, .SD, .SDcol = tm_sf_all],
    y = labeled_mode$labels,
    ntree = 100, mtry = ceiling(log2(length(tm_sf_all))+1 ) , 
    importance = TRUE, do.trace = 1, 
    keep.forest = TRUE
)


all_legs$predicted_purpose <- predict(
    mode_rf_res, 
    all_legs[, .SD, .SDcols = tm_sf_all]
)

all_legs$predicted_purpose_ori <- all_legs$predicted_purpose

all_legs$predicted_purpose_ori[all_legs$labels != 'Unknown'] <- 
    droplevels(all_legs$labels[all_legs$labels != 'Unknown'])


save(all_legs,file = '3_output/all_legs.RData')
save(labeled_mode, file = '3_output/labeled_mode.RData')
save(mode_rf_res, file = '3_output/mode_rf_res.RData')




# trip mode imputation - Ensemble filter ----

load('3_output/labeled_mode.RData')

# Currently the model lacks power for across participantsimputation
labeled_mode <- labeled_mode[order(user_id)]
# labeled_mode <- labeled_mode[1:10000, ]

clf_res_labeled_mode <- multi_clf_kfold(
    x = labeled_mode[, .SD, .SDcol = tm_sf_all],
    y = labeled_mode$labels,
    ID = labeled_mode$ID,
    k = 2,
    par_rf = list(
        ntree = 100, importance = FALSE, do.trace = 1, keep.forest = FALSE)
)
clf_res_labeled_mode$prediction$all_predicted <- 
    clf_res_labeled_mode$prediction$rf_predicted + 
    clf_res_labeled_mode$prediction$c50_predicted + 
    clf_res_labeled_mode$prediction$mars_predicted
save(clf_res_labeled_mode, file = '3_output/clf_res_labeled_mode.RData')
# load('3_output/clf_res_labeled_mode.RData')

load('3_output/all_legs.RData')

# dim(all_legs)
# dim(clf_res_labeled_mode$prediction[, c("ID", "all_predicted")])
all_legs <- data.table::merge.data.table(
    all_legs, 
    clf_res_labeled_mode$prediction[, c("ID", "all_predicted")], 
    by = 'ID',
    all.x = TRUE, 
    sort = FALSE
)

system.time(
    clf_all_modes <- multi_clf_kfold(
        x = all_legs[all_predicted > 0, .SD, .SDcol = tm_sf_all],
        y = droplevels(all_legs$labels[
            which(all_legs$all_predicted > 0)]),
        ID = all_legs[all_predicted > 0, .SD, .SDcol = 'ID'],
        k = 1,
        multi_algorithms = list(
            rf = TRUE, c50 = TRUE, nb = FALSE, mars = TRUE),
        # all_predicted == 0 | is.na(all_predicted)
        x_predictors = all_legs[, .SD, .SDcol = tm_sf_all],
        ID_predict = all_legs[, .SD, .SDcol = 'ID']
    )
)
save(clf_all_modes, file = '3_output/clf_all_modes.RData')






filtered_mode <- labeled_mode[
    ! ID %in% clf_res_labeled_mode$prediction$ID[
        clf_res_labeled_mode$prediction$all_predicted == 0]][
            order(user_id)]
clf_res_filtered_mode <- multi_clf_kfold(
    x = filtered_mode[, .SD, .SDcol = tm_sf_all],
    y = filtered_mode$labels,
    ID = filtered_mode$ID,
    k = 2, 
    par_rf = list(
        ntree = 100, importance = FALSE, do.trace = 1, keep.forest = FALSE)
)
save(clf_res_filtered_mode, file = '3_output/clf_res_filtered_mode.RData')
# load('3_output/clf_res_filtered_mode.RData')

clf_res_labeled_mode$accuracy
clf_res_filtered_mode$accuracy
caret::confusionMatrix(
    table(
        factor(clf_res_labeled_mode$prediction$labels,
               levels = mode_categories),
        factor(clf_res_labeled_mode$prediction$rf_prediction,
               levels = mode_categories)
    )
)
caret::confusionMatrix(
    table(
        factor(clf_res_filtered_mode$prediction$labels,
               levels = mode_categories),
        factor(clf_res_filtered_mode$prediction$rf_prediction,
               levels = mode_categories)
    )
)





