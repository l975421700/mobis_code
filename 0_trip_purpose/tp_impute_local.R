
# This code is used to do automatic trip purpose imputation
# 
# It can be run based on original data on Rserver or local data in this folder

i_rserver = 1 # If it need to be run on R server, set this value to 1
i_rworkspace = 0 # If load workspace from Mr. Molly directly
n_max = Inf # 10000 for test, Inf for all

# Input packages, functions, and namelist for all subsequent sections ----
setwd("~/")
library('readr')
suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('stringi'))
suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('VIM'))
suppressPackageStartupMessages(library('rgdal'))
suppressPackageStartupMessages(library('caret'))
library('sp') 
library('parallel')

source('mobis_code/r_module/install_packages.R', chdir = TRUE) 
source('mobis_code/r_module/wkb2lonlat.R', chdir = TRUE) 
source('mobis_code/r_module/utc2local_time.R', chdir = TRUE) 
source('mobis_code/r_module/hierarchical_clustering.R', chdir = TRUE) 
source('mobis_code/r_module/namelist.R', chdir = TRUE) 
source('mobis_code/r_module/multi_classification_kfold_cv.R', chdir = TRUE) 


# Download swiss OSM poi shapefile
# download.file(
#     "https://download.geofabrik.de/europe/switzerland-latest-free.shp.zip",
#     '3_output/switzerland-latest-free.shp.zip')
# utils::unzip(
#     '3_output/switzerland-latest-free.shp.zip',
#     exdir = '3_output/geo/switzerland-latest-free.shp')
# file.remove('3_output/switzerland-latest-free.shp.zip')


# data 'activities' cleaning ----

# data.table will be extremely faster than tbl for large group analysis
# Firstly we can use readr to automatically identify column class

if(i_rserver == 0){
    if(i_rworkspace == 1){
        load('1_data/mobis/mobis_r_workspace.RData')
    }else if(i_rworkspace == 0){
        activities <- readr::read_delim(
            '1_data/mobis/csv/activities.csv', delim = ',', n_max = n_max)
        covid_activities <- readr::read_delim(
            '1_data/mobis/covid/covid_activities.csv', 
            delim = ',', n_max = n_max)
    }
}else {
    if(i_rworkspace == 1){
        load('/data/mobis/data/mobis_r_workspace.RData')
    }else if(i_rworkspace == 0){
        activities <- readr::read_delim(
            '/data/mobis/data/csv/activities.csv', delim = ',', n_max = n_max)
        covid_activities <- readr::read_delim(
            '/data/mobis/data/covid/covid_activities.csv', delim = ',', 
            n_max = n_max)
    }
}

activities$i_covid <- 0
covid_activities$i_covid <- 1

# all_activities <- activities
all_activities <- rbind(activities, covid_activities)
rm(activities, covid_activities)

all_activities <- as.data.table(all_activities)
colnames(all_activities)[colnames(all_activities) == "activity_id"] <- 'ID'


# transform trip purpose to our classification
all_activities$labels <- sapply(all_activities$purpose, function(x) {
    if(x %in% c('unknown', "leisure", "shopping", "home", "work", "errand", 
                "assistance")){
        return(stringi::stri_trans_totitle(x))
    } else if(x == 'homeoffice'){
        return('Work')
    } else if(x == 'coworking'){
        return('Work')
    } else if(x == 'study'){
        return('Education')
    } else if(x == 'wait'){
        return('Other')
    }
})

# transform wkb geometry data into longitude+latitude
all_activities <- cbind(all_activities, wkb2lonlat(all_activities$geometry))

# transform UTC time to local time, and infer time info.
all_activities$started_at_local <- utc2local_time(
    all_activities$started_at, all_activities$sf)
all_activities$finished_at_local <- utc2local_time(
    all_activities$finished_at, all_activities$sf)

all_activities$starttime <- lubridate::hour(all_activities$started_at_local)
all_activities$endtime <- lubridate::hour(all_activities$finished_at_local)
all_activities$weekday <- lubridate::wday(all_activities$started_at_local, 
                                      week_start = 1)
all_activities$yday <- lubridate::yday(all_activities$started_at_local)
all_activities$inside_day <- ifelse(all_activities$yday == 
                              lubridate::yday(all_activities$finished_at_local), 
                              1, 0)
all_activities$duration_min <- round(all_activities$duration/60, 1)


#### extract cluster based info

# conduct spatial hierarchical clustering

all_activities <- data.table::merge.data.table(
    all_activities, all_activities[, h_clustering(.SD), by = 'user_id', 
                           .SDcols = c('ID', 'lon', 'lat')][, -1], 
    by = 'ID', sort = FALSE
)

# infer how many activities for each participants

all_activities <- data.table::merge.data.table(
    all_activities, all_activities[, .(activities_per_user = length(ID), 
                               surveyed_day = length(unique(yday))), 
                           by = 'user_id'], 
    by = c('user_id'), sort = FALSE
)

all_activities <- data.table::merge.data.table(
    all_activities, all_activities[, .(
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

all_activities$cluster_freq <- all_activities$activities_per_cluster / 
    all_activities$activities_per_user

all_activities$daily_cluster_freq <- all_activities$activities_per_cluster / 
    all_activities$surveyed_day

all_activities$daily_activities <- all_activities$activities_per_user / 
    all_activities$surveyed_day

# Handling missing value
# apply(all_activities, 2, function(x) length(which(is.na(x))))
all_activities[is.na(all_activities)] = 0

# Code categorical variables as factor

all_activities[, c('labels', 'weekday', 'inside_day')] <- 
    lapply(all_activities[,  c('labels', 'weekday', 'inside_day')], factor)

# extract spatial feature of all_activities to "SpatialPointsDataFrame" 
# and save to .shp file

activities_sf <- all_activities[, .(ID, lon, lat)]
sp::coordinates(activities_sf) <- ~ lon + lat
sp::proj4string(activities_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'

rgdal::writeOGR(activities_sf, dsn = '3_output/geo',
                layer = 'activities_sf', driver = "ESRI Shapefile", 
                check_exists = TRUE, overwrite_layer = TRUE)

# transform projections of current shapefile

swiss_kanton <- rgdal::readOGR(
    '1_data/geo_data/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp')
swiss_kanton <- sp::spTransform(
    swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )

rgdal::writeOGR(swiss_kanton, dsn = '3_output/geo',
                layer = 'swiss_kanton', driver = "ESRI Shapefile", 
                check_exists = TRUE, overwrite_layer = TRUE)

# infer in which kanton the activities occur 
point_polygon <- sp::over(activities_sf, swiss_kanton)
all_activities$inkanton <- as.character(point_polygon$NAME)


save(all_activities,file = '3_output/all_activities.RData')




# data 'participants' cleaning ----


# load('3_output/all_activities.RData')

if(i_rserver == 0){
    participants <- readr::read_delim(
        '1_data/mobis/csv/participants.csv', delim = ';', 
        guess_max = 100000, n_max = 100000)
}else {
    participants <- readr::read_delim(
        '/data/mobis/data/csv/participants.csv', delim = ';', 
        guess_max = 100000, n_max = 100000)
}

# remove duplicate participants info
# sum(duplicated(participants$participant_ID))
participants <- participants %>% dplyr::group_by(participant_ID) %>% 
    filter(row_number()==1)

# remove participants without record: #95502 to #3689
participants <- participants[
    participants$participant_ID %in% all_activities$user_id, ]

# Re-code surveyed results
participants$income[which(participants$income == '99')] <- NA
participants$own_car[which(participants$own_car == 9)] <- 0
participants$own_motorbike[which(participants$own_motorbike == 9)] <- 0
participants$own_bike_ebike[which(participants$own_bike_ebike == 9)] <- 0

participants <- as.data.table(participants)

# Home location imputation

home_location1 <- all_activities[
    (endtime > 5 & inside_day == 0 & in_switzerland == TRUE) | 
        labels == 'Home', 
    .(home_hc_cat = hc_cat[which(hc_cat == get_mode(hc_cat))[1]], 
      home_lon = mean(lon[which(hc_cat == get_mode(hc_cat))] ), 
      home_lat = mean(lat[which(hc_cat == get_mode(hc_cat))] )), 
    by = user_id
    ]

# home_location1[4, ]
# apply(all_activities[user_id == 'ZSYUP' & hc_cat == 4 & (
#     (endtime > 5 & inside_day == 0 & in_switzerland == TRUE) | 
#         labels == 'Home'), .(lon, lat)], 2, mean)


center_hc_cat <- all_activities[
    , .(center_hc_cat = hc_cat[which(hc_cat == get_mode(hc_cat))[1]]), 
    by = user_id
    ]

# 92% of the home cluster corresponds to the cluster with most activities
# home_center_hc_cat <- data.table::merge.data.table(
#     home_location1, center_hc_cat, by = 'user_id'
# )
# length(which(home_center_hc_cat$home_hc_cat == home_center_hc_cat$center_hc_cat))

home_location2 <- all_activities[
    ! user_id %in% home_location1$user_id,
    .(home_hc_cat = hc_cat[which(hc_cat == get_mode(hc_cat))[1]], 
      home_lon = mean(lon[which(hc_cat == get_mode(hc_cat))] ), 
      home_lat = mean(lat[which(hc_cat == get_mode(hc_cat))] )), 
    by = user_id
    ]
# apply(all_activities[user_id == 'TXWKE' & hc_cat == 6, .(lon, lat)], 2, mean)

home_location <- data.table::merge.data.table(
    rbind(home_location1, home_location2), center_hc_cat, by = 'user_id'
)
# length(which(home_location$home_hc_cat == home_location$center_hc_cat))


# ddd <- all_activities[! all_activities$user_id %in% home_location1$user_id, ]
# # length(unique(ddd$user_id))
# for(i in 1:length(unique(ddd$user_id))){
#     i = 1
#     ccc <- ddd[user_id == unique(ddd$user_id)[i], ]
#     print(sort(table(ccc$hc_cat), decreasing = TRUE)[1:5] )
#     # print(table(ccc$labels))
# }

# Infer how many users have labeled activities as 'Home'
# ddd <- all_activities[labels == 'Home']
# length(unique(ddd$user_id))


# Investigate an example without imputed 'Home'
# table(ddd$user_id)
# 
# ccc[, .(starttime, endtime)]
# which(ccc$endtime > 5 & ccc$inside_day == 0)
# which(ccc$inside_day == 0)
# plot(ccc[hc_cat==1]$starttime, ccc[hc_cat==1]$endtime)

# Investigate an example for hierarchical clustering
# ddd = all_activities[user_id == 'ASPUV']
# ccc = h_clustering(ddd[, .(ID, lon, lat)])
# eee = data.table::merge.data.table(ddd, ccc, by = c('ID'), sort = FALSE)
# eee[eee$hc_cat.x == 2, 
# .(center_lon.x, center_lat.x, center_lon.y, center_lat.y)]
# eee$center_lon.y
# get_mode(all_activities[user_id == 'ASPUV', ]$hc_cat)
# all_activities[user_id == 'ASPUV' & hc_cat == 13, .(lon, lat, center_lon, center_lat)]

participants <- data.table::merge.data.table(
    participants, home_location, 
    by.x = 'participant_ID', by.y = 'user_id'
)

participants$ifworker <- 0
participants$ifstudent <- 0

participants[employment_1 %in% c(10, 20, 30, 60) | 
                 employment_2 %in% c(10, 20, 30, 60) | 
                 employment_3 %in% c(10, 20, 30, 60), 'ifworker'] <- 1
participants[employment_1 %in% c(50) | 
                 employment_2 %in% c(50) | 
                 employment_3 %in% c(50), 'ifstudent'] <- 1

# Reserve only necessary features
participants <- participants[, .SD
    , .SDcol = c('participant_ID', participants_features, 
                 'home_hc_cat', 'home_lon', 'home_lat', 'center_hc_cat')]

# Code character features as factor
participants[, participants_factorfeatures] <- lapply(
        participants[, ..participants_factorfeatures], factor) 

# missing value imputation
participants <- as.data.frame(participants)
participants[, -1] <- VIM::kNN(participants[, -1], k = 20, imp_var = FALSE)
participants <- as.data.table(participants)

save(participants,file = '3_output/participants.RData')




# Combine activity data with participants information ----

# load('3_output/all_activities.RData')
# load('3_output/participants.RData')

all_activities <- data.table::merge.data.table(
    all_activities, participants, 
    by.x = "user_id", by.y = "participant_ID", all.x = TRUE
)

all_activities$dist2home <- geosphere::distGeo(
    p1 = all_activities[, .(lon, lat)], 
    p2 = all_activities[, .(home_lon, home_lat)])


# plot(all_activities$dist2center[1:10000], 
#      all_activities$dist2home[1:10000], log = “xy”)

save(all_activities,file = '3_output/all_activities.RData')




# Initial trip purpose imputation ----

# load('3_output/all_activities.RData')

# labeled_activity <- droplevels(
#     all_activities[all_activities$labels != 'Unknown', ])
# labeled_activity$labels <- factor(labeled_activity$labels, 
#                                   levels = activity_categories)
# 
# activity_rf_res <- randomForest::randomForest(
#     x = labeled_activity[, .SD, .SDcol = tp_sf_all],
#     y = labeled_activity$labels,
#     ntree = 100, mtry = ceiling(log2(length(tp_sf_all))+1 ) , 
#     importance = TRUE, do.trace = 1, 
#     keep.forest = TRUE
# )
# 
# 
# all_activities$predicted_purpose <- predict(
#     activity_rf_res, 
#     all_activities[, .SD, .SDcols = tp_sf_all]
# )
# 
# all_activities$predicted_purpose_ori <- all_activities$predicted_purpose
# 
# all_activities$predicted_purpose_ori[all_activities$labels != 'Unknown'] <- 
#     droplevels(all_activities$labels[all_activities$labels != 'Unknown'])
# 
# 
# save(all_activities,file = '3_output/all_activities.RData')
# save(labeled_activity, file = '3_output/labeled_activity.RData')
# save(activity_rf_res, file = '3_output/activity_rf_res.RData')


# table(purpose_imputation_res$activities_data[
#     predicted_purpose_ori == 'Work', ifworker])
# table(purpose_imputation_res$activities_data[
#     predicted_purpose_ori == 'Education', ifstudent])
# 
# table(purpose_imputation_res$activities_data[
#     predicted_purpose == 'Work', ifworker])
# table(purpose_imputation_res$activities_data[
#     predicted_purpose == 'Education', ifstudent])
# 
# table(purpose_imputation_res$activities_data[
#     labels == 'Work', ifworker])
# table(purpose_imputation_res$activities_data[
#     labels == 'Education', ifstudent])

# trip purpose imputation - Ensemble filter ----

# load('3_output/all_activities.RData')

labeled_activity <- droplevels(
    all_activities[all_activities$labels != 'Unknown', ])
labeled_activity$labels <- factor(labeled_activity$labels, 
                                  levels = activity_categories)

# Currently the model lacks power for across participants imputation
labeled_activity <- labeled_activity[order(user_id)]

clf_tp_labeled <- multi_clf_kfold(
    x = labeled_activity[, .SD, .SDcol = tp_sf_all],
    y = labeled_activity$labels,
    ID = labeled_activity$ID,
    k = 2
)
save(clf_tp_labeled, file = '3_output/clf_tp_labeled.RData')
# load('3_output/clf_tp_labeled.RData')

# load('3_output/all_activities.RData')

# dim(all_activities)
# dim(clf_tp_labeled$prediction[, c("ID", "all_predicted")])
all_activities <- data.table::merge.data.table(
    all_activities, 
    clf_tp_labeled$prediction[, c("ID", "all_predicted")], 
    by = 'ID',
    all.x = TRUE, 
    sort = FALSE
)

system.time(
    clf_tp_all <- multi_clf_kfold(
        x = all_activities[all_predicted > 0, .SD, .SDcol = tp_sf_all],
        y = droplevels(all_activities$labels[
            which(all_activities$all_predicted > 0)]),
        ID = all_activities[all_predicted > 0, .SD, .SDcol = 'ID'],
        k = 1,
        multi_algorithms = list(
            rf = TRUE, c50 = TRUE, nb = FALSE, mars = TRUE),
        # all_predicted == 0 | is.na(all_predicted)
        x_predictors = all_activities[, .SD, .SDcol = tp_sf_all],
        ID_predict = all_activities[, .SD, .SDcol = 'ID']
    )
)
save(clf_tp_all, file = '3_output/clf_tp_all.RData')

all_activities <- data.table::merge.data.table(
    all_activities, 
    clf_tp_all$prediction, 
    by = 'ID',
    all.x = TRUE, 
    sort = FALSE
)

# caret::confusionMatrix(table(
#     factor(all_activities$labels[which(all_activities$all_predicted > 0)],
#            levels = activity_categories),
#     factor(all_activities$mars_prediction[
#         which(all_activities$all_predicted > 0)],
#            levels = activity_categories)
# ))
    
all_activities$imputed_purpose <- all_activities$rf_prediction
all_activities$imputed_purpose[which(all_activities$all_predicted > 0)] <- 
    all_activities$labels[which(all_activities$all_predicted > 0)]

# caret::confusionMatrix(table(
#     factor(all_activities$rf_prediction[which(all_activities$all_predicted > 0)],
#            levels = activity_categories),
#     factor(all_activities$labels[which(all_activities$all_predicted > 0)],
#         levels = activity_categories)
# ))

save(all_activities, file = '3_output/all_activities.RData')

# imputed_activity_0507 <- all_activities[
#     , .SD, .SDcol = c("ID", "labels", "imputed_purpose")]
# save(imputed_activity_0507, file = '3_output/imputed_activity_0507.RData')
imputed_activity_0608 <- all_activities[
    , .SD, .SDcol = c("ID", "labels", "imputed_purpose")]
save(imputed_activity_0608, 
     file = '/data/mobis/data/enrichments/imputed_activity_0608.RData')







filtered_activity <- labeled_activity[
    ! ID %in% clf_res_labeled_activity$prediction$ID[
        clf_res_labeled_activity$prediction$all_predicted == 0]][
            order(user_id)]

clf_res_filtered_activity <- multi_clf_kfold(
    x = filtered_activity[, .SD, .SDcol = tp_sf_all],
    y = filtered_activity$labels,
    ID = filtered_activity$ID,
    k = 2, 
    par_rf = list(
        ntree = 100, importance = FALSE, do.trace = 1, keep.forest = FALSE)
)
save(clf_res_filtered_activity, file = '3_output/clf_res_filtered_activity.RData')
# load('3_output/clf_res_filtered_activity.RData')


clf_res_labeled_activity_across_participants <- multi_clf_kfold(
    x = labeled_activity[, .SD, .SDcol = tp_sf_all],
    y = labeled_activity$labels,
    ID = labeled_activity$ID,
    k = 2, 
    group = labeled_activity$user_id
)
save(clf_res_labeled_activity_across_participants, 
     file = '3_output/clf_res_labeled_activity_across_participants.RData')
# load('3_output/clf_res_labeled_activity_across_participants.RData')

clf_res_filtered_activity_across_participants <- multi_clf_kfold(
  x = filtered_activity[, .SD, .SDcol = tp_sf_all],
  y = filtered_activity$labels,
  ID = filtered_activity$ID,
  k = 2, 
  group = filtered_activity$user_id
)
save(clf_res_filtered_activity_across_participants, 
     file = '3_output/clf_res_filtered_activity_across_participants.RData')
# load('3_output/clf_res_filtered_activity_across_participants.RData')

# length(clf_res_labeled_activity$prediction$all_predicted == 4)
# cor(clf_res_labeled_activity$prediction[, c("rf_predicted", "c50_predicted", "nb_predicted", "mars_predicted")])
# ddd <- clf_res_labeled_activity$prediction$ID[
#     clf_res_labeled_activity$prediction$all_predicted == 0]

# clf_res_labeled_activity$accuracy
# clf_res_filtered_activity$accuracy
# caret::confusionMatrix(
#     table(
#         factor(clf_res_labeled_activity$prediction$labels, 
#                levels = activity_categories), 
#         factor(clf_res_labeled_activity$prediction$rf_prediction, 
#                levels = activity_categories)
#     )
# )
# caret::confusionMatrix(
#     table(
#         factor(clf_res_filtered_activity$prediction$labels, 
#                levels = activity_categories),
#         factor(clf_res_filtered_activity$prediction$rf_prediction, 
#                levels = activity_categories)
#     )
# )



tp_py <- all_activities[, .SD, .SDcol = c('ID', 'labels', tp_sf_all)]
readr::write_csv(tp_py, '3_output/tp_py.csv')

ddd <- readr::read_delim(
  '3_output/tp_py.csv', delim = ',', n_max = n_max)


# Add new properties ------------------------------------------------------


load('/data/mobis/data/enrichments/scratch/all_activities.RData')

swiss_kanton <- rgdal::readOGR(
    '~/scratch/geo/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp')
swiss_kanton <- sp::spTransform(
    swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )

activities_sf <- all_activities[1:10000, .(ID, lon, lat)]
sp::coordinates(activities_sf) <- ~ lon + lat
sp::proj4string(activities_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'

# infer in which kanton the activities occur 
system.time(
    point_polygon <- sp::over(activities_sf, swiss_kanton)
)
all_activities$inkanton <- as.character(point_polygon$NAME)


save(all_activities,file = '3_output/all_activities.RData')








