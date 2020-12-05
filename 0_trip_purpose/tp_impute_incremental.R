
# Main code to do incremental trip purpose imputation

# before you run ----------------------------------------------------------

# It's suggested to source this file under the working directory 'mobid_analysis',
# then in case of no new activities it will stop sooner with an error msg.
# After sourcing this file all the imputed activities will be in 
# '/data/mobis/data/enrichments/imputed_activity_incremental.RData';


# Input packages, functions, and namelist for all subsequent sections ----

source('r/eth_purpose_imputation/r_module/install_packages.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/wkb2lonlat.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/utc2local_time.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/hierarchical_clustering.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/namelist.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/multi_classification_kfold_cv.R', chdir = TRUE) 

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




# data 'activities' cleaning ----

# load data 
n_max = Inf

activities <- readr::read_delim(
    '/data/mobis/data/csv/activities.csv', delim = ',', n_max = n_max)
covid_activities <- readr::read_delim(
    '/data/mobis/data/covid/covid_activities.csv', delim = ',', 
    n_max = n_max)

# analyze only the activities that has not been imputed
load('/data/students/qigao/scratch/run_gao/imputed_activity.RData')
load('/data/mobis/data/enrichments/participants.RData')

# combine original mobis and covid activities
old_activities <- rbind(activities, covid_activities)

# remove original files
rm(activities, covid_activities)


old_activities <- as.data.table(old_activities)
colnames(old_activities)[colnames(old_activities) == "activity_id"] <- 'ID'



all_activities = old_activities[! old_activities$ID %in% imputed_activity$ID, ]

if(dim(all_activities)[1] == 0){
    stop('No new activities found!')
}

# all_activities = old_activities[seq(1,10000, 10), ]



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



# Combine activity data with participants information ----

all_activities <- data.table::merge.data.table(
    all_activities, participants, 
    by.x = "user_id", by.y = "participant_ID", all.x = TRUE
)

all_activities$dist2home <- geosphere::distGeo(
    p1 = all_activities[, .(lon, lat)], 
    p2 = all_activities[, .(home_lon, home_lat)])



# trip purpose imputation  ----

load('/nas/qigao/scratch/run_gao/clf_tp_all.RData')

all_activities$imputed_purpose = predict(
    clf_tp_all$rf[[1]], 
    all_activities[, .SD, .SDcol = tp_sf_all]
)

# caret::confusionMatrix(
#     table(factor(all_activities[which(all_activities$labels != 'Unknown')
#                                 ]$imputed_purpose, levels = activity_categories),
#           factor(all_activities[which(all_activities$labels != 'Unknown')]$labels, 
#                  levels = activity_categories)
#     )
# )

all_activities$imputed_purpose[which(all_activities$labels != 'Unknown')] <- 
    all_activities$labels[which(all_activities$labels != 'Unknown')]

imputed_activity_incremental <- all_activities[
    , .SD, .SDcol = c("ID", "labels", "imputed_purpose")]

imputed_activity_incremental = rbind(
    imputed_activity, imputed_activity_incremental
)


if(dim(all_activities)[1] != 0){
    save(imputed_activity_incremental,
         file = '/data/mobis/data/enrichments/imputed_activity_incremental.RData')
}








