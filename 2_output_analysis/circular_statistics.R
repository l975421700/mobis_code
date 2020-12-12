

# Directories and namelist specification ---------------------------------

# Time Based Heatmaps: https://www.littlemissdata.com/blog/heatmaps
mobis_code_dir <- '/data/students/qigao/mobis_code'
imputed_activity_incremental_path <-
    "/data/mobis/data/enrichments/run_gao/imputed_activity_incremental.RData"
activities_path = '/data/mobis/data/csv/activities.csv'
covid_activities_glob = '/data/mobis/data/covid/*[0-9]'
participants_path = '/data/mobis/data/enrichments/run_gao/participants.RData'
all_activities_path = '/data/mobis/data/enrichments/run_gao/all_activities.RData'

mobis_start_date <- as.POSIXlt("2019-09-02 00:00:00 CEST")
monday_weekth_labels <- c(
    'Sep-02', 'Sep-30', 'Oct-28', 'Nov-25', 'Dec-23', 'Jan-20', 'Feb-17',
    'Mar-16', 'Apr-13', 'May-11', 'Jun-08', 'Jul-06', 'Aug-03', 'Aug-31',
    'Sep-28', 'Oct-26', 'Nov-23', 'Dec-21', 'Jan-18', 'Feb-15', 'Mar-15'
)
heatmap_legend_labels <- c(
    "0","0.05", "0.1", "0.15", "0.2","0.25", "0.3", "0.35", "0.4")
# monday_weekth[seq(1, 64, 4)]
lockdown = 29 # monday_weekth[29]
post_lockdown = 37 # monday_weekth[37]
mask_requirement = 45 # monday_weekth[45]
second_wave = 60  # monday_weekth[60]


# Import packages --------------------------------------------------------

source(paste(mobis_code_dir, '/r_module/install_packages.R',
             sep = ''), chdir = TRUE)
source(paste(mobis_code_dir, '/r_module/utc2local_time.R',
             sep = ''), chdir = TRUE)
source(paste(mobis_code_dir, '/r_module/namelist.R',
             sep = ''), chdir = TRUE)
source(paste(mobis_code_dir, '/r_module/wkb2lonlat.R',
             sep = ''), chdir = TRUE)

library('readr')
suppressPackageStartupMessages(library('plyr'))
suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('stringi'))
suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('data.table'))
suppressPackageStartupMessages(library('ggplot2'))
suppressPackageStartupMessages(library('gridExtra'))


# load data --------------------------------------------------------------

load(imputed_activity_incremental_path)
load(participants_path)
load(all_activities_path)

activities <- readr::read_delim(activities_path, delim = ',', n_max = Inf)

# combine original mobis and covid activities
covid_activities_paths <- Sys.glob(covid_activities_glob)
for(i in 1:length(covid_activities_paths)){
    covid_activities <- readr::read_delim(
        paste(covid_activities_paths[i], '/covid_activities.csv', sep=''),
        delim = ',', n_max = Inf)
    if(i == 1){
        old_activities <- rbind(activities, covid_activities)
    }else{
        old_activities <- rbind(old_activities, covid_activities)
    }
}

old_activities <- as.data.table(old_activities)
colnames(old_activities)[colnames(old_activities) == "activity_id"] <- 'ID'


# data enrichments -------------------------------------------------------

new_activities <- old_activities[! old_activities$ID %in% all_activities$ID, ]
new_activities <- cbind(new_activities, wkb2lonlat(new_activities$geometry))
new_activities$started_at_local <- utc2local_time(
    new_activities$started_at, new_activities$sf)
new_activities$finished_at_local <- utc2local_time(
    new_activities$finished_at, new_activities$sf)

new_activities$starttime <- lubridate::hour(new_activities$started_at_local)
new_activities$endtime <- lubridate::hour(new_activities$finished_at_local)
new_activities$weekday <- lubridate::wday(new_activities$started_at_local, 
                                          week_start = 1)

reserved_columns <- c('user_id', 'ID', 'lon', 'lat', 'started_at_local',
                      'finished_at_local', 'starttime', 'endtime', 'weekday')

allnew_activities <- rbind(
    all_activities[all_activities$ID %in% old_activities$ID, ..reserved_columns],
    new_activities[, ..reserved_columns])

#### add tp information
allnew_activities <- data.table::merge.data.table(
    allnew_activities,
    imputed_activity_incremental[, c("ID", "imputed_purpose")],
    all.x = TRUE
)


# Week-hourly activity distribution --------------------------------------


#### calculate the #th of day and week
allnew_activities$dayth <- ceiling(as.numeric(difftime(
    allnew_activities$started_at_local,
    mobis_start_date, units='days')))
allnew_activities$weekth <- (allnew_activities$dayth - 1) %/% 7 + 1


#### aggregate activities for each week and hours
week_hourly_activities <- allnew_activities[
    , .(week_hour_N = .N), 
    by = c('weekth', 'starttime', 'imputed_purpose')
    ]

week_activities_users <- allnew_activities[
    , .(week_N = .N, week_user_N = length(unique(user_id)) ), 
    by = c('weekth', 'imputed_purpose')
    ]
week_activities_users$activities_per_user <- 
    week_activities_users$week_N / week_activities_users$week_user_N

week_hourly_activities <- data.table::merge.data.table(
    week_hourly_activities, week_activities_users,
    by = c('weekth', 'imputed_purpose')
)

week_hourly_activities$week_hour_freq <- 
    week_hourly_activities$week_hour_N / week_hourly_activities$week_N


#### remove the latest week
week_hourly_activities <- week_hourly_activities[weekth != max(weekth), ]
week_activities_users <- week_activities_users[weekth != max(weekth), ]

# circular statistics --------------------------------------

suppressPackageStartupMessages(library('circular'))

load("/data/students/qigao/mobis_code/2_output_analysis/CircStatsInR.RData")

class(week_hourly_activities)
plot(ﬁsherB1c, pch=16, col='blue', stack=T, shrink=1.2, bins=720, ticks=T)





