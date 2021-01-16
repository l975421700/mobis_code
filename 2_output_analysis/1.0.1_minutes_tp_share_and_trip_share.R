
# whether to reimpute activities
reimpute <- FALSE

# whether it is debug run
debug_run <- FALSE
debug_run_trip <- TRUE

# whether to focus on activities in Swiss
i_inswiss <- TRUE

# specify package, namelist, functions -----------------------------------


################################ reimpute activities

if(isTRUE(reimpute)){
    mobis_analysis_directory = '/data/students/qigao/mobis_analysis'
    imputed_activity_incremental_output_path =
        '/data/mobis/data/enrichments/run_gao/imputed_activity_incremental.RData'
    imputed_activity_path = '/data/mobis/data/enrichments/imputed_activity_incremental.RData'
    # imputed_activity_path = '/data/mobis/data/enrichments/imputed_activity_incremental.RData'
    source(paste(
        mobis_analysis_directory,
        '/r/eth_purpose_imputation/r_module/tp_incremental_impute_function.R',
        sep=''))
    tp_increment_imputation(
        mobis_analysis_directory = mobis_analysis_directory,
        imputed_activity_incremental_output_path =
            imputed_activity_incremental_output_path,
        imputed_activity_path = imputed_activity_path
    )
}



################################ Directories and namelist specification

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

monday_weekth_labels_changes <- c(
    'Aug-31', 'Sep-07', 'Sep-14', 'Sep-21', 'Sep-28', 'Oct-05', 'Oct-12',
    'Oct-19', 'Oct-26', 'Nov-02', 'Nov-09', 'Nov-16', 'Nov-23', 'Nov-30',
    'Dec-07', 'Dec-14', 'Dec-21', 'Dec-28', 'Jan-04', 'Jan-11', 'Jan-18',
    'Jan-25', 'Feb-01', 'Feb-08', 'Feb-15', 'Feb-22', 'Mar-01', 'Mar-08',
    'Mar-15', 'Mar-22', 'Mar-29', 'Apr-05', 'Apr-12', 'Apr-19', 'Apr-26'
)

# seq(as.Date("2019-09-02"), to=as.Date("2021-4-30"), by='1 weeks')

# heatmap_legend_labels <- c(
#     "0","0.05", "0.1", "0.15", "0.2","0.25", "0.3", "0.35", "0.4")
# monday_weekth[seq(1, 64, 4)]
lockdown = 29 # monday_weekth[29]
post_lockdown = 37 # monday_weekth[37]
mask_requirement = 45 # monday_weekth[45]
second_wave = 60  # monday_weekth[60]


################################ Import packages

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


################################ define required functions

floor_datetime <- function(
    date_var, floor_seconds = 60, origin = "1970-01-01") {
    
    # defaults to minute rounding
    return(as.POSIXct(floor(as.numeric(date_var) /
                                (floor_seconds))*(floor_seconds),
                      origin = origin))
    
    # if(!is(date_var, "POSIXct"))
    #     stop("Please pass in a POSIXct variable")
    # if(is.na(date_var))
    #     return(as.POSIXct(NA))
    # else {
    #     return(as.POSIXct(floor(as.numeric(date_var) / 
    #                                 (floor_seconds))*(floor_seconds),
    #                       origin = origin))
    # }
}

activity_minutes <- function(starttime, endtime){
    startmin <- round(starttime, units="min")
    endmin <- round(endtime, units="min") - 60
    
    if(startmin > endmin){
        endmin = endmin + 60*60
    }
    return(seq(startmin, endmin, "min"))
}

# starttime = allnew_activities$started_at_local[1]
# endtime = allnew_activities$finished_at_local[1]
# activity_minutes(starttime, endtime)



# estimate minutes share of activities -----------------------------------


################################ load activities data
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


if(isTRUE(debug_run)){
    allnew_activities = allnew_activities[1:40000, ]
}

if(isTRUE(i_inswiss)){
    #### add Kanton information
    swiss_kanton <- rgdal::readOGR(
        '~/scratch/geo/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp')
    swiss_kanton <- sp::spTransform(
        swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )
    start_activities_sf <- allnew_activities[, .(ID, lon, lat)]
    sp::coordinates(start_activities_sf) <- ~ lon + lat
    sp::proj4string(start_activities_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'
    start_point_polygon <- sp::over(start_activities_sf, swiss_kanton)
    allnew_activities$start_inkanton <- as.character(start_point_polygon$NAME)
    
    #### select activities in Swiss
    allnew_activities <- allnew_activities[!is.na(start_inkanton), ]
}


################################ minutes_share_of_activities


strat_min = floor_datetime(min(allnew_activities$started_at_local), 15 * 60)
end_min = floor_datetime(max(allnew_activities$finished_at_local), 15 * 60)

minutes_share = data.table(
    timeslot=seq(strat_min, end_min, by=60 * 15),
    Home = 0, Work = 0, Leisure = 0, Shopping = 0,
    Errand = 0, Other = 0, Assistance = 0, Education = 0
)
minutes_share_colnames = colnames(minutes_share)

# expand activities to minutes interval


Sys.time()
ite_num = dim(allnew_activities)[1] %/% 10000 + 1
for(i in 1:ite_num){
    # i=1
    
    # iterate over data table
    if(i < ite_num){
        ite_allnew_activities <- allnew_activities[((i-1)*10000) + (1:10000), ]
    }else{
        ite_allnew_activities <- allnew_activities[
            ((ite_num-1)*10000 + 1):dim(allnew_activities)[1], ]
    }
    
    # expand activities to each minnutes
    ite_expanded_activities <- ite_allnew_activities[
        , .(minute = activity_minutes(started_at_local, finished_at_local)),
        by = c('ID', 'user_id', 'imputed_purpose')
        ]
    
    # add 15 minutes time slot
    ite_expanded_activities$by15 = floor_datetime(
        ite_expanded_activities$minute, 15 * 60)
    
    # aggregate count of activity in each timeslot
    ite_timeslot_activity_count = ite_expanded_activities[
        , .(timeslot_count = .N), by = c('imputed_purpose', 'by15')
    ]
    
    # store the count
    for(j in 1:dim(ite_timeslot_activity_count)[1]){
        # j = 1
        # ite_timeslot_activity_count[j, ]
        row_index = which(minutes_share$timeslot == 
                              ite_timeslot_activity_count[j, ]$by15)
        # minutes_share[row_index, ]
        column_index = which(minutes_share_colnames == 
                                 ite_timeslot_activity_count[j, ]$imputed_purpose)
        
        set(minutes_share, i = row_index, 
            j = column_index,
            minutes_share[row_index, ..column_index] + 
                ite_timeslot_activity_count[j, ]$timeslot_count)
    }
    
    print(paste(i, ite_num, sep='/'))
}
Sys.time()

save(minutes_share,
     file = '/data/students/qigao/scratch/2_output_analysis/minutes_share.RData')

# check
# summary(minutes_share)


# estimate trip minutes in each timeslot ---------------------------------

legs_path = '/data/mobis/data/csv/legs.csv'
covid_legs_glob = '/data/mobis/data/covid/*[0-9]'


legs <- readr::read_delim(legs_path, delim = ',', n_max = Inf)
# legs1 <- readr::read_delim(legs_path, delim = ',', n_max = Inf,
#                           locale=readr::locale(tz='Europe/Zurich'),
#                           col_types = cols(trip_id = col_character()))
legs <- as.data.table(legs)

# combine original mobis and covid activities
covid_legs_paths <- Sys.glob(covid_legs_glob)
for(i in 1:length(covid_legs_paths)){
    covid_legs <- readr::read_delim(
        paste(covid_legs_paths[i], '/covid_legs.csv', sep=''),
        delim = ',', n_max = Inf)
    covid_legs <- as.data.table(covid_legs)
    
    if(i == 1){
        all_legs <- data.table::rbindlist(
            list(legs[, .SD, .SDcol = colnames(covid_legs)], covid_legs)
        )
    }else{
        all_legs <- data.table::rbindlist(
            list(all_legs, covid_legs)
        )
    }
}


colnames(all_legs)[colnames(all_legs) == "trip_id"] <- 'ID'

if(isTRUE(debug_run_trip)){
    all_legs = all_legs[1:40000, ]
}

# all_legs = all_legs[1500:2500, ]

# transform wkb geometry data into longitude+latitude
all_legs <- cbind(all_legs, wkb2lonlat(all_legs$start_point, suffix = 'start'))
all_legs <- cbind(all_legs, wkb2lonlat(all_legs$end_point, suffix = 'end'))

# transform UTC time to local time
all_legs$started_at_local <- utc2local_time(
    all_legs$started_at, all_legs$start_sf)
all_legs$finished_at_local <- utc2local_time(
    all_legs$finished_at, all_legs$end_sf)


if(isTRUE(i_inswiss)){
    # import kanton shapefile
    swiss_kanton <- rgdal::readOGR(
        '~/scratch/geo/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp')
    swiss_kanton <- sp::spTransform(
        swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )
    
    # locate start point
    start_legs_sf <- all_legs[, .(ID, start_lon, start_lat)]
    sp::coordinates(start_legs_sf) <- ~ start_lon + start_lat
    sp::proj4string(start_legs_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'
    start_point_polygon <- sp::over(start_legs_sf, swiss_kanton)
    
    # locate end point
    end_legs_sf <- all_legs[, .(ID, end_lon, end_lat)]
    sp::coordinates(end_legs_sf) <- ~ end_lon + end_lat
    sp::proj4string(end_legs_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'
    end_point_polygon <- sp::over(end_legs_sf, swiss_kanton)
    
    # select legs in swiss
    all_legs = all_legs[
        (!is.na(as.character(start_point_polygon$NAME))) &
            (!is.na(as.character(end_point_polygon$NAME))),
    ]
}

rm(start_point_polygon, end_point_polygon)


################################ minutes_share_of_trips

start_min_t = floor_datetime(min(all_legs$started_at_local), 15 * 60)
end_min_t = floor_datetime(max(all_legs$finished_at_local), 15 * 60)


minutes_share_t = data.table(
    timeslot=seq(start_min_t, end_min_t, by=60 * 15), Count = 0
)

# expand trips to minutes interval

Sys.time()
ite_num = dim(all_legs)[1] %/% 10000 + 1

for(i in 1:ite_num){
    begintime = Sys.time()
    # i=1
    
    # iterate over data table
    if(i < ite_num){
        ite_all_legs <- all_legs[((i-1)*10000) + (1:10000), ]
    }else{
        ite_all_legs <- all_legs[((ite_num-1)*10000 + 1):dim(all_legs)[1], ]
    }
    
    
    # expand trips to each minutes
    ite_expanded_legs <- ite_all_legs[
        , .(minute = activity_minutes(started_at_local, finished_at_local)),
        by = c('ID')
        ]
    
    # add 15 minutes time slot
    ite_expanded_legs$by15 = floor_datetime(
        ite_expanded_legs$minute, 15 * 60)
    
    # aggregate count of legs in each timeslot
    ite_timeslot_legs_count = ite_expanded_legs[
        , .(timeslot_count = .N), by = c('by15')
        ]
    
    # store the count
    for(j in 1:dim(ite_timeslot_legs_count)[1]){
        # j = 1
        # ite_timeslot_legs_count[j, ]
        row_index = which(minutes_share_t$timeslot ==
                              ite_timeslot_legs_count[j, ]$by15)
        # minutes_share_t[row_index, ]
        set(minutes_share_t, i = row_index,
            j = 2L,
            minutes_share_t[row_index, 2L] +
                ite_timeslot_legs_count[j, ]$timeslot_count)
    }
    
    print(paste(i, ite_num,
                as.numeric(Sys.time() - begintime, units = "secs"), sep='/'))
}
Sys.time()

save(minutes_share_t,
     file = '/data/students/qigao/scratch/2_output_analysis/minutes_share_t.RData')

# check
# summary(minutes_share_t)





# aggregate weekly 15min activities --------------------------------------


load('/data/students/qigao/scratch/2_output_analysis/minutes_share.RData')

monday_date <- seq(as.Date(min(minutes_share$timeslot)),
                   to=as.Date(max(minutes_share$timeslot)),
                   by='1 weeks')


################ calculate the #th of day and week
minutes_share$dayth <- ceiling(as.numeric(difftime(
    minutes_share$timeslot,
    mobis_start_date, units='days')))
minutes_share$weekth <- (minutes_share$dayth - 1) %/% 7 + 1


################ calculate the #th of hour_minute
minutes_share$hour_minute_th <- lubridate::hour(minutes_share$timeslot) * 4 + 
    lubridate::minute(minutes_share$timeslot) / 15 + 1


################ aggregate activities for each week and hour_minute
weekly_minutes <- minutes_share[
    , .(Home_total = sum(Home), Work_total = sum(Work), 
        Leisure_total = sum(Leisure), Shopping_total = sum(Shopping), 
        Errand_total = sum(Errand), Other_total = sum(Other), 
        Assistance_total = sum(Assistance), Education_total = sum(Education)),
    by = c('weekth', 'hour_minute_th')
]
weekly_minutes$Total = apply(weekly_minutes[, 3:10], 1, sum)
# minutes_share$total_minute = apply(minutes_share[, 2:9], 1, sum)
# all(weekly_minutes$Total == minutes_share[
#     , .(sum(total_minute)), by = c('weekth', 'hour_minute_th')]$V1)


################ calculate the minutes share

weekly_minutes$Home_share = weekly_minutes$Home_total / weekly_minutes$Total
weekly_minutes$Work_share = weekly_minutes$Work_total / weekly_minutes$Total
weekly_minutes$Leisure_share = 
    weekly_minutes$Leisure_total / weekly_minutes$Total
weekly_minutes$Shopping_share = 
    weekly_minutes$Shopping_total / weekly_minutes$Total
weekly_minutes$Errand_share = weekly_minutes$Errand_total / weekly_minutes$Total
weekly_minutes$Other_share = weekly_minutes$Other_total / weekly_minutes$Total
weekly_minutes$Assistance_share = 
    weekly_minutes$Assistance_total / weekly_minutes$Total
weekly_minutes$Education_share = 
    weekly_minutes$Education_total / weekly_minutes$Total
# table(apply(weekly_minutes[, 12:19], 1, sum))


################ remove the latest week

weekly_minutes <- weekly_minutes[weekth != max(weekth), ]


# aggregate weekly 15min trips -------------------------------------------


load('/data/students/qigao/scratch/2_output_analysis/minutes_share_t.RData')


################ calculate the #th of day and week
minutes_share_t$dayth <- ceiling(as.numeric(difftime(
    minutes_share_t$timeslot,
    mobis_start_date, units='days')))
minutes_share_t$weekth <- (minutes_share_t$dayth - 1) %/% 7 + 1


################ calculate the #th of hour_minute
minutes_share_t$hour <- lubridate::hour(minutes_share_t$timeslot)
minutes_share_t$minute <- lubridate::minute(minutes_share_t$timeslot)

minutes_share_t$hour_minute_th <-
    minutes_share_t$hour * 4 +
    minutes_share_t$minute / 15 + 1


################ aggregate activities for each week and hour_minute
weekly_minutes_t <- minutes_share_t[
    , .(Trip_Total = sum(Count)),
    by = c('weekth', 'hour_minute_th')
    ]

# sum(minutes_share_t$Count) == sum(weekly_minutes_t$Total)


################ calculate the minutes share




################ remove the latest week

weekly_minutes_t <- weekly_minutes_t[weekth != max(weekth), ]



# plot_the_results -------------------------------------------------------

################################ order the minutes share
weekly_minutes = weekly_minutes[order(weekth, hour_minute_th)]
weekly_minutes_t = weekly_minutes_t[order(weekth, hour_minute_th)]


################################ calculate the transparency
transparency = data.table::merge.data.table(
    weekly_minutes[, .SD, .SDcol = c('weekth', 'hour_minute_th', 'Total')],
    weekly_minutes_t,
    by = c('weekth', 'hour_minute_th')
)

################################ calculate the share
act_home_share = weekly_minutes$Home_share
act_work_edu_share = weekly_minutes$Leisure_share + 
    weekly_minutes$Shopping_share +
    weekly_minutes$Errand_share + weekly_minutes$Other_share +
    weekly_minutes$Assistance_share
act_others_share = weekly_minutes$Work_share + weekly_minutes$Education_share
act_share = (transparency$Trip_Total / transparency$Total)

################################ construct RGB
rgb_colors = rgb(
    red = act_home_share / 1.5,
    green = act_work_edu_share,
    blue = act_others_share,
    alpha = 1 - act_share
)

rgb_colors1 = rgb(
    red = act_home_share / 1.5,
    green = act_work_edu_share,
    blue = act_others_share,
    alpha = 1 - 2 * act_share
)

rgb_colors2 = rgb(
    red = act_home_share / 1.5,
    green = act_work_edu_share,
    blue = act_others_share,
    alpha = 1 - 1.5 * act_share
)

rgb_colors3 = rgb(
    red = rep(0, length(weekly_minutes$Home_share)),
    green = rep(0, length(weekly_minutes$Home_share)),
    blue = rep(0, length(weekly_minutes$Home_share)),
    alpha = 1 - 2 * act_share
)

rgb_colors4 = rgb(
    red = act_home_share / 1.5,
    green = act_work_edu_share,
    blue = act_others_share,
    alpha = 1 - 2.5 * (transparency$Trip_Total / 
                         (transparency$Total + transparency$Trip_Total))
)

rgb_colors5 = rgb(
    red = rep(0, length(weekly_minutes$Home_share)),
    green = rep(0, length(weekly_minutes$Home_share)),
    blue = rep(0, length(weekly_minutes$Home_share)),
    alpha = 1 - 2.3 * (transparency$Trip_Total / 
                         (transparency$Total + transparency$Trip_Total))
)

rgb_colors6 = rgb(
    red = rep(0, length(weekly_minutes$Home_share)),
    green = rep(0, length(weekly_minutes$Home_share)),
    blue = rep(0, length(weekly_minutes$Home_share)),
    alpha = 1 - 2.5 * (transparency$Trip_Total / 
                           (transparency$Total + transparency$Trip_Total))
)


################################ plot the minutes share

png(
    # 'visualization/01_minutes_share/1.2.0_minutes_share_of_activities__trips_in_transparency.png',
    # 'visualization/01_minutes_share/1.2.1_minutes_share_of_activities__trips_in_doubled_transparency.png',
    # 'visualization/01_minutes_share/1.2.2_minutes_share_of_activities__trips_in_1.5_transparency.png',
    # 'visualization/01_minutes_share/1.2.3_minutes_share_of_activities__only_doubled_transparency.png',
    'visualization/01_minutes_share/1.2.5_minutes_share_of_activities__trips_share_in_2.5_transparency.png',
    # 'visualization/01_minutes_share/1.2.6_minutes_share_of_activities__trips_share_only_2.3_transparency.png',
    # 'visualization/01_minutes_share/1.2.7_minutes_share_of_activities__trips_share_only_2.5_transparency.png',
    width = 8.8, height = 8.8, units = 'cm', res = 600)
ggplot() +
    geom_tile(
        data = weekly_minutes,
        aes(x = weekth, y = hour_minute_th,
            fill = factor(seq(1:length(rgb_colors)))), colour = 'white') +
    geom_vline(xintercept = lockdown, linetype = "dashed",
               color = 'black', size = 0.2) +
    geom_vline(xintercept = post_lockdown, linetype = "dashed",
               color = 'black', size = 0.2) +
    geom_vline(xintercept = mask_requirement, linetype = "dashed",
               color = 'black', size = 0.2) +
    geom_vline(xintercept = second_wave, linetype = "dashed",
               color = 'black', size = 0.2) +
    # scale_fill_manual(values = rgb_colors) +
    # scale_fill_manual(values = rgb_colors1) +
    # scale_fill_manual(values = rgb_colors2) +
    # scale_fill_manual(values = rgb_colors3) +
    scale_fill_manual(values = rgb_colors4) +
    # scale_fill_manual(values = rgb_colors5) +
    # scale_fill_manual(values = rgb_colors6) +
    ylab("O'clock") +
    theme_bw() + theme_minimal() +
    scale_x_continuous(
        limits = c(min(weekly_minutes$weekth) -1, 
                   max(weekly_minutes$weekth) +1),
        expand = c(0, 0),
        breaks = seq(1, max(weekly_minutes$weekth), 4),
        labels = monday_weekth_labels[
            1:length(seq(1, max(weekly_minutes$weekth), 4))] ) +
    scale_y_reverse(
        limits = c(96 + 1, 25 - 1),
        expand = c(0, 0),
        breaks = seq(93, 25, -4),
        labels = seq(23, 6, -1)
    ) +
    theme(
        axis.title.y = element_text(
            size = 8, family = 'Times New Roman'),
        axis.text.x = element_text(
            size = 8, family = 'Times New Roman', vjust = 0.75, angle = 45),
        axis.text.y = element_text(
            size = 8, family = 'Times New Roman', hjust = 1),
        axis.ticks = element_line(size = 0.2),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(0, "cm"),
        legend.position = "none",
        # legend.position = c(0.45, -0.25),
        # legend.background = element_blank(),
        # (top, right, bottom, left)
        plot.margin = unit(c(0.05, 0.3, 0.00, 0.08), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect( colour = "black", fill=NA, size=0.1),
        panel.background = element_blank()
    )
dev.off()


################################ plot the legend

# https://cran.r-project.org/web/packages/Ternary/vignettes/Ternary.html
# install.packages('Ternary')
# Ternary::TernaryApp()
suppressPackageStartupMessages(library('Ternary'))

png('visualization/01_minutes_share/1.2.4_minutes_share_of_activities_legend.png',
    width = 8.8, height = 7.8, units = 'cm', res = 600)
par(ps = 10, 
    # mai = c(0.01, 0.01, 0.01, 0.01), mar = c(0.01, 0.01, 0.01, 0.01),
    mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 0.1),
    family = 'Times New Roman')
TernaryPlot(
    alab = "Home \u2192", 
    blab = "Other activities \u2192", 
    clab = "\u2190 Work and Education",
    lab.col = c('red', 'darkgreen', 'blue'),
    lab.offset = 0.15,
    point = 'up',
    # xlim = c(0, 1), ylim = c(0, 1), 
    axis.col = 'gray', 
    # axis.labels = seq(0, 1, 0.1),
    axis.labels = seq(0, 100, 10),
    ticks.col = 'gray', ticks.length = 0.01, 
    axis.rotate = FALSE,
    padding = 0.1
    )
cols <- TernaryPointValues(
    function (a, b, c) rgb(a/1.5, b, c), resolution = 240L)
ColourTernary(cols, spectrum = NULL)
dev.off()











