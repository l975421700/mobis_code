

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

i_inswiss <- FALSE
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


# Heatmap and time series of all activities (start time) -----------------


#### calculate the #th of day and week
monday_date <- seq(as.Date(min(allnew_activities$started_at_local)),
                     to=as.Date(max(allnew_activities$started_at_local)),
                     by='1 weeks')
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


################################  plot the results

# i = 1
count_i <- c(3, 3, 3, 2, 1, 1, 1, 2)
for (i in 1:8){
    data1 <- week_hourly_activities[imputed_purpose == activity_categories[i], ]
    
    png(
        paste('visualization/2_output_analysis/mobis_1.0.', i,
              ' week_hourly heatmap of ',
              activity_categories[i], '.png', sep = ''),
        width = 8.8, height = 8.8, units = 'cm', res = 1200)
    heatmap1 = ggplot() +
        geom_tile(
            data = data1,
            aes(x = weekth, y = starttime, fill = week_hour_freq),
            colour = "white") +
        geom_vline(xintercept = lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = post_lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = mask_requirement, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = second_wave, linetype = "dashed",
                   color = 'black', size = 0.2) +
        scale_fill_gradient(
            low = "white", high = "blue",
            limits = c(0, (max(data1$week_hour_freq) %/% 0.05 + 1) * 0.05),
            breaks=seq(0, (max(data1$week_hour_freq) %/% 0.05 + 1) * 0.05, 0.05),
            labels=heatmap_legend_labels[
                1:length(seq(0, (max(data1$week_hour_freq) %/% 0.05 + 1) * 0.05,
                             0.05))],
            guide = guide_colourbar(
                title = paste('Frequency of', activity_categories[i]), 
                title.position = 'left',
                title.theme = element_text(size = 8, family = 'Times New Roman'),
                label.theme = element_text(size = 8, family = 'Times New Roman'),
                barwidth = unit(
                    0.8 * (length(seq(0, (max(data1$week_hour_freq) %/% 0.05 + 1
                    ) * 0.05, 0.05)) - 1), units = "cm"),
                barheight = unit(0.2, units = "cm"),
                draw.ulim = TRUE,
                title.vjust = 1,
                direction = 'horizontal'
            )) +
        ylab("O'clock") +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(week_hourly_activities$weekth) -1,
                       max(week_hourly_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(week_hourly_activities$weekth), 4),
            labels = monday_weekth_labels[
                1:length(seq(1, max(week_hourly_activities$weekth), 4))] ) +
        scale_y_reverse(
            limits = c(23 + 1, 0 - 1),
            expand = c(0, 0),
            breaks = seq(21, 0, -3),
            labels = seq(21, 0, -3)
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
            legend.position = c(0.45, -0.25),
            legend.background = element_blank(),
            # (top, right, bottom, left)
            plot.margin = unit(c(0.01, 0.3, 0.6, 0.08), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    
    count_int = count_i[i]
    
    if(i < 4){
        left = 0.08
    }else{
        left = 0.22
    }
    
    data2 <- week_activities_users[imputed_purpose == activity_categories[i], ]
    timeseries2 <- 
        ggplot(data = data2,
               aes(x = weekth, y = activities_per_user, group=1)) +
        geom_line(linetype = "solid", color = 'black', size = 0.2) +
        geom_vline(xintercept = lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = post_lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = mask_requirement, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = second_wave, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_text(x=lockdown - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int / 2,
                  label="lockdown", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=post_lockdown - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int/2,
                  label="post lockdown", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=mask_requirement - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int/2,
                  label="mask requirement", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=second_wave - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int/2,
                  label="second wave", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        ylab('Count / (user * week)') +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(week_hourly_activities$weekth) -1,
                       max(week_hourly_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(week_hourly_activities$weekth), 4)
        ) +
        scale_y_continuous(
            limits = c(0, (max(data2$activities_per_user) %/% count_int + 1
                           ) * count_int),
            expand = c(0, 0),
            breaks = seq(0, (max(data2$activities_per_user) %/% count_int + 1
                             ) * count_int, 
                         count_int),
            labels = seq(0, (max(data2$activities_per_user) %/% count_int + 1
                             ) * count_int,
                         count_int)/1 ) +
        theme(
            axis.title.y = element_text(
                size = 8, family = 'Times New Roman'),
            text = element_text(size = 8, family = 'Times New Roman'),
            axis.text.y = element_text(
                size = 8, family = 'Times New Roman', hjust = 1),
            axis.ticks = element_line(size = 0.1),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.spacing.x = unit(0, "cm"),
            legend.position = c(0.45, -0.25),
            legend.background = element_blank(),
            # (top, right, bottom, left)
            plot.margin = unit(c(0.2, 0.3, 0.02, left), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    
    gridExtra::grid.arrange(
        timeseries2, heatmap1, ncol = 1, heights = c(2.4, 6.4), newpage = TRUE)
    dev.off()
    print(i)
}

################################  plot the changes

head(week_hourly_activities)
head(week_activities_users)

# i = 1
count_i <- c(3, 3, 3, 2, 1, 1, 1, 2)
for (i in 1:8){
    data1 <- week_hourly_activities[imputed_purpose == activity_categories[i], ]
    
    png(
        paste('visualization/2_output_analysis/mobis_1.0.', i,
              ' week_hourly heatmap of ',
              activity_categories[i], '_changes.png', sep = ''),
        width = 8.8, height = 8.8, units = 'cm', res = 1200)
    heatmap1 = ggplot() +
        geom_tile(
            data = data1,
            aes(x = weekth, y = starttime, fill = week_hour_freq),
            colour = "white") +
        geom_vline(xintercept = lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = post_lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = mask_requirement, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = second_wave, linetype = "dashed",
                   color = 'black', size = 0.2) +
        scale_fill_gradient(
            low = "white", high = "blue",
            limits = c(0, (max(data1$week_hour_freq) %/% 0.05 + 1) * 0.05),
            breaks=seq(0, (max(data1$week_hour_freq) %/% 0.05 + 1) * 0.05, 0.05),
            labels=heatmap_legend_labels[
                1:length(seq(0, (max(data1$week_hour_freq) %/% 0.05 + 1) * 0.05,
                             0.05))],
            guide = guide_colourbar(
                title = paste('Frequency of', activity_categories[i]), 
                title.position = 'left',
                title.theme = element_text(size = 8, family = 'Times New Roman'),
                label.theme = element_text(size = 8, family = 'Times New Roman'),
                barwidth = unit(
                    0.8 * (length(seq(0, (max(data1$week_hour_freq) %/% 0.05 + 1
                    ) * 0.05, 0.05)) - 1), units = "cm"),
                barheight = unit(0.2, units = "cm"),
                draw.ulim = TRUE,
                title.vjust = 1,
                direction = 'horizontal'
            )) +
        ylab("O'clock") +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(week_hourly_activities$weekth) -1,
                       max(week_hourly_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(week_hourly_activities$weekth), 4),
            labels = monday_weekth_labels[
                1:length(seq(1, max(week_hourly_activities$weekth), 4))] ) +
        scale_y_reverse(
            limits = c(23 + 1, 0 - 1),
            expand = c(0, 0),
            breaks = seq(21, 0, -3),
            labels = seq(21, 0, -3)
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
            legend.position = c(0.45, -0.25),
            legend.background = element_blank(),
            # (top, right, bottom, left)
            plot.margin = unit(c(0.01, 0.3, 0.6, 0.08), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    
    count_int = count_i[i]
    
    if(i < 4){
        left = 0.08
    }else{
        left = 0.22
    }
    
    data2 <- week_activities_users[imputed_purpose == activity_categories[i], ]
    timeseries2 <- 
        ggplot(data = data2,
               aes(x = weekth, y = activities_per_user, group=1)) +
        geom_line(linetype = "solid", color = 'black', size = 0.2) +
        geom_vline(xintercept = lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = post_lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = mask_requirement, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = second_wave, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_text(x=lockdown - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int / 2,
                  label="lockdown", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=post_lockdown - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int/2,
                  label="post lockdown", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=mask_requirement - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int/2,
                  label="mask requirement", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=second_wave - 2,
                  y=(max(data2$activities_per_user) %/% count_int + 1) * count_int/2,
                  label="second wave", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        ylab('Count / (user * week)') +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(week_hourly_activities$weekth) -1,
                       max(week_hourly_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(week_hourly_activities$weekth), 4)
        ) +
        scale_y_continuous(
            limits = c(0, (max(data2$activities_per_user) %/% count_int + 1
            ) * count_int),
            expand = c(0, 0),
            breaks = seq(0, (max(data2$activities_per_user) %/% count_int + 1
            ) * count_int, 
            count_int),
            labels = seq(0, (max(data2$activities_per_user) %/% count_int + 1
            ) * count_int,
            count_int)/1 ) +
        theme(
            axis.title.y = element_text(
                size = 8, family = 'Times New Roman'),
            text = element_text(size = 8, family = 'Times New Roman'),
            axis.text.y = element_text(
                size = 8, family = 'Times New Roman', hjust = 1),
            axis.ticks = element_line(size = 0.1),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.spacing.x = unit(0, "cm"),
            legend.position = c(0.45, -0.25),
            legend.background = element_blank(),
            # (top, right, bottom, left)
            plot.margin = unit(c(0.2, 0.3, 0.02, left), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    
    gridExtra::grid.arrange(
        timeseries2, heatmap1, ncol = 1, heights = c(2.4, 6.4), newpage = TRUE)
    dev.off()
    print(i)
}




# Heatmap and time series of all activities (entire duration) ------------

i_inswiss <- FALSE
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

activity_duration <- function(starttime, endtime){
    starthour <- round(starttime, units="hours")
    endhour <- round(endtime, units="hours")
    
    if(starthour > endhour){
        endhour = endhour + 60*60
    }
    return(seq(starthour, endhour, "hour"))
}

# i = 103
# activity_duration(allnew_activities$started_at_local[i],
#                   allnew_activities$finished_at_local[i])
# allnew_activities[i, .(started_at_local, finished_at_local)]
# lubridate::hour(seq(
#     round(allnew_activities$started_at_local[i], units="hours"),
#     round(allnew_activities$finished_at_local[i], units="hours"), "hour"))

# for(i in 1:217){
#     i=48
#     ddd <- allnew_activities[((i-1)*10000) + (1:10000), ]
#     for(j in 1:100){
#         j = 4
#         ccc <- ddd[((j-1)*100) + (1:100), ]
#         for(k in 1:100){
#             k=37
#             eee <- ccc[k, ]
#             expanded_activities <- eee[
#                 , .(time = activity_duration(started_at_local, finished_at_local)),
#                 by = c('ID', 'user_id', 'started_at_local', 'finished_at_local',
#                        'imputed_purpose')
#                 ]
#             print(k)
#         }
#         expanded_activities <- ccc[
#             , .(time = activity_duration(started_at_local, finished_at_local)),
#             by = c('ID', 'user_id', 'started_at_local', 'finished_at_local',
#                    'imputed_purpose')
#             ]
#         print(j)
#     }
#     expanded_activities <- ddd[
#         , .(time = activity_duration(started_at_local, finished_at_local)),
#         by = c('ID', 'user_id', 'started_at_local', 'finished_at_local',
#                'imputed_purpose')
#         ]
#     print(i)
# }


expanded_activities <- allnew_activities[
    , .(time = activity_duration(started_at_local, finished_at_local)),
    by = c('ID', 'user_id', 'started_at_local', 'finished_at_local',
           'imputed_purpose')
]

save(expanded_activities,
     file = '/data/students/qigao/scratch/2_output_analysis/expanded_activities.RData')
load('/data/students/qigao/scratch/2_output_analysis/expanded_activities.RData')

expanded_activities$starttime <- lubridate::hour(expanded_activities$time)
expanded_activities$weekday <- lubridate::wday(expanded_activities$time, 
                                               week_start = 1)

#### calculate the #th of day and week
expanded_activities$dayth <- ceiling(as.numeric(difftime(
    expanded_activities$time,
    mobis_start_date, units='days')))
expanded_activities$weekth <- (expanded_activities$dayth - 1) %/% 7 + 1


#### aggregate activities for each week and hours
week_hourly_activities <- expanded_activities[
    , .(week_hour_N = .N, week_hour_user_N = length(unique(user_id)) ), 
    by = c('weekth', 'starttime', 'imputed_purpose')
    ]

week_activities_users <- expanded_activities[
    , .(week_N = .N, week_user_N = length(unique(user_id)) ), 
    by = c('weekth', 'imputed_purpose')
    ]
week_activities_users$activityhours_per_user <- 
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


#### plot the results

# i = 1
count_i <- c(25, 10, 10, 2, 2, 1, 4, 5)
freq_i <- c(0.02, 0.03, 0.03, 0.04, 0.03, 0.03, 0.03, 0.04)
for (i in 1:8){
    freq_int <- freq_i[i]
    data1 <- week_hourly_activities[imputed_purpose == activity_categories[i], ]
    
    png(
        paste('visualization/2_output_analysis/mobis_1.1.', i,
              ' week_hourly heatmap of ',
              activity_categories[i], '_entire_duration.png', sep = ''),
        width = 8.8, height = 8.8, units = 'cm', res = 1200)
    heatmap1 = ggplot() +
        geom_tile(
            data = data1,
            aes(x = weekth, y = starttime, fill = week_hour_freq),
            colour = "white") +
        geom_vline(xintercept = lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = post_lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = mask_requirement, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = second_wave, linetype = "dashed",
                   color = 'black', size = 0.2) +
        scale_fill_gradient(
            low = "white", high = "blue",
            limits = c(0, (max(data1$week_hour_freq) %/% freq_int + 1
                           ) * freq_int),
            breaks=seq(0, (max(data1$week_hour_freq) %/% freq_int + 1
                           ) * freq_int, freq_int),
            labels=seq(0, (max(data1$week_hour_freq) %/% freq_int + 1
                           ) * freq_int, freq_int),
            guide = guide_colourbar(
                title = paste('Frequency of', activity_categories[i], 'hours'), 
                title.position = 'left',
                title.theme = element_text(size = 8, family = 'Times New Roman'),
                label.theme = element_text(size = 8, family = 'Times New Roman'),
                barwidth = unit(
                    0.8 * (length(seq(0, (max(data1$week_hour_freq
                                              ) %/% freq_int + 1
                    ) * freq_int, freq_int)) - 1), units = "cm"),
                barheight = unit(0.2, units = "cm"),
                draw.ulim = TRUE,
                title.vjust = 1,
                direction = 'horizontal'
            )) +
        ylab("O'clock") +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(week_hourly_activities$weekth) -1,
                       max(week_hourly_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(week_hourly_activities$weekth), 4),
            labels = monday_weekth_labels[
                1:length(seq(1, max(week_hourly_activities$weekth), 4))] ) +
        scale_y_reverse(
            limits = c(23 + 1, 0 - 1),
            expand = c(0, 0),
            breaks = seq(21, 0, -3),
            labels = seq(21, 0, -3)
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
            legend.position = c(0.45, -0.25),
            legend.background = element_blank(),
            # (top, right, bottom, left)
            plot.margin = unit(c(0.01, 0.3, 0.6, 0.2), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    
    count_int = count_i[i]
    if(i == 1){
        left = 0.06
    }else{
        left = 0.2
    }
    data2 <- week_activities_users[imputed_purpose == activity_categories[i], ]
    timeseries2 <- 
        ggplot(data = data2,
               aes(x = weekth, y = activityhours_per_user, group=1)) +
        geom_line(linetype = "solid", color = 'black', size = 0.2) +
        geom_vline(xintercept = lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = post_lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = mask_requirement, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = second_wave, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_text(x=lockdown - 2,
                  y=(max(data2$activityhours_per_user) %/% count_int + 1
                     ) * count_int / 2,
                  label="lockdown", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=post_lockdown - 2,
                  y=(max(data2$activityhours_per_user) %/% count_int + 1
                     ) * count_int/2,
                  label="post lockdown", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=mask_requirement - 2,
                  y=(max(data2$activityhours_per_user) %/% count_int + 1
                     ) * count_int/2,
                  label="mask requirement", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=second_wave - 2,
                  y=(max(data2$activityhours_per_user) %/% count_int + 1
                     ) * count_int/2,
                  label="second wave", angle = 90, color = 'gray',
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        ylab('Hours / (user * week)') +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(week_hourly_activities$weekth) -1,
                       max(week_hourly_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(week_hourly_activities$weekth), 4)
        ) +
        scale_y_continuous(
            limits = c(0, (max(data2$activityhours_per_user) %/% count_int + 1
            ) * count_int),
            expand = c(0, 0),
            breaks = seq(0, (max(data2$activityhours_per_user) %/% count_int + 1
            ) * count_int, 
            count_int),
            labels = seq(0, (max(data2$activityhours_per_user) %/% count_int + 1
            ) * count_int,
            count_int)/1 ) +
        theme(
            axis.title.y = element_text(
                size = 8, family = 'Times New Roman', vjust = 1),
            text = element_text(size = 8, family = 'Times New Roman'),
            axis.text.y = element_text(
                size = 8, family = 'Times New Roman', hjust = 1),
            axis.ticks = element_line(size = 0.1),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.spacing.x = unit(0, "cm"),
            legend.position = c(0.45, -0.25),
            legend.background = element_blank(),
            # (top, right, bottom, left)
            plot.margin = unit(c(0.2, 0.3, 0.02, left), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    
    gridExtra::grid.arrange(
        timeseries2, heatmap1, ncol = 1, heights = c(2.4, 6.4), newpage = TRUE)
    dev.off()
    print(i)
}




# Heatmap and time series of all activities (entire duration)_cha --------




