

# Import packages -------------------------------------------------------

mobis_analysis_directory <- '/data/students/qigao/mobis_analysis'
source(paste(mobis_analysis_directory, 
             '/r/eth_purpose_imputation/r_module/install_packages.R',
             sep = ''), chdir = TRUE)
source(paste(mobis_analysis_directory, 
             '/r/eth_purpose_imputation/r_module/utc2local_time.R',
             sep = ''), chdir = TRUE)
source(paste(mobis_analysis_directory, 
             '/r/eth_purpose_imputation/r_module/namelist.R',
             sep = ''), chdir = TRUE)
source(paste(mobis_analysis_directory, 
             '/r/eth_purpose_imputation/r_module/wkb2lonlat.R',
             sep = ''), chdir = TRUE)

library('readr')
library(plyr)
suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('stringi'))
suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('data.table'))
library(ggplot2)

# imput activities data -------------------------------------------------------

load("/data/mobis/data/enrichments/run_gao/imputed_activity_incremental.RData")

activities <- readr::read_delim(
    activities_path, delim = ',', n_max = Inf)

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
activities <- readr::read_delim(
    activities_path, delim = ',', n_max = Inf)

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

activities_path = '/data/mobis/data/csv/activities.csv'
covid_activities_path1 = '/data/mobis/data/covid/phase_4/covid_activities.csv'
covid_activities_path2 = '/data/mobis/data/covid/phase_5/covid_activities.csv'
covid_activities_path3 = '/data/mobis/data/covid/phase_6/covid_activities.csv'
covid_activities_path4 = '/data/mobis/data/covid/phase_7/covid_activities.csv'

activities <- readr::read_delim(
    activities_path, delim = ',', n_max = Inf)
covid_activities1 <- readr::read_delim(
    covid_activities_path1, delim = ',', n_max = Inf)
covid_activities2 <- readr::read_delim(
    covid_activities_path2, delim = ',', n_max = Inf)
covid_activities3 <- readr::read_delim(
    covid_activities_path3, delim = ',', n_max = Inf)
covid_activities4 <- readr::read_delim(
    covid_activities_path4, delim = ',', n_max = Inf)

# combine original mobis and covid activities
all_activities <- rbind(
    activities, covid_activities1, covid_activities2,
    covid_activities3, covid_activities4)

all_activities <- as.data.table(all_activities)
colnames(all_activities)[colnames(all_activities) == "activity_id"] <- 'ID'

# dim(all_activities)[1]
# length(which(all_activities$ID %in% imputed_activity_incremental$ID))

all_activities <- data.table::merge.data.table(
    all_activities, imputed_activity_incremental[, c("ID", "imputed_purpose")],
    all.x = TRUE
)

all_activities <- cbind(all_activities, wkb2lonlat(all_activities$geometry))
all_activities$started_at_local <- utc2local_time(
    all_activities$started_at, all_activities$sf)

all_activities$starttime <- lubridate::hour(all_activities$started_at_local)
all_activities$weekday <- lubridate::wday(all_activities$started_at_local, 
                                          week_start = 1)

# heat map based on weekday-hours ----

week_hourly_activities <- all_activities[
    , .(.N), 
    by = c('weekday', 'starttime', 'imputed_purpose')
    ]
week_hourly_activities$weekday <- factor(
    week_hourly_activities$weekday,
    levels=unique(week_hourly_activities$weekday))
# sum(week_hourly_activities$N)

# Time Based Heatmaps: https://www.littlemissdata.com/blog/heatmaps
ggplot(week_hourly_activities[imputed_purpose == 'Home', ],
       aes(starttime, weekday)) + 
    geom_tile(aes(fill = N), colour = "white", na.rm = TRUE) +
    scale_fill_gradient(low = "#d8e1cf", high = "#438484") +  
    guides(fill=guide_legend(title="Total counts")) +
    theme_bw() + theme_minimal() + 
    labs(title = "Week-hourly distribution of activities 'Home'",
         x = "Hour", y = "Day of Week") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# data preprocess ----

# Time Based Heatmaps: https://www.littlemissdata.com/blog/heatmaps
# Home Work Leisure   Shopping      Other     Errand Assistance  Education 

# lubridate::wday(min(all_activities$started_at_local), week_start = 1)

mobis_start_date <- as.POSIXlt("2019-09-02 00:00:00 CEST")

# add Kanton information
swiss_kanton <- rgdal::readOGR(
    '~/scratch/geo/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp')
swiss_kanton <- sp::spTransform(
    swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )
start_activities_sf <- all_activities[, .(ID, lon, lat)]
sp::coordinates(start_activities_sf) <- ~ lon + lat
sp::proj4string(start_activities_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'
start_point_polygon <- sp::over(start_activities_sf, swiss_kanton)
all_activities$start_inkanton <- as.character(start_point_polygon$NAME)

# order activity based on date
tp_inswiss <- all_activities[!is.na(start_inkanton), ]
tp_inswiss$dayth <- ceiling(as.numeric(difftime(
    tp_inswiss$started_at_local,
    mobis_start_date, units='days')))
tp_inswiss$weekth <- (tp_inswiss$dayth - 1) %/% 7 + 1

monday_weekth <- seq(as.Date(min(tp_inswiss$started_at_local)),
    to=as.Date(max(tp_inswiss$started_at_local)), by='1 weeks')

# aggregate for each week and hours
weekth_hour_activities <- tp_inswiss[
    , .(week_hour_N = .N), 
    by = c('weekth', 'starttime', 'imputed_purpose')
    ]

weekth_activities <- tp_inswiss[
    , .(week_N = .N), 
    by = c('weekth', 'imputed_purpose')
    ]

weekth_hour_activities <- data.table::merge.data.table(
    weekth_hour_activities, weekth_activities,
    by = c('weekth', 'imputed_purpose')
)

weekth_hour_activities$week_hour_freq <- 
    weekth_hour_activities$week_hour_N / weekth_hour_activities$week_N

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


# Plot heatmap and time series ----

library(gridExtra)
png('visualization/mobis1. hourly heatmap of activity_home distribution.png',
    width = 8.8, height = 8.8, units = 'cm', res = 1200)
heatmap1 = ggplot() +
    geom_tile(
        data = weekth_hour_activities[imputed_purpose == 'Home', ],
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
        # low = "#d8e1cf", high = "#438484",
        low = "white", high = "blue",
        limits = c(0, 0.15),
        breaks=seq(0, 0.15, 0.05),
        labels=c("0","0.05", "0.1", "0.15"),
        guide = guide_colourbar(
            title = 'Frequency of Home', title.position = 'left',
            title.theme = element_text(size = 8, family = 'Times New Roman'),
            label.theme = element_text(size = 8, family = 'Times New Roman'),
            barwidth = unit(3, units = "cm"),
            barheight = unit(0.2, units = "cm"),
            draw.ulim = TRUE,
            title.vjust = 1,
            direction = 'horizontal'
        )) +
    ylab("Clock") +
    theme_bw() + theme_minimal() +
    scale_x_continuous(
        limits = c(min(weekth_hour_activities$weekth) -1,
                   max(weekth_hour_activities$weekth) +1),
        expand = c(0, 0),
        breaks = seq(1, 64, 4),
        labels = monday_weekth_labels ) +
    scale_y_continuous(
        limits = c(0 - 1, 23 + 1),
        expand = c(0, 0),
        breaks = seq(0, 23, 4),
        labels = seq(0, 23, 4) ) +
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
        plot.margin = unit(c(0.01, 0.01, 0.6, 0.2), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect( colour = "black", fill=NA, size=0.1),
        panel.background = element_blank()
    )
timeseries2 <- 
    ggplot(data = weekth_activities[imputed_purpose == 'Home', ],
           aes(x = weekth_activities[imputed_purpose == 'Home', ]$weekth,
               y = weekth_activities[imputed_purpose == 'Home', ]$week_N,
               group=1)) +
    geom_line(linetype = "solid", color = 'black', size = 0.2) +
    geom_vline(xintercept = lockdown, linetype = "dashed",
               color = 'black', size = 0.2) +
    geom_vline(xintercept = post_lockdown, linetype = "dashed",
               color = 'black', size = 0.2) +
    geom_vline(xintercept = mask_requirement, linetype = "dashed",
               color = 'black', size = 0.2) +
    geom_vline(xintercept = second_wave, linetype = "dashed",
               color = 'black', size = 0.2) +
    geom_text(x=lockdown - 2, y=15000, label="lockdown", angle = 90,
              fontface = 'plain', size = 2.5, family = 'Times New Roman') + 
    geom_text(x=post_lockdown - 2, y=15000, label="post lockdown", angle = 90,
              fontface = 'plain', size = 2.5, family = 'Times New Roman') + 
    geom_text(x=mask_requirement - 2, y=15000, label="mask requirement", angle = 90,
              fontface = 'plain', size = 2.5, family = 'Times New Roman') + 
    geom_text(x=second_wave - 2, y=15000, label="second wave", angle = 90,
              fontface = 'plain', size = 2.5, family = 'Times New Roman') + 
    ylab(expression(paste("Count [", 10^{3}, ']'))) +
    theme_bw() + theme_minimal() +
    scale_x_continuous(
        limits = c(min(weekth_hour_activities$weekth) -1,
                   max(weekth_hour_activities$weekth) +1),
        expand = c(0, 0),
        breaks = seq(1, 64, 4)
        # labels = monday_weekth_labels
    ) +
    scale_y_continuous(
        limits = c(0, (max(weekth_activities[
            imputed_purpose == 'Home', ]$week_N) %/% 5000 + 1) * 5000),
        expand = c(0, 0),
        breaks = seq(0, (max(weekth_activities[
            imputed_purpose == 'Home', ]$week_N) %/% 5000 + 1) * 5000, 5000),
        labels = seq(0, (max(weekth_activities[
            imputed_purpose == 'Home', ]$week_N) %/% 5000 + 1) * 5000,
            5000)/1000 ) +
    theme(
        axis.title.y = element_text(
            size = 8, family = 'Times New Roman'),
        # axis.text.x = element_text(
        #     size = 8, family = 'Times New Roman', vjust = 0.75, angle = 45),
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
        plot.margin = unit(c(0.12, 0.01, 0.02, 0.08), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect( colour = "black", fill=NA, size=0.1),
        panel.background = element_blank()
    )

gridExtra::grid.arrange(
    timeseries2, heatmap1, ncol = 1, heights = c(2.4, 6.4), newpage = TRUE)
dev.off()


# Plot heatmap and time series general activities ----

library(gridExtra)

activities2plot <- c(
    'Home', 'Work', 'Leisure', 'Shopping', 'Other', 'Errand', 'Assistance',
    'Education' 
)

# i = 2

for (i in 1:8){
    data1 <- weekth_hour_activities[imputed_purpose == activities2plot[i], ]
    
    png(
        paste('visualization/mobis_1.0.', i, ' week_hourly heatmap of activity ',
              activities2plot[i], '.png', sep = ''),
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
                title = paste('Frequency of', activities2plot[i]), 
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
        ylab("Clock") +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(weekth_hour_activities$weekth) -1,
                       max(weekth_hour_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(weekth_hour_activities$weekth), 4),
            labels = monday_weekth_labels[
                1:length(seq(1, max(weekth_hour_activities$weekth), 4))] ) +
        scale_y_continuous(
            limits = c(0 - 1, 23 + 1),
            expand = c(0, 0),
            breaks = seq(0, 23, 4),
            labels = seq(0, 23, 4) ) +
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
            plot.margin = unit(c(0.01, 0.01, 0.6, 0.2), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    
    data2 <- weekth_activities[imputed_purpose == activities2plot[i], ]
    timeseries2 <- 
        ggplot(data = data2,
               aes(x = weekth, y = week_N, group=1)) +
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
                  y=(max(data2$week_N) %/% 5000 + 1) * 2500,
                  label="lockdown", angle = 90,
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=post_lockdown - 2,
                  y=(max(data2$week_N) %/% 5000 + 1) * 2500,
                  label="post lockdown", angle = 90,
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=mask_requirement - 2,
                  y=(max(data2$week_N) %/% 5000 + 1) * 2500,
                  label="mask requirement", angle = 90,
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        geom_text(x=second_wave - 2,
                  y=(max(data2$week_N) %/% 5000 + 1) * 2500,
                  label="second wave", angle = 90,
                  fontface = 'plain', size = 2.4, family = 'Times New Roman') + 
        ylab(expression(paste("Count [", 10^{3}, ']'))) +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(weekth_hour_activities$weekth) -1,
                       max(weekth_hour_activities$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(weekth_hour_activities$weekth), 4)
        ) +
        scale_y_continuous(
            limits = c(0, (max(data2$week_N) %/% 5000 + 1) * 5000),
            expand = c(0, 0),
            breaks = seq(0, (max(data2$week_N) %/% 5000 + 1) * 5000, 5000),
            labels = seq(0, (max(data2$week_N) %/% 5000 + 1) * 5000,
                         5000)/1000 ) +
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
            plot.margin = unit(c(0.12, 0.01, 0.02, 0.08), "cm"),
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




