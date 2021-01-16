
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


# plot_the_results -------------------------------------------------------


################################ order the minutes share
weekly_minutes = weekly_minutes[order(weekth, hour_minute_th)]










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


################################ plot the minutes share

png(
    'visualization/01_minutes_share/1.3.0_minutes_share_of_activities_change.png',
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


# plot the legend --------------------------------------------------------

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











