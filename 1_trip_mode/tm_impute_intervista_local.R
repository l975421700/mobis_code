
# Main code to do trip mode imputation for intervista data


# before you run ----------------------------------------------------------

# number of rows in csv data to read, Inf for all,  finite number for code test
n_max = Inf

# Input packages, functions, and namelist for all subsequent sections ----

setwd("/data/students/qigao/mobis_analysis")
source('r/eth_purpose_imputation/r_module/install_packages.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/wkb2lonlat.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/utc2local_time.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/hierarchical_clustering.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/namelist.R', chdir = TRUE) 
source('r/eth_purpose_imputation/r_module/multi_classification_kfold_cv.R', chdir = TRUE) 
source('/data/students/qigao/mobis_analysis/r/gao/mobis_code/r_module/point_count.R', echo=TRUE)

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


# data 'legs' cleaning ----

GeoPoints <- read_csv("/data/mobis/data/intervista_data/Datenlieferung/GeoPoints.csv")

SozioInfo <- read_csv("/data/mobis/data/intervista_data/Datenlieferung/SozioInfo.csv")

plot(1:24, as.numeric(table(as.numeric(substr(GeoPoints$GeoDateLocal, 12, 13) ) )))

# Spatial distribution of misclassified trip purpose ----

swiss_kanton <- rgdal::readOGR('/data/students/qigao/scratch/geo/swiss_kanton.shp')

iv_point <- GeoPoints[GeoPoints$Domestic == TRUE, ]
iv_point_density <- point_count(
    lon = iv_point$Longitude, lat = iv_point$Latitude, 
    lon_range = c(5.9, 10.55), lat_range = c(45.78, 47.85), n = c(100, 100))
iv_point_density@data@values[iv_point_density@data@values == 0] <- NA


iv_point_density@data@values[!is.na(iv_point_density@data@values)] <- 
    log(iv_point_density@data@values[
        !is.na(iv_point_density@data@values)])

iv_point_density@data@values[which(iv_point_density@data@values > 10)] <- 10

iv_point_density_spdf <- as.data.frame(as(
    iv_point_density, "SpatialPixelsDataFrame"))
iv_point_density_spdf$layer_brk <- cut(
    iv_point_density_spdf$layer, 
    breaks = c(-0.001, 2, 4, 6, 8, 10), 
    labels = c(expression(e^{2}), expression(e^{4}), expression(e^{6}), 
               expression(e^{8}), expression(e^{10})))



png('/data/students/qigao/visualization/intervista_point_spatial_density.png',
    width = 15, height = 11, units = 'cm', res = 600)
ggplot() +
    geom_tile(data = iv_point_density_spdf,
              aes(x = x, y = y, fill = layer_brk)) +
    geom_polygon(aes(long, lat, group = group), fill = NA,
                 color = 'black', size = 0.05, data = swiss_kanton) +
    scale_x_continuous(
        limits = c(5.9, 10.55), expand = c(0, 0),
        breaks = seq(6, 10, 1),
        labels = paste(seq(6, 10, 1), '°E', sep = '') ) +
    scale_y_continuous(
        limits = c(45.78, 47.85), expand = c(0, 0),
        breaks = seq(46, 47.5, 0.5),
        labels = paste(seq(46, 47.5, 0.5), '°N', sep = '')) +
    theme(
        panel.border = element_rect(
            colour = "black", fill=NA, size=0.1),
        panel.background = element_blank(),
        axis.text.x = element_text(
            size = 16, family = 'Times New Roman', vjust = 0),
        axis.text.y = element_text(
            size = 16, family = 'Times New Roman', hjust = 1),
        axis.ticks = element_line(size = 0.2),
        axis.title = element_blank(),
        panel.spacing.x = unit(0, "cm"),
        legend.position = c(0.45, -0.22), 
        legend.direction = 'horizontal', 
        legend.background = element_blank(),
        # (top, right, bottom, left)
        plot.margin = unit(c(0.2, 0.1, 2, 0.3), "cm") ) + 
    scale_fill_manual(
        values = colorRampPalette(c("black", "steelblue1"))(5),
        labels = c(expression(e^2), expression(e^4), expression(e^6), 
                   expression(e^8), expression(e^10)),
        guide = guide_legend(
            keyheight = unit(0.25, units = "cm"),
            keywidth = unit(2, units = "cm"),
            title.position = 'top', title = 'Number of activities',
            title.vjust = -1.2, title.hjust = 0.6,
            nrow = 1, byrow = T,
            label.position = "bottom", default.unit = 'cm',
            title.theme = element_text(size = 20, family = 'Times New Roman'),
            label.theme = element_text(size = 20, family = 'Times New Roman'),
            reverse = FALSE, label.vjust = 3, label.hjust = 1.1)
    )
dev.off()


# Check points ----














