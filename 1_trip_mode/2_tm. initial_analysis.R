


# Raw data cleaning -------------------------------------------------------

source('~/mobis_code/source file and import packages.R')

load("scratch/all_legs.RData")
swiss_kanton <- rgdal::readOGR(
    '~/scratch/geo/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp')
swiss_kanton <- sp::spTransform(
    swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )

# Infer in which canto are they
start_legs_sf <- all_legs[, .(ID, start_lon, start_lat)]
sp::coordinates(start_legs_sf) <- ~ start_lon + start_lat
sp::proj4string(start_legs_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'
start_point_polygon <- sp::over(start_legs_sf, swiss_kanton)
all_legs$start_inkanton <- as.character(start_point_polygon$NAME)

end_legs_sf <- all_legs[, .(ID, end_lon, end_lat)]
sp::coordinates(end_legs_sf) <- ~ end_lon + end_lat
sp::proj4string(end_legs_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'
end_point_polygon <- sp::over(end_legs_sf, swiss_kanton)
all_legs$end_inkanton <- as.character(end_point_polygon$NAME)


tm_inswiss <- all_legs[
    (!is.na(start_inkanton)) & (!is.na(end_inkanton)), 
    -c('all_predicted', 'rf_prediction', 'c50_prediction', 
       'mars_prediction', 'modes', 'imputed_mode')]
tm_inswiss_label <- droplevels(tm_inswiss[labels != 'Unknown', ])
tm_inswiss_label$labels <- factor(tm_inswiss_label$labels, 
                                  levels = mode_categories)

tm_inswiss_label = tm_inswiss_label[
    , .SD, .SDcol = setdiff(
        colnames(tm_inswiss_label), c(
            participants_features, "treatment", "phase", "started_at", 
            "finished_at", "start_point", "end_point", "mid_point", 
            "type", "mode", "was_confirmed", "externality_health", 
            "externality_co2", "externality_congestion", 
            "externality_total", "in_switzerland", "start_sf", "end_sf"))
    ]

save(tm_inswiss_label, 
     file = '/data/students/qigao/scratch/tm_inswiss_label.RData')


# Feature importance using random forests -------------------------------

load('/data/students/qigao/scratch/tm_inswiss_label.RData')

tm_train <- randomForest::randomForest(
    x = tm_inswiss_label[, .SD, .SDcol = tm_sf_all],
    y = tm_inswiss_label$labels,
    ntree = 100, mtry = ceiling(log2(length(tm_sf_all))+1 ) ,
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)

save(tm_train, file = '/data/students/qigao/scratch/tm_train.RData')


png('visualization/tm1. Feature importance in trip mode imputation.png',
    width = 8.8, height = 15, units = 'cm', res = 600)
par(ps = 10, mar = c(2, 9, 0.1, 0.1), font = 1, family = 'Times New Roman')
plot(sort(tm_train$importance[, 9]), 1:length(tm_sf_all), type = 'o', pch = 20,
     xlab = NA, ylab = NA, main = NA, lwd = 0.8, cex = 0.6,
     xlim = c(0, 250000), xaxt='n', yaxt='n')
title(xlab = 'Mean decrease in Gini', cex.lab = 0.8, line = 1)
title(ylab = 'Features', cex.lab = 0.8, line = 8)
axis(1, at = seq(0, 250000, 50000), 
     labels = FALSE,
     tick = T, lwd.ticks = 1, tck = -0.01,
     cex.axis = 0.8, mgp = c(0, 0.3, 0))
text(x = seq(0, 250000, 50000), 
     y = -0.5, 
     labels = seq(0, 250000, 50000), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = seq(1, length(tm_sf_all), 1),
     label = sapply(names(sort(tm_train$importance[, 9])),
                    function(x) mapping_var_tm[['meaning']][
                        which(mapping_var_tm[['variable']] == x)]),
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8,
     mgp = c(0, 0.5, 0))
dev.off()


# features selection based on Boruta----

system.time(
    tm_fs_boruta <- Boruta::Boruta(
        x = tm_inswiss_label[, .SD, .SDcol = tm_sf_all],
        y = tm_inswiss_label$labels,
        maxRuns = 1000,
        doTrace = 3
    )
)

save(tm_fs_boruta, file = '/data/students/qigao/scratch/tm_fs_boruta.RData')




