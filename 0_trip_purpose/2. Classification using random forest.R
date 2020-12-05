

# Feature importance using random forests ----


source('~/mobis_code/source file and import packages.R')
load('/data/students/qigao/scratch/tp_inswiss_label.RData')

tp_train <- randomForest::randomForest(
    x = tp_inswiss_label[, .SD, .SDcol = tp_sf_all],
    y = tp_inswiss_label$labels,
    ntree = 100, mtry = ceiling(log2(length(tp_sf_all))+1 ) ,
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)

save(tp_train, file = '/data/students/qigao/scratch/tp_train.RData')

# caret::confusionMatrix(table(tp_train$y, tp_train$predicted))
# tp_train$confusion
# load('/data/students/qigao/scratch/tp_train.RData')

png('visualization/1. Feature importance in trip purpose imputation.png',
    width = 8.8, height = 10, units = 'cm', res = 600)
par(ps = 10, mar = c(2, 9, 0.1, 0.1), font = 1, family = 'Times New Roman')
plot(sort(tp_train$importance[, 10]), 1:length(tp_sf_all), type = 'o', pch = 20,
     xlab = NA, ylab = NA, main = NA, lwd = 0.8, cex = 0.6,
     xlim = c(0, 90000), xaxt='n', yaxt='n')
title(xlab = 'Mean decrease in Gini', cex.lab = 0.8, line = 1)
title(ylab = 'Features', cex.lab = 0.8, line = 8)
axis(1, at = seq(0, 80000, 20000), 
     labels = FALSE,
     tick = T, lwd.ticks = 1, tck = -0.01,
     cex.axis = 0.8, mgp = c(0, 0.3, 0))
text(x = seq(0, 80000, 20000), 
     y = -0.5, 
     labels = seq(0, 80000, 20000), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = seq(1, length(tp_sf_all), 1),
     label = sapply(names(sort(tp_train$importance[, 10])),
                    function(x) mapping_var[['meaning']][
                        which(mapping_var[['variable']] == x)]),
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8,
     mgp = c(0, 0.5, 0))
dev.off()


# select different set of features ----

feature_importance <- names(sort(tp_train$importance[, 10], decreasing = TRUE) )

tp_subset_features <- list(
    top6 = 0, top12 = 0, no_personal = 0, no_activities = 0, no_cluster = 0)

tp_subset_features$top6 <- randomForest::randomForest(
    x = tp_inswiss_label[
        , .SD, 
        .SDcol = feature_importance[1:6]],
    y = tp_inswiss_label$labels,
    ntree = 100, 
    mtry = ceiling(log2(length(
        feature_importance[1:6]))+1 ) ,
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)

tp_subset_features$top12 <- randomForest::randomForest(
    x = tp_inswiss_label[
        , .SD, 
        .SDcol = feature_importance[1:12]],
    y = tp_inswiss_label$labels,
    ntree = 100, 
    mtry = ceiling(log2(length(
        feature_importance[1:12]))+1 ) ,
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)

tp_subset_features$no_personal <- randomForest::randomForest(
    x = tp_inswiss_label[
        , .SD, 
        .SDcol = 
            feature_importance[!feature_importance %in% participants_features]],
    y = tp_inswiss_label$labels,
    ntree = 100, 
    mtry = ceiling(log2(length(
        feature_importance[!feature_importance %in% participants_features]))+1) ,
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)

tp_subset_features$no_activities <- randomForest::randomForest(
    x = tp_inswiss_label[
        , .SD, 
        .SDcol = 
            feature_importance[!feature_importance %in% activities_features]],
    y = tp_inswiss_label$labels,
    ntree = 100, 
    mtry = ceiling(log2(length(
        feature_importance[!feature_importance %in% activities_features]))+1),
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)

tp_subset_features$no_cluster <- randomForest::randomForest(
    x = tp_inswiss_label[
        , .SD, 
        .SDcol = feature_importance[
            !feature_importance %in% activities_clusterfeatures]],
    y = tp_inswiss_label$labels,
    ntree = 100, 
    mtry = ceiling(log2(length(
        feature_importance[
            !feature_importance %in% activities_clusterfeatures]))+1 ),
    importance = TRUE, do.trace = 1,
    keep.forest = FALSE
)


save(tp_subset_features, 
     file = '/data/students/qigao/scratch/tp_subset_features.RData')

png(
    'visualization/2 Impact of feature selection on trip purpose imputation.png',
    width = 8.8, height = 5, units = 'cm', res = 1200)
# (bottom, left, top, right)
par(ps = 8, mar = c(2.1, 2, 0.1, 0.6), font = 1, family = 'Times New Roman')
plot(1:9, 1 - tp_train$err.rate[100, ], type = 'l', lwd = 1, lty = 1,
     col = 'black', xlab = NA, ylab = NA, main = NA, 
     ylim = c(0.2, 1), xaxt='n', yaxt='n')
# lines(1:9, 1 - tp_subset_features$top6$err.rate[100, ], 
#       type = 'l', lwd = 1, lty = 2, col = 'steelblue2')
# lines(1:9, 1 - tp_subset_features$top12$err.rate[100, ], 
#       type = 'l', lwd = 1, lty = 2, col = 'steelblue2')
lines(1:9, 1 - tp_subset_features$no_personal$err.rate[100, ], 
      type = 'l', lwd = 1, lty = 2, col = 'red')
lines(1:9, 1 - tp_subset_features$no_activities$err.rate[100, ], 
      type = 'l', lwd = 1, lty = 2, col = 'steelblue2')
lines(1:9, 1 - tp_subset_features$no_cluster$err.rate[100, ], 
      type = 'l', lwd = 1, lty = 2, col = 'grey')
title(xlab = 'Activity categories', line = 1.15)
title(ylab = 'Accuracy', line = 1.3)
axis(1, at = seq(1, 9, 1), labels = FALSE,
     tick = T, lwd.ticks = 1, tck = -0.01,
     cex.axis = 1, mgp = c(0, 0.6, 0))
text(x = seq(1, 9, 1), y = 0.09, labels = c('Overall', activity_categories), 
     srt = 30, cex = 1, xpd = TRUE)
axis(2, at = seq(0.2, 1, 0.1),
     tick = T, lwd.ticks = 1, las = 1.5, tck = -0.01, cex.axis = 1, 
     mgp = c(0, 0.4, 0))
legend(0.5, 0.65, 
       legend = c('All features', 
                  # 'Top 6 features', 
                  # 'Top 12 features', 
                  'No personal info', 
                  'No activity info', 
                  'No cluster info'), 
       col = c('black', 'red', 'steelblue2', 'grey'), 
       lty = c(1, 2, 2, 2, 2), cex = 1, lwd = 1,
       box.lty = 0, y.intersp = 0.8, bg = NA, seg.len = 1)
dev.off()


# features selection based on Boruta----

system.time(
    tp_fs_boruta <- Boruta::Boruta(
        x = tp_inswiss_label[, .SD, .SDcol = tp_sf_all],
        y = tp_inswiss_label$labels,
        maxRuns = 1000,
        doTrace = 3
    )
)

save(tp_fs_boruta, file = '/data/students/qigao/scratch/tp_fs_boruta.RData')




# Spatial distribution of accuracy rate ----

source('~/mobis_code/r_module/point_count.R', chdir = TRUE)
swiss_kanton <- rgdal::readOGR(
    '~/scratch/geo/swiss/ch_boundaries/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp')
swiss_kanton <- sp::spTransform(
    swiss_kanton, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84') )

# Infer in which canto are they
activities_sf <- tp_inswiss_label[, .(ID, lon, lat)]
sp::coordinates(activities_sf) <- ~ lon + lat
sp::proj4string(activities_sf) <- '+proj=longlat +datum=WGS84 +ellps=WGS84'
point_polygon <- sp::over(activities_sf, swiss_kanton)
tp_inswiss_label$inkanton <- as.character(point_polygon$NAME)

tp_imputed <- as.data.frame(
    tp_inswiss_label[
        tp_inswiss_label$labels == tp_subset_features$no_personal$predicted &
            ! is.na(tp_inswiss_label$inkanton), 
        c('lon', 'lat')]
)
tp_total <- as.data.frame(
    tp_inswiss_label[! is.na(tp_inswiss_label$inkanton), c('lon', 'lat')]
)

imputed_density <- point_count(
    lon = tp_imputed$lon, lat = tp_imputed$lat, 
    lon_range = c(5.9, 10.55), lat_range = c(45.78, 47.85), n = c(100, 100))
total_density <- point_count(
    lon = tp_total$lon, lat = tp_total$lat, 
    lon_range = c(5.9, 10.55), lat_range = c(45.78, 47.85), n = c(100, 100))

accuracy_rate <- imputed_density
accuracy_rate@data@values <- imputed_density@data@values / 
    total_density@data@values
accuracy_rate_spdf <- as.data.frame(as(accuracy_rate, "SpatialPixelsDataFrame"))
accuracy_rate_spdf$layer_brk <- cut(
    accuracy_rate_spdf$layer, 
    breaks = c(-0.001, 0.2, 0.4, 0.6, 0.8, 1), 
    labels = c(20, 40, 60, 80, 100))

total_density@data@values[total_density@data@values == 0] <- NA
total_density@data@values[!is.na(total_density@data@values)] <- 
    log(total_density@data@values[
        !is.na(total_density@data@values)])
total_density_spdf <- as.data.frame(as(total_density, "SpatialPixelsDataFrame"))
total_density_spdf$layer_brk <- cut(
    total_density_spdf$layer, 
    breaks = c(-0.001, 2, 4, 6, 8, 10), 
    labels = c(expression(e^{2}), expression(e^{4}), expression(e^{6}), 
               expression(e^{8}), expression(e^{10})))

png('visualization/3 Spatial distribution of accuracy rate around Switzerland.png',
    width = 8.8, height = 6.4, units = 'cm', res = 1200)
ggplot() +
    geom_tile(data = accuracy_rate_spdf,
              aes(x = x, y = y, fill = layer_brk)) +
    geom_polygon(aes(long, lat, group = group), fill = NA,
                 color = 'black', size = 0.05, data = swiss_kanton) +
    scale_x_continuous(
        limits = c(5.9, 10.55), expand = c(0, 0), breaks = seq(6, 10, 1),
        labels = paste(seq(6, 10, 1), '°E', sep = '') ) +
    scale_y_continuous(
        limits = c(45.78, 47.85), expand = c(0, 0), breaks = seq(46, 47.5, 0.5),
        labels = paste(seq(46, 47.5, 0.5), '°N', sep = '')) +
    theme(
        panel.border = element_rect( colour = "black", fill=NA, size=0.1),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 8, 
                                   family = 'Times New Roman', vjust = 0),
        axis.text.y = element_text(size = 8, 
                                   family = 'Times New Roman', hjust = 1),
        axis.ticks = element_line(size = 0.2),
        axis.title = element_blank(),
        panel.spacing.x = unit(0, "cm"),
        legend.position = c(0.45, -0.2), 
        legend.direction = 'horizontal', 
        legend.background = element_blank(),
        # (top, right, bottom, left)
        plot.margin = unit(c(0.01, 0.01, 1, 0.18), "cm") ) + 
    scale_fill_manual(
        values = colorRampPalette(c("black", "steelblue1"))(5),
        guide = guide_legend(
            keyheight = unit(0.2, units = "cm"),
            keywidth = unit(1, units = "cm"),
            title.position = 'top', title = 'Accuracy (%)',
            title.vjust = -0.5, title.hjust = 0.5,
            nrow = 1, byrow = T,
            label.position = "bottom", default.unit = 'cm',
            title.theme = element_text(size = 8, family = 'Times New Roman'),
            label.theme = element_text(size = 8, family = 'Times New Roman'),
            reverse = FALSE, label.vjust = 2, label.hjust = 1.2)
    )
dev.off()


png('visualization/4 Spatial distribution of labeled activities around Switzerland.png',
    width = 8.8, height = 6.4, units = 'cm', res = 1200)
ggplot() +
    geom_tile(data = total_density_spdf,
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
            size = 8, family = 'Times New Roman', vjust = 0),
        axis.text.y = element_text(
            size = 8, family = 'Times New Roman', hjust = 1),
        axis.ticks = element_line(size = 0.2),
        axis.title = element_blank(),
        panel.spacing.x = unit(0, "cm"),
        legend.position = c(0.45, -0.2), 
        legend.direction = 'horizontal', 
        legend.background = element_blank(),
        # (top, right, bottom, left)
        plot.margin = unit(c(0.01, 0.01, 1, 0.18), "cm") ) + 
    scale_fill_manual(
        values = colorRampPalette(c("black", "steelblue1"))(5),
        labels = c(expression(e^2), expression(e^4), expression(e^6), 
                   expression(e^8), expression(e^10)),
        guide = guide_legend(
            keyheight = unit(0.2, units = "cm"),
            keywidth = unit(1, units = "cm"),
            title.position = 'top', title = 'Number of activities',
            title.vjust = -0.5, title.hjust = 0.5,
            nrow = 1, byrow = T,
            label.position = "bottom", default.unit = 'cm',
            title.theme = element_text(size = 8, family = 'Times New Roman'),
            label.theme = element_text(size = 8, family = 'Times New Roman'),
            reverse = FALSE, label.vjust = 2, label.hjust = 1.2)
    )
dev.off()


