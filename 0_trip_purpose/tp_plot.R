

#### Impact of number of trees ----
source('2_code/r_module/namelist.R', chdir = TRUE) 
load('3_output/28_Apr/tp_rf_train_whole.RData')
load('3_output/28_Apr/purpose_data.RData')

tp_rf_train_whole$train$confusion
tp_rf_train_whole$train$err.rate[500, ]

png('4_visualization/3.1.1 Impact of number of trees on trip purpose derivation.png', 
    width = 12, height = 8, units = 'cm', res = 600)
par(ps = 20, mar = c(3, 4.5, 0.1, 0.1), font = 1, family = 'Times New Roman')
plot(1:500, 1-tp_rf_train_whole$train$err.rate[, 1], type = 'l', 
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0.75, 0.86), xaxt='n', yaxt='n')
title(xlab = 'Parameter N', line = 2)
title(ylab = 'Accuracy', line = 3)
axis(1, at = seq(0, 500, 100), tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.5, 0))
axis(2, at = seq(0.75, 0.85, 0.02), label = T, tick = T, lwd.ticks = 1, 
     las = 2, tck = -0.01, cex.axis = 0.8, mgp = c(0, 0.5, 0))
dev.off()	


#### Impact of parameters M ----

load('3_output/28_Apr/tp_rf_train_whole_mtry.RData')

tp_rf_train_whole_mtry_error_rate <- 0
for(i in 1: length(tp_rf_train_whole_mtry) ){
  tp_rf_train_whole_mtry_error_rate[i] <- 
    tp_rf_train_whole_mtry[[i]]$train$err.rate[100, 1]
}

png('4_visualization/3.1.2 Impact of parameters M on trip purpose derivation.png', 
    width = 12, height = 8, units = 'cm', res = 600)
par(ps = 20, mar = c(3, 4.5, 0.1, 0.1), font = 1, family = 'Times New Roman')
plot(1:30, 1 - tp_rf_train_whole_mtry_error_rate, type = 'l', 
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0.65, 0.86), xaxt='n', yaxt='n')
# abline(v = 100, col = 'black', lwd = 0.3)
title(xlab = 'Parameter M', line = 2)
title(ylab = 'Accuracy', line = 3)
axis(1, at = seq(0, 30, 5), tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.5, 0))
axis(2, at = seq(0.65, 0.85, 0.05), label = T, tick = T, lwd.ticks = 1, 
     las = 2, tck = -0.01, cex.axis = 0.8, mgp = c(0, 0.5, 0))
dev.off()	


#### Feature importance ----

source('2_code/r_module/namelist.R', chdir = TRUE) 
load('3_output/28_Apr/tp_rf_train_whole.RData')

png('4_visualization/3.1.3 Feature importance in trip purpose derivation.png', 
    width = 21, height = 25, units = 'cm', res = 600)
par(ps = 20, mar = c(3, 20, 0.1, 0.8), font = 1, family = 'Times New Roman')
plot(sort(tp_rf_train_whole$train$importance[, 10]), 1:30, type = 'o', pch = 20, 
     xlab = NA, ylab = NA, main = NA, 
     xlim = c(0, 50000), xaxt='n', yaxt='n')
title(xlab = 'Mean decrease in Gini', line = 2)
title(ylab = 'Selected features', line = 18)
axis(1, at = seq(0, 50000, 10000), tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.5, 0))
axis(2, at = seq(1, 30, 1), 
     label = sapply(names(sort(tp_rf_train_whole$train$importance[, 10])),
                    function(x) mapping_var[['meaning']][
                      which(mapping_var[['variable']] == x)]), 
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8, 
     mgp = c(0, 0.5, 0))
dev.off()	


#### Impact of Feature selection. all - withou prn - 20 sf - 6 sf - pca ----

load('3_output/28_Apr/tp_rf_train_whole.RData')
load('3_output/28_Apr/tp_rf_train_noprn.RData')
# load('3_output/28_Apr/tp_rf_train_20sf.RData')
load('3_output/28_Apr/tp_rf_train_6sf.RData')
load('3_output/28_Apr/tp_rf_train_pca.RData')
source('2_code/r_module/namelist.R', chdir = TRUE) 

# tp_rf_train_whole$train$err.rate[100, ]
# tp_rf_train_20sf$train$err.rate[100, ]
# tp_rf_train_6sf$train$err.rate[100, ]
# tp_rf_train_noprn$train$err.rate[100, ]
# tp_rf_train_pca$train$err.rate[100, ]

png(
  '4_visualization/3.1.4 Impact of feature selection on trip purpose derivation.png',
  width = 18, height = 12, units = 'cm', res = 600)
# (bottom, left, top, right)
par(ps = 20, mar = c(5, 4, 0.1, 1.2), font = 1, family = 'Times New Roman')
plot(1:9, 1 - tp_rf_train_whole$train$err.rate[100, ], type = 'n',
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0.4, 1), xaxt='n', yaxt='n')
lines(1:9, 1 - tp_rf_train_whole$train$err.rate[100, ], 
      type = 'l', lwd = 2, lty = 1, col = 'black')
lines(1:9, 1 - tp_rf_train_noprn$train$err.rate[100, ], 
      type = 'l', lwd = 2, lty = 2, col = 'steelblue2')
lines(1:9, 1 - tp_rf_train_6sf$train$err.rate[100, ], 
      type = 'l', lwd = 2, lty = 2, col = 'red')
lines(1:9, 1 - tp_rf_train_pca$train$err.rate[100, ], 
      type = 'l', lwd = 2, lty = 2, col = 'black')
title(xlab = 'Activity categories', line = 3.6)
title(ylab = 'Accuracy', line = 2.6)
axis(1, at = seq(1, 9, 1), labels = FALSE,
     tick = T, lwd.ticks = 1, tck = -0.01,
     cex.axis = 0.8, mgp = c(0, 0.6, 0))
text(x = seq(1, 9, 1), y = 0.32, labels = c('Overall', activity_categories), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = seq(0.4, 1, 0.1),
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8, 
     mgp = c(0, 0.6, 0))
legend(4.1, 1.04, 
       legend = c('All features', 
                  'All but socio-demographic features', 
                  '6 most significant features', 
                  'PCs of numerical features'), 
       col = c('black', 'steelblue2', 'red', 'black'), 
       lty = c(1, 2, 2, 2), cex = 0.8, lwd = 2,
       box.lty = 0, y.intersp = 1.8, bg = NA)
dev.off()	


#### Comparison between results from swrf and ADASYN ----

load('3_output/28_Apr/tp_rf_cv_2fold.RData')
cv_rftrain <- (tp_rf_cv_2fold$split_activities[[1]]$train$err.rate[100, ] + 
                 tp_rf_cv_2fold$split_activities[[2]]$train$err.rate[100, ]) /2
cv_rftest <- (tp_rf_cv_2fold$split_activities[[1]]$train$test$err.rate[100, ] + 
                tp_rf_cv_2fold$split_activities[[2]]$train$test$err.rate[
                  100, ]) / 2
cv_rftrain_user <- (
    tp_rf_cv_2fold$split_user[[1]]$train$err.rate[100, ] + 
        tp_rf_cv_2fold$split_user[[2]]$train$err.rate[100, ]) /2
cv_rftest_user <- (
    tp_rf_cv_2fold$split_user[[1]]$train$test$err.rate[100, ] +
        tp_rf_cv_2fold$split_user[[2]]$train$test$err.rate[100, ]) / 2
rm(tp_rf_cv_2fold)

load('3_output/28_Apr/tp_swrf_cv_2fold.RData')
cv_swrf_test <- c(
  (tp_swrf_cv_2fold$split_activities[[1]]$test$s1$confusion$overall[
    1] +
     tp_swrf_cv_2fold$split_activities[[2]]$test$s1$confusion$overall[
       1] ) /2, 
  (tp_swrf_cv_2fold$split_activities[[1]]$test$s1$confusion$byClass[
    , 6] + 
     tp_swrf_cv_2fold$split_activities[[2]]$test$s1$confusion$byClass[
       , 6]) /2
)
rm(tp_swrf_cv_2fold)

load('3_output/28_Apr/tp_swrf_adasyn_cv_2fold.RData')
cv_swrf_adasyntest <- c(
  (tp_swrf_adasyn_cv_2fold$split_activities[[1]]$test$s1$confusion$overall[
    1] +
     tp_swrf_adasyn_cv_2fold$split_activities[[2]]$test$s1$confusion$overall[
       1] ) /2, 
  (tp_swrf_adasyn_cv_2fold$split_activities[[1]]$test$s1$confusion$byClass[
    , 6] + 
  tp_swrf_adasyn_cv_2fold$split_activities[[2]]$test$s1$confusion$byClass[
    , 6]) /2
)
rm(tp_swrf_adasyn_cv_2fold)

source('2_code/r_module/namelist.R', chdir = TRUE) 


png(
  '4_visualization/3.1.5 Comparison between original and modified random forest model perfomance_pre.png',
  width = 18, height = 12, units = 'cm', res = 600)
par(ps = 20, mar = c(5, 4, 0.1, 1.2), font = 1, family = 'Times New Roman')
plot(1:9, 1 - cv_rftrain, type = 'n',
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0, 1), xaxt='n', yaxt='n')
lines(1:9, 1 - cv_rftest, type = 'l', lty = 1, lwd = 3, col = 'black')
lines(1:9, 1 - cv_rftest_user, type = 'l', lty = 2, lwd = 2, col = 'black')
lines(1:9, cv_swrf_test, type = 'l', lty = 2, lwd = 2, col = 'steelblue2')
lines(1:9, cv_swrf_adasyntest, type = 'l', lty = 2, lwd = 2, col = 'red')
title(xlab = 'Activity categories', line = 3.6)
title(ylab = 'Accuracy', line = 2.6)
axis(1, at = seq(1, 9, 1), labels = FALSE, 
     tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.6, 0))
text(x = seq(1, 9, 1), y = -0.13, labels = c('Overall', activity_categories), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = seq(0, 1, 0.2),
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8, 
     mgp = c(0, 0.6, 0))
legend(0.8, 0.4, 
       legend = c('Test set', 
                  # 'Training data', 
                  'Test set across participants', 
                  # 'Training data across participants', 
                  'Multistage random forest', 
                  'Multistage random forest with ADASYN'), 
       col = c('black', 'black', 'steelblue2', 'red'), 
       lty = c(1, 2, 2, 2), cex = 0.8, lwd = 2, 
       box.lty = 0, y.intersp = 1.8, bg = NA)
dev.off()	




png(
  '4_visualization/3.1.5 Comparison between original and modified random forest model perfomance.png',
  width = 18, height = 12, units = 'cm', res = 600)
par(ps = 20, mar = c(5, 4, 0.1, 1.2), font = 1, family = 'Times New Roman')
plot(1:9, 1 - cv_rftrain, type = 'n',
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0, 1), xaxt='n', yaxt='n')
lines(1:9, 1 - cv_rftest, type = 'l', lty = 1, lwd = 1.5, col = 'black')
lines(1:9, 1 - cv_rftrain, type = 'l', lty = 2, lwd = 1.5, col = 'black')

lines(1:9, 1 - cv_rftest_user, type = 'l', lty = 1, lwd = 1.5, col = 'grey')
lines(1:9, 1 - cv_rftrain_user, type = 'l', lty = 2, lwd = 1.5, col = 'grey')

lines(1:9, cv_swrf_test, type = 'l', lty = 1, lwd = 1.5, col = 'steelblue2')
lines(1:9, cv_swrf_adasyntest, type = 'l', lty = 1, lwd = 1.5, col = 'red')
title(xlab = 'Activity categories', line = 3.6)
title(ylab = 'Accuracy', line = 2.6)
axis(1, at = seq(1, 9, 1), labels = FALSE, 
     tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.6, 0))
text(x = seq(1, 9, 1), y = -0.13, labels = c('Overall', activity_categories), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = seq(0, 1, 0.2),
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8, 
     mgp = c(0, 0.6, 0))
legend(0.8, 0.56, 
       legend = c('Test set', 
                  'Training set',
                  'Test set across participants', 
                  'Training set across participants',
                  'Multistage random forest', 
                  'Multistage random forest with ADASYN'), 
       col = c('black', 'black', 'grey', 'grey', 'steelblue2', 'red'), 
       lty = c(1, 2, 1, 2, 1, 1), cex = 0.8, lwd = 1.5, 
       box.lty = 0, y.intersp = 1.8, bg = NA)
dev.off()	



#### Considering benefits of POI info for ten person/activities in Zuerich ----

source('2_code/r_module/namelist.R', chdir = TRUE) 
load('3_output/28_Apr/tp_rf_10prn.RData')

png('4_visualization/3.1.6 Benefits from Google places API.png',
    width = 18, height = 12, units = 'cm', res = 600)
par(ps = 20, mar = c(5, 4, 0.1, 1.2), font = 1, family = 'Times New Roman')
plot(1:9, 1 - tp_rf_10prn$original$train$err.rate[100, ], type = 'n',
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0, 1), xaxt='n', yaxt='n')
lines(1:9, 1 - tp_rf_10prn$original$train$err.rate[100, ], 
      type = 'l', lwd = 2, lty = 1)
lines(1:9, 1 - tp_rf_10prn$add_poi$train$err.rate[100, ], 
      type = 'l', lwd = 2, lty = 2, col = 'steelblue2')
lines(1:9, tp_rf_10prn$svm$error_rate[11, ], 
      lwd = 2, type = 'l', lty = 2, col = 'red')
title(xlab = 'Activity categories', line = 3.6)
title(ylab = 'Accuracy', line = 2.6)
axis(1, at = seq(1, 9, 1), labels = FALSE, 
     tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.6, 0))
text(x = seq(1, 9, 1), y = -0.13, labels = c('Overall', activity_categories), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = seq(0, 1, 0.2),
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8, 
     mgp = c(0, 0.6, 0))
legend(5, 1.05, 
       legend = c('Original random forest', 
                  'Add POI information', 
                  'SVM performance'), 
       col = c('black', 'steelblue2', 'red'), 
       lty = c(1, 2, 2), cex = 0.8, lwd = 2,
       box.lty = 0, y.intersp = 1.8, bg = NA)
dev.off()	







#### Spatial distribution of misclassified trip purpose ----

suppressPackageStartupMessages(library('rgdal'))
suppressPackageStartupMessages(library('ggmap') )
source('2_code/r_module/point_count.R', chdir = TRUE) 

load('3_output/28_Apr/tp_rf_train_whole.RData')
load('3_output/28_Apr/purpose_data.RData')
load('3_output/28_Apr/activities.RData')
swiss_kanton <- rgdal::readOGR('3_output/28_Apr/geo/swiss_kanton.shp')

purpose_labeled <- droplevels(purpose_data[
    which(purpose_data$labels != 'Unknown'), ]
)
# rm(purpose_data)

# purpose_labeled$labels <- factor(
#   purpose_labeled$labels, levels = activity_categories)
# caret::confusionMatrix(purpose_labeled$labels, 
#                        tp_rf_train_whole$train$predicted)

classified_swiss <- as.data.frame(
    activities[
        which(activities$ID %in% purpose_labeled$ID[
            which(purpose_labeled$labels == 
                      tp_rf_train_whole$train$predicted)] & 
                activities$lon >= 5.9 & activities$lon <= 10.55 &
                activities$lat >= 45.78 & activities$lat <= 47.85 &
                ! is.na(activities$inkanton)),
        c('lon', 'lat')]
)

total_swiss <- as.data.frame(
    activities[
        which(activities$ID %in% purpose_labeled$ID & 
                activities$lon >= 5.9 & activities$lon <= 10.55 &
                activities$lat >= 45.78 & activities$lat <= 47.85 &
                  ! is.na(activities$inkanton)),
        c('lon', 'lat')]
)

classified_swiss_density <- point_count(
    lon = classified_swiss$lon, lat = classified_swiss$lat, 
    lon_range = c(5.9, 10.55), lat_range = c(45.78, 47.85), n = c(100, 100))

total_swiss_density <- point_count(
    lon = total_swiss$lon, lat = total_swiss$lat, 
    lon_range = c(5.9, 10.55), lat_range = c(45.78, 47.85), n = c(100, 100))

accuracy_rate <- classified_swiss_density
accuracy_rate@data@values <- classified_swiss_density@data@values / 
    total_swiss_density@data@values

accuracy_rate_spdf <- as.data.frame(as(accuracy_rate, "SpatialPixelsDataFrame"))
accuracy_rate_spdf$layer_brk <- cut(
    accuracy_rate_spdf$layer, 
    breaks = c(-0.001, 0.2, 0.4, 0.6, 0.8, 1), 
    labels = c(20, 40, 60, 80, 100))

png('4_visualization/3.1.9 Spatial distribution of accuracy rate around Switzerland.png',
    width = 15, height = 11, units = 'cm', res = 600)
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
        axis.text.x = element_text(size = 16, 
                                   family = 'Times New Roman', vjust = 0),
        axis.text.y = element_text(size = 16, 
                                   family = 'Times New Roman', hjust = 1),
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
        guide = guide_legend(
            keyheight = unit(0.25, units = "cm"),
            keywidth = unit(2, units = "cm"),
            title.position = 'top', title = 'Accuracy (%)',
            title.vjust = -1.2, title.hjust = 0.6,
            nrow = 1, byrow = T,
            label.position = "bottom", default.unit = 'cm',
            title.theme = element_text(size = 20, family = 'Times New Roman'),
            label.theme = element_text(size = 16, family = 'Times New Roman'),
            reverse = FALSE, label.vjust = 3, label.hjust = 1.2)
    )
dev.off()


# png('4_visualization/3.1.9 Spatial distribution of accuracy rate around Switzerland.png',
#     width = 15, height = 11, units = 'cm', res = 600)
# ggplot() +
#     geom_tile(data = as.data.frame(as(
#         accuracy_rate, "SpatialPixelsDataFrame")),
#         aes(x = x, y = y, fill = layer)) +
#     geom_polygon(aes(long, lat, group = group), fill = NA,
#                  color = 'black', size = 0.05, data = swiss_kanton) +
#     scale_x_continuous(
#         limits = c(5.9, 10.55), expand = c(0, 0),
#         breaks = seq(6, 10, 1),
#         labels = paste(seq(6, 10, 1), '°E', sep = '') ) +
#     scale_y_continuous(
#         limits = c(45.78, 47.85), expand = c(0, 0),
#         breaks = seq(46, 47.5, 0.5),
#         labels = paste(seq(46, 47.5, 0.5), '°N', sep = '')) +
#     theme(
#         panel.border = element_rect(
#             colour = "black", fill=NA, size=0.1),
#         panel.background = element_blank(),
#         axis.text.x = element_text(
#             size = 16, family = 'Times New Roman', vjust = 0),
#         axis.text.y = element_text(
#             size = 16, family = 'Times New Roman', hjust = 1),
#         axis.ticks = element_line(size = 0.2),
#         axis.title = element_blank(),
#         panel.spacing.x = unit(0, "cm"),
#         legend.position = c(0.45, -0.22), 
#         legend.direction = 'horizontal', 
#         legend.background = element_blank(),
#         # (top, right, bottom, left)
#         plot.margin = unit(c(0.2, 0.1, 2, 0.3), "cm") ) + 
#     scale_fill_continuous(
#         limits = c(0, 1), breaks = seq(0, 1, 0.25),
#         labels = seq(0, 100, 25),
#         guide = guide_colourbar(
#             title = 'Accuracy (%)', 
#             title.position = 'top', title.vjust = -1.2, title.hjust = 0.6, 
#             label.position = "bottom", label.vjust = 3,
#             title.theme = element_text(
#                 size = 20, family = 'Times New Roman'),
#             label.theme = element_text(size = 16, family = 'Times New Roman'), 
#             barwidth = 8, barheight = 0.25, default.unit = 'cm'
#         ))
# dev.off()
# 


total_swiss_density@data@values[total_swiss_density@data@values == 0] <- NA
total_swiss_density@data@values[!is.na(total_swiss_density@data@values)] <- 
    log(total_swiss_density@data@values[
        !is.na(total_swiss_density@data@values)])

total_swiss_density_spdf <- as.data.frame(as(
    total_swiss_density, "SpatialPixelsDataFrame"))
total_swiss_density_spdf$layer_brk <- cut(
    total_swiss_density_spdf$layer, 
    breaks = c(-0.001, 2, 4, 6, 8, 10), 
    labels = c(expression(e^{2}), expression(e^{4}), expression(e^{6}), 
               expression(e^{8}), expression(e^{10})))



png('4_visualization/3.1.10 Spatial distribution of labeled activities around Switzerland.png',
    width = 15, height = 11, units = 'cm', res = 600)
ggplot() +
    geom_tile(data = total_swiss_density_spdf,
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


# data1 = as.data.frame(as(accuracy_rate, "SpatialPixelsDataFrame"))
# data2 = as.data.frame(as(total_swiss_density, "SpatialPixelsDataFrame"))
# cor(data1[, 1], data2[, 1])


# png('4_visualization/3.1.7 Spatial distribution of misclassified trip purpose around Switzerland.png',
#     width = 15, height = 9, units = 'cm', res = 600)
# ggmap(swiss_map) + 
#     # geom_polygon(aes(long, lat, group = group), fill = NA, 
#     #              color = 'black', size = 0.05, data = swiss_kanton) +
#     # geom_point(data = unclassified, 
#     #            aes(x = lon, y = lat), size = 0.05, shape = 20, 
#     #            color = "red", alpha = 0.1) +
#     stat_density_2d(
#         mapping = aes(x = lon, y = lat, fill = ..level..), data = unclassified) +
#     scale_x_continuous(
#         limits = c(5.9, 10.55), expand = c(0, 0),
#         breaks = seq(6, 10, 1), 
#         labels = paste(seq(6, 10, 1), '°E', sep = '') ) +
#     scale_y_continuous(
#         limits = c(45.78, 47.85), expand = c(0, 0),
#         breaks = seq(46, 47.5, 0.5),
#         labels = paste(seq(46, 47.5, 0.5), '°N', sep = '')) +
#     theme(
#         panel.border = element_rect(
#             colour = "black", fill=NA, size=0.1), 
#         panel.background = element_blank(),
#         axis.text.x = element_text(
#             size = 16, family = 'Times New Roman', vjust = 0),
#         axis.text.y = element_text(
#             size = 16, family = 'Times New Roman', hjust = 1), 
#         axis.ticks = element_line(size = 0.2), 
#         axis.title = element_blank(),
#         panel.spacing.x = unit(0, "cm"),
#         plot.margin = unit(c(0.25, 0.25, 0.2, 0.3), "lines"))
# dev.off()


# zuerich_map <- ggmap::get_map(location = c(lon = 8.67, lat = 47.425), 
#                       zoom = 10, maptype = "roadmap")
# 
# png('4_visualization/3.1.8 Spatial distribution of misclassified trip purpose around Zuerich.png',
#     width = 10, height = 11, units = 'cm', res = 600)
# ggmap(zuerich_map) + 
#     # geom_polygon(aes(long, lat, group = group), fill = NA, 
#     #              color = 'black', size = 0.05, data = swiss_kanton) +
#     geom_point(data = unclassified, 
#                aes(x = lon, y = lat), size = 0.05, shape = 20, 
#                color = "red", alpha = 0.1) +
#     scale_x_continuous(
#         limits = c(8.35, 8.91), expand = c(0, 0),
#         breaks = seq(8.40, 8.9, 0.2), 
#         labels = paste(seq(8.40, 8.9, 0.2), '°E', sep = '') ) +
#     scale_y_continuous(
#         limits = c(47.15, 47.61), expand = c(0, 0), 
#         breaks = seq(47.2, 47.6, 0.1),
#         labels = paste(seq(47.2, 47.6, 0.1), '°N', sep = '')) +
#     theme(
#         panel.border = element_rect(
#             colour = "black", fill=NA, size=0.1), 
#         panel.background = element_blank(),
#         axis.text.x = element_text(
#             size = 16, family = 'Times New Roman', vjust = 0),
#         axis.text.y = element_text(
#             size = 16, family = 'Times New Roman', hjust = 1), 
#         axis.ticks = element_line(size = 0.2), 
#         axis.title = element_blank(),
#         panel.spacing.x = unit(0, "cm"),
#         plot.margin = unit(c(0.1, 0.25, 0.1, 0.3), "lines"))
# dev.off()

    

  
#### Dependence on duration and Number of participants ----

load('3_output/28_Apr/Dependence_res.RData')


png(
    '4_visualization/3.1.11 Dependence of classification performance on number of participants and duration of survey.png',
    width = 15, height = 9, units = 'cm', res = 600)
par(ps = 20, mar = c(3, 4.5, 0.1, 0.1), font = 1, family = 'Times New Roman')
plot(1:180, 1 - dependence_res[[1]]$accuracy, 
     type = 'l', lwd = 2, lty = 1, col = 'black', ylim = c(0.68, 0.9), 
     xlab = NA, ylab = NA, main = NA, xaxt='n', yaxt='n')
lines(1:150, 1 - dependence_res[[2]]$accuracy, 
      type = 'l', lwd = 2, lty = 3, col = 'black')
lines(1:120, 1 - dependence_res[[3]]$accuracy, 
      type = 'l', lwd = 2, lty = 1, col = 'steelblue2')
lines(1:90, 1 - dependence_res[[4]]$accuracy, 
      type = 'l', lwd = 2, lty = 3, col = 'steelblue2')
lines(1:60, 1 - dependence_res[[5]]$accuracy, 
      type = 'l', lwd = 2, lty = 1, col = 'red')
lines(1:30, 1 - dependence_res[[6]]$accuracy, 
      type = 'l', lwd = 2, lty = 3, col = 'red')
title(xlab = 'Duration of survey (days)', line = 1.8)
title(ylab = 'Accuracy', line = 3)
axis(1, at = seq(0, 180, 30), tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.5, 0))
axis(2, at = seq(0.7, 0.9, 0.05), label = T, tick = T, lwd.ticks = 1, 
     las = 2, tck = -0.01, cex.axis = 0.8, mgp = c(0, 0.5, 0))
legend(70, 0.82, 
       legend = c('3 users report 180 days', 
                  '9 users report 150 days', 
                  '39 users report 120 days', 
                  '126 users report 90 days', 
                  '450 users report 60 days', 
                  '1216 users report 30 days'), 
       col = c('black', 'black', 'steelblue2', 'steelblue2', 'red', 'red'), 
       lty = c(1, 3, 1, 3, 1, 3), lwd = 2, 
       cex = 0.8, box.lty = 0, y.intersp = 1.8, bg = NA)
dev.off()	



# Trip mode classification results visulization ------------------------


#### Impact of number of trees ----

source('2_code/r_module/namelist.R', chdir = TRUE) 
load('3_output/28_Apr/tm_rf_train_whole.RData')

tm_rf_train_whole$train$confusion
tm_rf_train_whole$train$err.rate[100, ]

png('4_visualization/3.2.1 Impact of number of trees on trip mode derivation.png', 
    width = 12, height = 8, units = 'cm', res = 600)
par(ps = 20, mar = c(3, 4.5, 0.1, 0.1), font = 1, family = 'Times New Roman')
plot(1:100, 1 - tm_rf_train_whole$train$err.rate[, 1], type = 'l', 
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0.78, 0.87), xaxt='n', yaxt='n')
title(xlab = 'Parameter N', line = 2)
title(ylab = 'Accuracy', line = 3)
axis(1, at = seq(0, 100, 20), tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.5, 0))
axis(2, at = seq(0.78, 0.87, 0.02), label = T, tick = T, lwd.ticks = 1, 
     las = 2, tck = -0.01, cex.axis = 0.8, mgp = c(0, 0.5, 0))
dev.off()	


#### Impact of parameters M ----

load('3_output/28_Apr/tm_rf_train_whole_mtry.RData')

tm_rf_train_whole_mtry_error_rate <- 0
for(i in 1: length(tm_rf_train_whole_mtry) ){
  tm_rf_train_whole_mtry_error_rate[i] <- 
    tm_rf_train_whole_mtry[[i]]$train$err.rate[50, 1]
}

png('4_visualization/3.2.2 Impact of parameters M on trip mode derivation.png', 
    width = 12, height = 8, units = 'cm', res = 600)
par(ps = 20, mar = c(3, 4.5, 0.1, 0.1), font = 1, family = 'Times New Roman')
plot(1:length(tm_rf_train_whole_mtry), 
     1 - tm_rf_train_whole_mtry_error_rate, type = 'l', 
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0.8, 0.87), xaxt='n', yaxt='n')
title(xlab = 'Parameter M', line = 2)
title(ylab = 'Accuracy', line = 3)
axis(1, at = seq(0, length(tm_rf_train_whole_mtry), 5), tick = T, 
     lwd.ticks = 1, tck = -0.01, cex.axis = 0.8, mgp = c(0, 0.5, 0))
axis(2, at = seq(0.8, 0.86, 0.02), label = T, tick = T, lwd.ticks = 1, 
     las = 2, tck = -0.01, cex.axis = 0.8, mgp = c(0, 0.5, 0))
dev.off()	



#### Feature importance ----

source('2_code/r_module/namelist.R', chdir = TRUE) 
load('3_output/28_Apr/tm_rf_train_whole.RData')

png('4_visualization/3.2.3 Feature importance in trip mode derivation.png', 
    width = 25, height = 33, units = 'cm', res = 600)
par(ps = 20, mar = c(3, 25, 0.1, 0.8), font = 1, family = 'Times New Roman')
plot(sort(tm_rf_train_whole$train$importance[, 9]), 1:43, type = 'o', pch = 20, 
     xlab = NA, ylab = NA, main = NA, 
     xlim = c(0, 200000), xaxt='n', yaxt='n')
title(xlab = 'Mean decrease in Gini', line = 2)
title(ylab = 'Selected features', line = 23.5)
axis(1, at = seq(0, 200000, 40000), tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.5, 0))
axis(2, at = seq(1, 43, 1), 
     label = sapply(names(sort(tm_rf_train_whole$train$importance[, 9])),
                    function(x) mapping_var_tm[['meaning']][
                      which(mapping_var_tm[['variable']] == x)]), 
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8, 
     mgp = c(0, 0.5, 0))
dev.off()	


#### Impact of Feature selection. all - withou prn - 20 sf - 6 sf - pca ----

load('3_output/28_Apr/tm_rf_train_whole.RData')
# load('3_output/28_Apr/tm_rf_train_20sf.RData')
load('3_output/28_Apr/tm_rf_train_5sf.RData')
load('3_output/28_Apr/tm_rf_train_noprn.RData')
source('2_code/r_module/namelist.R', chdir = TRUE) 

# tm_rf_train_whole$train$err.rate[100, ]
# tm_rf_train_20sf$train$err.rate[100, ]
# tm_rf_train_5sf$train$err.rate[100, ]
# tm_rf_train_noprn$train$err.rate[100, ]

png(
  '4_visualization/3.2.4 Impact of feature selection on trip mode derivation.png',
  width = 18, height = 12, units = 'cm', res = 600)
par(ps = 20, mar = c(5, 4, 0.1, 1.2), font = 1, family = 'Times New Roman')
plot(1:8, 1 - tm_rf_train_whole$train$err.rate[100, ], type = 'n',
     xlab = NA, ylab = NA, main = NA, 
     ylim = c(0, 1), xaxt='n', yaxt='n')
lines(1:8, 1 - tm_rf_train_whole$train$err.rate[100, ], type = 'l', lwd = 2)
lines(1:8, 1 - tm_rf_train_noprn$train$err.rate[100, ], 
      type = 'l', lty = 2, lwd = 2, col = 'steelblue2')
lines(1:8, 1 - tm_rf_train_5sf$train$err.rate[100, ], 
      type = 'l', lty = 2, lwd = 2, col = 'red')
title(xlab = 'Mode categories', line = 3.6)
title(ylab = 'Accuracy', line = 2.6)
axis(1, at = seq(1, 8, 1), labels = FALSE, 
     tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.6, 0))
text(x = seq(1, 8, 1), y = -0.13, labels = c('Overall', mode_categories), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = seq(0, 1, 0.2),
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, cex.axis = 0.8, 
     mgp = c(0, 0.6, 0))
legend(3.5, 1, 
       legend = c('All features', 
                  'All but socio-demographic features', 
                  '5 most significant features'), 
       col = c('black', 'steelblue2', 'red'), 
       cex = 0.8, lty = c(1, 2, 2), lwd = 2,
       box.lty = 0, y.intersp = 1.5, bg = NA)
dev.off()	



# interaction between trip purpose and mode imputation --------------------
# 100 trees: 84.6 for tp, 86.3% for tm

load('3_output/28_Apr/interaction_tp_tm.RData')
source('2_code/r_module/namelist.R', chdir = TRUE) 

# summary(interaction_tp_tm$mode_data$length[which(
#   interaction_tp_tm$mode_data$labels == 'Walk')] ) # 622 m 
# summary(interaction_tp_tm$mode_data$duration_min[which(
#   interaction_tp_tm$mode_data$labels == 'Walk')] ) # 7.6 min
# summary(interaction_tp_tm$mode_data$speed[which(
#     interaction_tp_tm$mode_data$labels == 'Walk')] ) # 2.58 m/s

interaction_tp_tm$mode_choice <- droplevels(
    data.table::merge.data.table(
        interaction_tp_tm$purpose_data[
            which(interaction_tp_tm$purpose_data$labels != 'Unknown'), 
            .SD, .SDcol = c('ID', 'labels')], 
        interaction_tp_tm$mode_data[
            which(interaction_tp_tm$mode_data$labels != 'Unknown'), 
            .SD, .SDcol = c('next_activity_id', 'labels')], 
        by.x = 'ID', by.y = 'next_activity_id', 
        suffixes = c('_activity', '_mode')
    )
)

interaction_tp_tm$mode_choice$labels_activity <- factor(
    interaction_tp_tm$mode_choice$labels_activity, 
    levels =  names(sort(table(interaction_tp_tm$mode_choice$labels_activity, 
                              useNA = 'ifany'), decreasing = TRUE))
)

interaction_tp_tm$mode_choice$labels_mode <- factor(
    interaction_tp_tm$mode_choice$labels_mode, 
    levels = names(sort(table(interaction_tp_tm$mode_choice$labels_mode, 
                              useNA = 'ifany'), decreasing = TRUE))
)

interaction_tp_tm$purpose_mode <- table(
    interaction_tp_tm$mode_choice$labels_activity, 
    interaction_tp_tm$mode_choice$labels_mode)

purpose_mode <- read.table('3_output/28_Apr/purpose_mode.TXT', 
                           header = TRUE)

png('4_visualization/3.3.1 Interaction between trip purpose and mode choice.png', 
    width = 20, height = 12, units = 'cm', res = 600)
par(ps = 20, mar = c(5, 5, 0.1, 7), font = 1, family = 'Times New Roman')
plot(1:9, purpose_mode[, 1], type = 'l', lwd = 2,
     ylim = c(10^-4, 10^0), xlim = c(1, 9), col = 'black', log = 'y',
     xlab = NA, ylab = NA, main = NA, xaxt='n', yaxt='n')
lines(1:9, purpose_mode[, 2], col = 'black', lty = 2, lwd = 2)
lines(1:9, purpose_mode[, 3], col = 'steelblue2', lty = 1, lwd = 2)
lines(1:9, purpose_mode[, 4], col = 'steelblue2', lty = 2, lwd = 2)
lines(1:9, purpose_mode[, 5], col = 'red', lty = 1, lwd = 2)
lines(1:9, purpose_mode[, 6], col = 'steelblue2', lty = 3, lwd = 2)
lines(1:9, purpose_mode[, 7], col = 'red', lty = 2, lwd = 2)
title(xlab = 'Activity categories', line = 3.6)
title(ylab = 'Frequency', line = 3)
axis(1, at = seq(1, 9, 1), labels = FALSE,
     tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.6, 0))
text(x = seq(1, 9, 1), y = 0.00003, labels = row.names(purpose_mode), 
     srt = 30, cex = 0.8, xpd = TRUE)
axis(2, at = c(10^-4, 10^-3, 10^-2, 10^-1, 10^0), 
     label = c(expression(10^-4), expression(10^-3), expression(10^-2), 
               expression(10^-1), expression(10^0)), 
     tick = T, lwd.ticks = 1, las = 2, tck = -0.01, 
     cex.axis = 0.8, mgp = c(0, 0.6, 0))
legend(9.5, 1,
       legend = colnames(purpose_mode),
       col = c('black', 'black', 'steelblue2', 'steelblue2', 'red', 
               'steelblue2', 'red'),
       lty = c(1, 2, 1, 2, 1, 3, 2), lwd = 2, cex = 0.8, bg="transparent",
       box.lty = 0, box.lwd = 0.5, y.intersp = 2.2, xpd = TRUE, 
       ncol = 1)
dev.off()	





