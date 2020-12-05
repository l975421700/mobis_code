
source('~/mobis_code/source file and import packages.R')
source('/data/students/qigao/mobis_code/r_module/classification_accuracy.R')
load('/data/students/qigao/scratch/tp_inswiss_label.RData')

# initial 10-fold classification and ensemble filter ----
tp_inswiss_label <- tp_inswiss_label[order(user_id)]
tp_train_original <- multi_clf_kfold(
    x = tp_inswiss_label[
        , .SD, 
        .SDcol = tp_sf_all[!tp_sf_all %in% participants_features]],
    y = tp_inswiss_label$labels,
    ID = tp_inswiss_label$ID,
    k = 10
)

save(tp_train_original, 
     file = '/data/students/qigao/scratch/tp_train_original.RData')

# load('/data/students/qigao/scratch/tp_train_original.RData')
classified <- which(
    tp_inswiss_label$ID %in%
    tp_train_original$prediction$ID[
        tp_train_original$prediction$all_predicted > 0]
)
# tp_inswiss_label$ID; tp_train_original$prediction$ID
# sum(tp_inswiss_label$ID == tp_train_original$prediction$ID)
# length(which( tp_inswiss_label$ID %in% tp_train_original$prediction$ID))
# length(which(
#     tp_inswiss_label$ID[classified] %in%
#         tp_train_original$prediction$ID[
#             tp_train_original$prediction$all_predicted > 0]
# ))
rm(tp_train_original)
tp_train_filtered <- multi_clf_kfold(
    x = tp_inswiss_label[
        classified, .SD, 
        .SDcol = tp_sf_all[!tp_sf_all %in% participants_features]],
    y = tp_inswiss_label$labels[classified],
    ID = tp_inswiss_label$ID[classified],
    k = 10
)

save(tp_train_filtered, 
     file = '/data/students/qigao/scratch/tp_train_filtered.RData')


# Inter-personal classification ----

tp_train_original_interprn <- multi_clf_kfold(
    x = tp_inswiss_label[
        , .SD, 
        .SDcol = tp_sf_all[!tp_sf_all %in% participants_features]],
    y = tp_inswiss_label$labels,
    ID = tp_inswiss_label$ID,
    group = tp_inswiss_label$user_id,
    k = 10
)

save(tp_train_original_interprn, 
     file = '/data/students/qigao/scratch/tp_train_original_interprn.RData')


filtered_data <- tp_inswiss_label[classified, ]
filtered_data <- filtered_data[order(user_id)]
tp_train_filtered_interprn <- multi_clf_kfold(
    x = filtered_data[
        , .SD, 
        .SDcol = tp_sf_all[!tp_sf_all %in% participants_features]],
    y = filtered_data$labels,
    ID = filtered_data$ID,
    group = filtered_data$user_id,
    k = 10
)

tp_train_filtered_interprn$kfold_indices = 0
tp_train_filtered_interprn$rf = 0
tp_train_filtered_interprn$c50 = 0
tp_train_filtered_interprn$nb = 0
tp_train_filtered_interprn$mars = 0

save(tp_train_filtered_interprn, 
     file = '/data/students/qigao/scratch/tp_train_filtered_interprn.RData')

# tp_train_original_interprn$accuracy

# second run of ensemble filter ----






# Plot ----

# load('/data/students/qigao/scratch/tp_train.RData')
load('/data/students/qigao/scratch/tp_train_original.RData')
load('/data/students/qigao/scratch/tp_train_filtered.RData')
load('/data/students/qigao/scratch/tp_train_original_interprn.RData')
load('/data/students/qigao/scratch/tp_train_filtered_interprn.RData')

classified <- which(
    tp_inswiss_label$ID %in%
        tp_train_original$prediction$ID[
            tp_train_original$prediction$all_predicted > 0]
)



# table(tp_inswiss_label$labels[-classified]) / table(tp_inswiss_label$labels)

tp_train_original$accuracy
tp_train_filtered$accuracy
tp_train_original_interprn$accuracy
tp_train_filtered_interprn$accuracy
clf_accuracy(tp_train_original$prediction$labels,
             tp_train_original$prediction$rf_prediction)
clf_accuracy(
    factor(tp_train_filtered$prediction$labels, levels = activity_categories),
    factor(tp_train_filtered$prediction$rf_prediction, 
           levels = activity_categories))
# clf_accuracy(tp_train_original_interprn$prediction$labels,
#              tp_train_original_interprn$prediction$rf_prediction)
# clf_accuracy(tp_train_filtered_interprn$prediction$labels,
#              tp_train_filtered_interprn$prediction$rf_prediction)


png(
    'visualization/6 Impact of ensemble filter.png',
    width = 8.8, height = 5, units = 'cm', res = 1200)
# (bottom, left, top, right)
par(ps = 8, mar = c(2.1, 2, 0.1, 0.6), font = 1, family = 'Times New Roman')
plot(clf_accuracy(tp_train_original$prediction$labels,
                  tp_train_original$prediction$rf_prediction),
     type = 'l', lwd = 1, lty = 2,
     col = 'black', xlab = NA, ylab = NA, main = NA, 
     ylim = c(0.5, 1), xaxt='n', yaxt='n')
lines(clf_accuracy(
    factor(tp_train_filtered$prediction$labels, levels = activity_categories),
    factor(tp_train_filtered$prediction$rf_prediction, 
           levels = activity_categories)), 
      type = 'l', lwd = 1, lty = 1, col = 'black')
title(xlab = 'Activity categories', line = 1.15)
title(ylab = 'Accuracy', line = 1.3)
axis(1, at = seq(1, 9, 1), labels = FALSE,
     tick = T, lwd.ticks = 1, tck = -0.01,
     cex.axis = 1, mgp = c(0, 0.6, 0))
text(x = seq(1, 9, 1), y = 0.42, labels = c('Overall', activity_categories), 
     srt = 30, cex = 1, xpd = TRUE)
axis(2, at = seq(0.5, 1, 0.1),
     tick = T, lwd.ticks = 1, las = 1.5, tck = -0.01, cex.axis = 1, 
     mgp = c(0, 0.4, 0))
legend(0.5, 0.675,
       legend = c('Original data', 
                  'Ensemble filtered (8.5%) data'), 
       col = 'black', 
       lty = c(2, 1), cex = 1, lwd = 1,
       box.lty = 0, y.intersp = 0.8, bg = NA, seg.len = 1)
dev.off()








