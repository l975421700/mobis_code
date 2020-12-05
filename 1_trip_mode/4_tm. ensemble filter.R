
source('~/mobis_code/source file and import packages.R')
source('/data/students/qigao/mobis_code/r_module/classification_accuracy.R')
load('/data/students/qigao/scratch/tm_inswiss_label.RData')

# initial 2-fold classification and ensemble filter ----

tm_inswiss_label <- tm_inswiss_label[order(user_id)]


tm_train_original <- multi_clf_kfold(
    x = tm_inswiss_label[
        , .SD, 
        .SDcol = tm_sf_all],
    y = tm_inswiss_label$labels,
    ID = tm_inswiss_label$ID,
    k = 2
)




save(tm_train_original, 
     file = '/data/students/qigao/scratch/tm_train_original.RData')


load('/data/students/qigao/scratch/tm_train_original.RData')
classified <- which(
    tm_inswiss_label$ID %in%
    tm_train_original$prediction$ID[
        tm_train_original$prediction$all_predicted > 0]
)




tm_inswiss_label$ID; tm_train_original$prediction$ID
sum(tm_inswiss_label$ID == tm_train_original$prediction$ID)
length(which( tm_inswiss_label$ID %in% tm_train_original$prediction$ID))
length(which(
    tm_inswiss_label$ID[classified] %in%
        tm_train_original$prediction$ID[
            tm_train_original$prediction$all_predicted > 0]
))
rm(tm_train_original)
tp_train_filtered <- multi_clf_kfold(
    x = tm_inswiss_label[
        classified, .SD,
        .SDcol = tp_sf_all[!tp_sf_all %in% participants_features]],
    y = tm_inswiss_label$labels[classified],
    ID = tm_inswiss_label$ID[classified],
    k = 10
)

save(tp_train_filtered,
     file = '/data/students/qigao/scratch/tp_train_filtered.RData')


# Inter-personal classification ----

# second run of ensemble filter ----

# Plot ----
