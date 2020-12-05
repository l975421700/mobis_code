

# Dependence on duration and Number of participants -----------------------

source('~/mobis_code/source file and import packages.R')
load("/data/mobis/data/enrichments/scratch/all_activities.RData")

# Filter unfully labeled user days

all_activities$dayth <- ceiling(as.numeric(difftime(
    all_activities$started_at_local, as.Date("2019-09-02", tz="UTC"), units = 'days'
)))

user_alllabelled_days <- all_activities[
    , .(unlabeled_activities = length(which(labels == 'Unknown'))), 
    by = .(user_id, dayth)
    ]

user_alllabelled_days <- user_alllabelled_days[
    which(user_alllabelled_days$unlabeled_activities == 0),
    ]

alllabelled_activities <- data.table::merge.data.table(
    all_activities, user_alllabelled_days, by = c("user_id", "dayth"), all = FALSE
)


alllabelled_activities$labels <- 
    factor(alllabelled_activities$labels, levels = activity_categories)
survey_length <- seq(30, 300, by = 30)
singleres <- list(data = 0, rf_res = list(0), accuracy = 0)
dependence_res <- list(
    d30 = singleres, d60 = singleres, d90 = singleres, d120 = singleres, 
    d150 = singleres, d180 = singleres, d210 = singleres, d240 = singleres, 
    d270 = singleres, d300 = singleres
)
rm(singleres)

for(i in 1:length(survey_length)){
    dependence_res[[i]]$data <- droplevels(alllabelled_activities[
        alllabelled_activities$user_id %in% (
            names(which(table(user_alllabelled_days$user_id) >= 
                            survey_length[i]))
        )])
    for(j in 1 : survey_length[i]){
        traindata = droplevels(data.table::merge.data.table(
            dependence_res[[i]]$data, 
            dependence_res[[i]]$data[
                , .N, by = .(user_id, dayth)][
                    , .(dayth = head(dayth, j) ), by = .(user_id)], 
            by = c('user_id', 'dayth')
        ))
        
        dependence_res[[i]]$rf_res[[j]] <- ranger::ranger(
            x = traindata[
                , .SD, 
                .SDcol = tp_sf_all[!tp_sf_all %in% participants_features]],
            y = traindata$labels,
            num.trees = 100,
            mtry = ceiling(log2(length(
                tp_sf_all[!tp_sf_all %in% participants_features]))+1 ) ,
            write.forest = FALSE
        )
        
        dependence_res[[i]]$accuracy[j] <- 
            1 - dependence_res[[i]]$rf_res[[j]]$prediction.error
        
        print(paste('i/10 = ', i, '; j = ', j, sep = ''))
    }
}

save(dependence_res, file = '/data/students/qigao/scratch/dependence_res.RData')



# Plot 


png(
    'visualization/5 Dependence of classification performance on number of participants and duration of survey.png',
    width = 8.8, height = 5, units = 'cm', res = 1200)
par(ps = 8, mar = c(1.5, 2, 0.1, 0.1), font = 1, family = 'Times New Roman')

plot(
    dependence_res[[1]]$accuracy, type = 'n', lwd = 1, lty = 1, col = 'black',
    xlim = c(0, 300), ylim = c(0.68, 0.9), 
    xlab = NA, ylab = NA, main = NA, xaxt='n', yaxt='n'
)
lines(
    dependence_res[[2]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'black'
)
# lines(
#     dependence_res[[3]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'steelblue2'
# )
lines(
    dependence_res[[4]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'steelblue2'
)
# lines(
#     dependence_res[[5]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'red'
# )
lines(
    dependence_res[[6]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'red'
)
# lines(
#     dependence_res[[7]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'green'
# )
lines(
    dependence_res[[8]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'green'
)
# lines(
#     dependence_res[[9]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'orange'
# )
lines(
    dependence_res[[10]]$accuracy, type = 'l', lwd = 1, lty = 1, col = 'orange'
)
title(xlab = 'Duration of survey (days)', line = 0.5)
title(ylab = 'Accuracy', line = 1.4)
axis(1, at = seq(0, 300, 60), tick = T, lwd.ticks = 1, tck = -0.01, 
     cex.axis = 1, mgp = c(0, -0.1, 0))
axis(2, at = seq(0.7, 0.9, 0.05), label = T, tick = T, lwd.ticks = 1, 
     las = 2, tck = -0.01, cex.axis = 1, mgp = c(0, 0.3, 0))
legend(120, 0.82, 
       legend = c(
           # '1357 users report 30 days',
           '785 users report 60 days', 
           # '509 users report 90 days', 
           '343 users report 120 days', 
           # '216 users report 150 days', 
           '131 users report 180 days', 
           # '69 users report 210 days', 
           '31 users report 240 days', 
           # '15 users report 270 days', 
           '8 users report 300 days'
       ), 
       col = c('black', 
               'steelblue2', 
               'red', 
               'green', 
               'orange'
       ), 
       lty = 1, lwd = 1, 
       cex = 1, box.lty = 0, y.intersp = 0.8, bg = NA)
dev.off()	

# for(i in 1:length(survey_length)){
#     print(length(which(table(user_alllabelled_days$user_id) >= survey_length[i]) ))
# }
# length(which(table(user_alllabelled_days$user_id) >= 300) )





