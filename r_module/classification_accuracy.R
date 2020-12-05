

# Function to calculate overall classification accuracy and for each class ----

clf_accuracy <- function(
    labels, predicts){
    
    # Input description ----
    # labels: vector.
    # predicts: vector.
    # 
    # Value ----
    # overall accuracy and for each class: vector.
    
    
    confusion_table <- table(labels, predicts)
    
    overall_accuracy <- sum(diag(confusion_table)) / length(labels)
    single_accuracy <- diag(confusion_table) / rowSums(confusion_table)
    
    accuracy <- c(overall_accuracy, single_accuracy)
    
    return(accuracy)
}

# labels <- tp_train_original$prediction$labels
# predicts <- tp_train_original$prediction$rf_prediction

# clf_accuracy(tp_train_original$prediction$labels, 
#              tp_train_original$prediction$rf_prediction)





