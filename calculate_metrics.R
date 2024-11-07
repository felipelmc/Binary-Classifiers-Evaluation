library(pROC)

calculate_metrics <- function(model, response_variable, threshold) {
  # Make predictions
  predictions_prob <- predict(model, type = "response")
  predictions <- ifelse(predictions_prob > threshold, 1, 0)
  
  # Create confusion matrix
  TP <- sum(predictions == 1 & response_variable == 1)
  FP <- sum(predictions == 1 & response_variable == 0)
  TN <- sum(predictions == 0 & response_variable == 0)
  FN <- sum(predictions == 0 & response_variable == 1)
  
  # Extract metrics
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Print metrics
  print(paste("Accuracy:", accuracy))
  print(paste("Precision:", precision))
  print(paste("Recall:", recall))
  print(paste("F1 Score:", f1_score))
  
  # Calculate ROC and AUC
  roc_result <- roc(response_variable, predictions_prob)
  auc_value <- auc(roc_result)
  
  # Plot ROC curve
  plot(roc_result)
  print(paste("AUC:", auc_value))
  
  # Calculate Log Loss
  log_loss <- -mean(response_variable * log(predictions_prob) + (1 - response_variable) * log(1 - predictions_prob))
  print(paste("Log Loss:", log_loss))
}