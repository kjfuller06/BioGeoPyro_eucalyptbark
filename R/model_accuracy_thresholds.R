library(caret)
library(randomforest)

records = read.csv("allPCA_valuesforRF.csv")

for(b1 in c(1:10)){
  num = as.numeric(b1)
  nom = names(records)[(num)]
  modelnom = paste0("RF", b1, "_", nom)
  final = readRDS(paste0(modelnom, "_final.rds"))
  
  test = read.csv(paste0(modelnom, "_testdata.csv"))
  test$group_PA = as.factor(test$group_PA)
  test$fire = as.factor(test$fire)
  test$murphy = as.factor(test$murphy)
  test$fire_reg = as.factor(test$fire_reg)
  
  rf_pred = predict(final, test, type = "prob")
  rf_pred = cbind(test, rf_pred)
  write.csv(rf_pred, paste0(modelnom, "_testpred.csv"), row.names = FALSE)
  
  rf_pred = predict(final, test)
  
  # generate prediction confusion matrix
  confusion = confusionMatrix(as.factor(rf_pred$pred), as.factor(test$group_PA))
  capture.output(
    confusion,
    file = paste0(modelnom, "_confusion.txt")
  )
  
  df = data.frame()
  for(i in seq(0, 1, by = 0.005)){
    predtemp = rf_pred
    predtemp$pred = NA
    predtemp$pred[predtemp$x1 > i] = "x1"
    predtemp$pred[predtemp$x1 <= i] = "x0"
    predtemp$pred = as.factor(predtemp$pred)
    
    spe = nrow(predtemp |> filter(group_PA == "x1" & pred == group_PA))/nrow(predtemp |> filter(group_PA == "x1"))
    sen = nrow(predtemp |> filter(group_PA == "x0" & pred == group_PA))/nrow(predtemp |> filter(group_PA == "x0"))
    df_temp = data.frame(prob = i,
                         specificity = spe, 
                         sensitivity = sen, 
                         balanced = mean(c(spe, sen)))
    df = rbind(df, df_temp)
  }
  
  with(df, plot(balanced ~ prob))
  df = df |> filter(balanced == max(df$balanced, na.rm = T))
  thresh = df |> dplyr::select(prob) |> as.numeric()
  
  rf_pred = predict(final, test, type = "prob")
  rf_pred = cbind(test, rf_pred)
  rf_pred$pred = NA
  rf_pred$pred[rf_pred$x1 > thresh] = "x1"
  rf_pred$pred[rf_pred$x1 <= thresh] = "x0"
  rf_pred$threshold = thresh
  write.csv(rf_pred, paste0(modelnom, "_testpred_manual.csv"), row.names = FALSE)
}

records = read.csv("allPCA_valuesforRF_ribbons.csv")

for(b1 in c(1:2)){
  num = as.numeric(b1+1)
  nom = names(records)[(num)]
  modelnom = paste0("RF", b1+10, "_", nom)
  final = readRDS(paste0(modelnom, "_final.rds"))
  
  test = read.csv(paste0(modelnom, "_testdata.csv"))
  test$group_PA = as.factor(test$group_PA)
  test$fire = as.factor(test$fire)
  test$murphy = as.factor(test$murphy)
  
  rf_pred = predict(final, test, type = "prob")
  write.csv(rf_pred, paste0(modelnom, "_testpred.csv"), row.names = FALSE)
  rf_pred = predict(final, test)
  
  # generate prediction confusion matrix
  confusion = confusionMatrix(rf_pred, as.factor(test$group_PA))
  capture.output(
    confusion,
    file = paste0(modelnom, "_confusion.txt")
  )
  
  df = data.frame()
  for(i in seq(0, 1, by = 0.005)){
    predtemp = rf_pred
    predtemp$pred = NA
    predtemp$pred[predtemp$x1 > i] = "x1"
    predtemp$pred[predtemp$x1 <= i] = "x0"
    predtemp$pred = as.factor(predtemp$pred)
    
    spe = nrow(predtemp |> filter(group_PA == "x1" & pred == group_PA))/nrow(predtemp |> filter(group_PA == "x1"))
    sen = nrow(predtemp |> filter(group_PA == "x0" & pred == group_PA))/nrow(predtemp |> filter(group_PA == "x0"))
    df_temp = data.frame(prob = i,
                         specificity = spe, 
                         sensitivity = sen, 
                         balanced = mean(c(spe, sen)))
    df = rbind(df, df_temp)
  }
  
  with(df, plot(balanced ~ prob))
  df = df |> filter(balanced == max(df$balanced, na.rm = T))
  thresh = df |> dplyr::select(prob) |> as.numeric()
  
  rf_pred = predict(final, test, type = "prob")
  rf_pred = cbind(test, rf_pred)
  rf_pred$pred = NA
  rf_pred$pred[rf_pred$x1 > thresh] = "x1"
  rf_pred$pred[rf_pred$x1 <= thresh] = "x0"
  rf_pred$threshold = thresh
  write.csv(rf_pred, paste0(modelnom, "_testpred_manual.csv"), row.names = FALSE)
}