b1 = 1
library(raster)
library(tidyverse)
library(sf)
library(caret)
library(randomForest)
library(doParallel)
library(snowfall)

r1 = raster("mosaic_PCA_PC1.tif")
records = read.csv("allPCA_valuesforRF.csv")

# select data
num = as.numeric(b1)
nom = names(records)[(num)]
modelnom = paste0("RF", b1, "_", nom)

train = read.csv(paste0(modelnom, "_traindata_withfolds.csv"))
folds <- CreateSpacetimeFolds(train, spacevar="foldID", k=10)
train = train %>% 
  dplyr::select(-foldID)
train$murphy = as.factor(train$murphy)
train$fire_reg = as.factor(train$fire_reg)
train$group_PA = as.factor(train$group_PA)
# train model
results = data.frame()
modellist = list()
rf_grid = expand.grid(.mtry = c(2:9),
                      .splitrule = c("gini", "extratrees"),
                      .min.node.size = c(1:40))
trainfun = function(x){
  set.seed(105)
  model <- train(train[, c(2:10)],
                 train$group_PA,
                 method="ranger",
                 importance= "permutation",
                 tuneGrid = rf_grid,
                 trControl = trainControl(method="cv",index=folds$index, classProbs = TRUE),
                 num.trees = x)
  results1 = as.data.frame(model$results)
  results1$ntree = x
  write.csv(results1, paste0(modelnom, "_tuning_", x, ".csv"), row.names = FALSE)
  saveRDS(model, file = paste0(modelnom, "_ntree", x, ".rds"))
}

cl = makePSOCKcluster(10)
registerDoParallel(cl)
trainfun(500)
trainfun(1000)
trainfun(1500)
stopCluster(cl)

# select top results
results1 = read.csv(paste0(modelnom, "_tuning_500.csv"))
results2 = read.csv(paste0(modelnom, "_tuning_1000.csv"))
results3 = read.csv(paste0(modelnom, "_tuning_1500.csv"))
results = rbind(results1, results2, results3)

top = results[results$Accuracy == max(results$Accuracy, na.rm = T),]
top = top[top$ntree == min(top$ntree, na.rm = T),]
top = top[1,]
ntree = top$ntree
mtry = top$mtry
splitrule = top$splitrule
min_n = top$min.node.size
label = paste("mtry",
              as.character(mtry), 
              "trees",
              as.character(ntree),
              "min_n",
              as.character(min_n),
              sep = "_")

# extract best model
final = readRDS(paste0(modelnom, "_ntree", ntree, ".rds"))
# save final model
saveRDS(final, file = paste0(modelnom, "_final.rds"))
capture.output(
  final$finalModel,
  file = paste0(modelnom, "_print.txt")
)
vars = as.data.frame(varImp(final,scale = T)$importance)
vars = data.frame(predictors = row.names(vars),
                  importance = vars$Overall)
write.csv(vars, paste0(modelnom, "_varstats.csv"), row.names = FALSE)

# generate test predictions
test = read.csv(paste0(modelnom, "_testdata.csv"))
test$fire_reg = as.factor(test$fire_reg)
test$murphy = as.factor(test$murphy)

rf_pred = predict(final, test, type = "prob")
rf_pred = cbind(test, rf_pred)
write.csv(rf_pred, paste0(modelnom, "_testpred.csv"), row.names = FALSE)