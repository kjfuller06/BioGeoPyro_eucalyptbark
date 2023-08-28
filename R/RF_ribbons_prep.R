library(Rcpp)
library(raster)
library(tidymodels)
library(tidyverse)
library(sf)
library(randomForest)
library(alookr)
library(CAST)
library(caret)
library(blockCV)
library(exactextractr)

r1 = raster("mosaic_PCA_PC1.tif")
rec = read.csv("allPCA_valuesforRF_ribbons.csv") |>
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(r1))
murphy = raster("murphy_reclass.tif")
veg = raster("fuels_30m.tif")

recbuff = st_buffer(records, dist = 12.5)
recbuff = st_transform(recbuff, crs = st_crs(veg))

records$murphy = exact_extract(murphy, recbuff, fun = "mode")
records$veg = exact_extract(veg, recbuff, fun = "mode")

fireregs = read.csv("veg_fireregimes.csv")
records = records |>
  left_join(fireregs) |>
  dplyr::select(-veg) |>
  filter(!is.na(fire_reg)) |> 
  filter(!is.na(murphy))

for(b1 in c(2, 3)){
  # select data
  num = as.numeric(b1)
  nom = names(rec)[(num)]
  modelnom = paste0("RF", b1+9, "_", nom)

  rec1 = records |>
    dplyr::select(all_of(num), c(4:ncol(records)))
  names(rec1)[1] = "group_PA"
  rec1$group_PA = paste0("x", rec1$group_PA)
  rec1$lon = st_coordinates(rec1)[,1]
  rec1$lat = st_coordinates(rec1)[,2]
  st_geometry(rec1) = NULL
  
  set.seed(5)
  sb = rec1 %>%
    initial_split(strata = group_PA, prop = 7/10, seed = 5)
  test = testing(sb)
  write.csv(test, paste0(modelnom, "_testdata.csv"), row.names = F)
  
  train = training(sb)
  print("nrow(train) = ")
  print(nrow(train))
  
  train$group_PA = as.factor(train$group_PA)
  train$murphy = as.factor(train$murphy)
  train$fire_reg = as.factor(train$fire_reg)
  train = st_as_sf(train, coords = c("lon", "lat"), crs = st_crs(r1))
  
  # spatial blocking by specified range with random assignment
  sb <- spatialBlock(speciesData = train,
                     species = "group_PA",
                     theRange = 114807, # size of the blocks
                     k = 10,
                     selection = "random",
                     iteration = 100, # find evenly dispersed folds
                     biomod2Format = FALSE,
                     xOffset = 0,
                     yOffset = 0,
                     seed = 10)
  
  # reformat for next function
  train = st_transform(train, crs = st_crs(r1))
  train$lon = st_coordinates(train)[,1]
  train$lat = st_coordinates(train)[,2]
  st_geometry(train) = NULL
  train$foldID = as.factor(sb$foldID)
  
  # reformat folds for use in following functions
  write.csv(train, paste0(modelnom, "_traindata_withfolds.csv"), row.names = FALSE)
}