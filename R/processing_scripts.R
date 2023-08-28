# code used to process datasets in preparation for analysis
# species data ####
library(tidyverse)
library(CoordinateCleaner)
library(rnaturalearth)
library(sf)
library(RColorBrewer)
library(raster)
library(spData)
library(tmap)

flora <- read.delim("BioNet_FloraSurveys_SightingsData_all.txt", header = TRUE, sep = "\t", dec = ".") %>%
  dplyr::select(ScientificName,
                Assgn_ScientificName,
                Exotic,
                NSWStatus,
                CommStatus,
                SensitivityClass,
                DateFirst,
                DateLast,
                NumberIndividuals,
                EstimateTypeCode,
                SourceCode,
                ObservationType,
                Status,
                Latitude_GDA94,
                Longitude_GDA94,
                Accuracy,
                Stratum,
                GrowthForm,
                CoverScore,
                AbundanceScore,
                PercentCover,
                LowerHeight,
                UpperHeight)

# list columns that will be converted to factor variables
columns=c("ScientificName", "Assgn_ScientificName","Exotic","NSWStatus","CommStatus","SensitivityClass","EstimateTypeCode","SourceCode","ObservationType","Status","Stratum","GrowthForm","CoverScore","AbundanceScore")
# convert columns to factors
flora[columns] = lapply(flora[columns], factor)

# convert columns to date variables
timefunction <- function(x) as.Date(x, format="%d/%m/%Y")
flora[c("DateFirst","DateLast")] = lapply(flora[c("DateFirst", "DateLast")], timefunction)

flora = flora %>% 
  filter(Accuracy <= 10) |> 
  filter(DateLast - DateFirst < 8) |> 
  filter(grepl("accepted", Status, ignore.case = TRUE)) |> 
  filter(PercentCover > 0) |> 
  filter(ObservationType == "J") |> 
  filter(SourceCode != 5 & SourceCode != 6)

unique = unique(flora[c("DateFirst", "DateLast", "Latitude_GDA94", "Longitude_GDA94")])
unique$ID = seq_len(nrow(unique))
flora = left_join(flora, unique)

unique = unique(flora[c("ScientificName", "Assgn_ScientificName")])
unique$SppID = seq_len(nrow(unique))
flora = left_join(flora, unique)

backup = flora
backup$ISO = "AUS"
land = st_read("ne_10m_land.shp") ## downloaded using the link https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip and unzipped
land = as(land, "Spatial")
backup = CoordinateCleaner::clean_coordinates(backup, 
                                              lon = "Longitude_GDA94",
                                              lat = "Latitude_GDA94",
                                              species = "SppID",
                                              countries = "ISO",
                                              country_ref = rnaturalearth:ne_countries(scale = 10),
                                              seas_ref = land,
                                              seas_scale = 10,
                                              tests = c("capitals", 
                                                        "centroids", 
                                                        "equal", 
                                                        "gbif", 
                                                        "institutions",
                                                        "seas", 
                                                        "zeros"),
                                              verbose = TRUE)

# remove failed observations and test columns
flora = backup %>% 
  filter(.cap == TRUE & .sea == TRUE & .summary == TRUE) %>% 
  dplyr::select(ID, 
                SppID,
                ScientificName,
                Assgn_ScientificName, 
                DateFirst,
                DateLast,
                Latitude_GDA94,
                Longitude_GDA94,
                GrowthForm) 

timefunction <- function(x) as.Date(x, format="%Y-%m-%d")
flora[c("DateFirst","DateLast")] = lapply(flora[c("DateFirst", "DateLast")], timefunction)
flora = flora %>% 
  filter(DateFirst > 1989-12-31)

flora$GrowthForm[flora$GrowthForm == "T"] = "Tree"
flora$GrowthForm[flora$GrowthForm == "M" | flora$GrowthForm == "S" | flora$GrowthForm == "U" | flora$GrowthForm == "Y" | flora$GrowthForm == "Z"] = "Mallee"

flora = flora %>% 
  unique()
write.csv(flora, "BioNet_allfloralsurvey_cleaned.csv", row.names = FALSE)

# -------------- assign bark traits ####
# This script is for selecting species for analysis from all cleaned BioNet species records.
library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(spData)

flora = read.csv("BioNet_allfloralsurvey_cleaned.csv")
sample = read.csv("candidate_species.csv")
names(sample) = c("ScientificName", "Assgn_ScientificName", "NicolleName")
traits = read.csv("allspecies_traits.csv")
names(traits)[1] = "NicolleName"

flora = flora %>% 
  left_join(sample) %>% 
  drop_na(NicolleName) %>% 
  unique()
flora$GrowthForm[flora$GrowthForm != "Mallee" & flora$GrowthForm != "Tree" & flora$GrowthForm != ""] = ""

switchers = c("Eucalyptus bakeri", "Eucalyptus elata", "Eucalyptus glaucescens", "Eucalyptus goniocalyx", "Eucalyptus morrisii", "Eucalyptus porosa", "Eucalyptus smithii", "Eucalyptus socialis", "Eucalyptus triflora")
flora2 = flora %>% 
  filter(!NicolleName %in% switchers)
switchers = flora %>% 
  filter(NicolleName %in% switchers) %>% 
  filter(GrowthForm == "Tree" | GrowthForm == "Mallee")
flora = full_join(flora2, switchers)

# select traits that exclude:
#   -"mallee"- the mallee form of a species that can be either tree or mallee
#   -"yes"- mallee-only species
trees = traits %>% 
  filter(mallee != "mallee" & mallee != "yes")
# select traits that exclude:
#   -"tree"- the tree form of a species that can be either tree or mallee
#   -"no"- tree-only species
mallees = traits %>% 
  filter(mallee != "tree" & mallee != "no")

flora2 = flora %>%
  filter(GrowthForm != "Mallee") %>% 
  full_join(trees)
flora2 = flora2 %>% 
  filter(!is.na(ID))
correction = flora2[is.na(flora2$mallee) == TRUE,] %>% 
  dplyr::select(-Horseybark1_final,
                -Horseybark2_final,
                -forming.ribbons_final,
                -mallee)
correction = inner_join(correction, mallees)
flora2 = rbind(flora2[is.na(flora2$mallee) == FALSE,], correction)

flora3 = flora %>% 
  filter(GrowthForm == "Mallee") %>% 
  left_join(mallees)
correction = flora3[is.na(flora3$mallee) == TRUE,] %>% 
  dplyr::select(-Horseybark1_final,
                -Horseybark2_final,
                -forming.ribbons_final,
                -mallee)
correction = inner_join(correction, trees)
flora3 = rbind(flora3[is.na(flora3$mallee) == FALSE,], correction)
flora = rbind(flora2, flora3) %>% 
  unique()

# get Australia layer
aus = getData(name = "GADM", country = "AUS", level = 1, download = TRUE) %>% 
  st_as_sf()

# create NSW layer with ACT included
nsw = aus %>% 
  filter(NAME_1 == "New South Wales" | NAME_1 == "Australian Capital Territory") %>% 
  dplyr::select(geometry)

# create polygon with an extent that hugs the NSW coastline so we can snip off stray islands
# points are introduced in sequence as they would be drawn on paper, with the last coordinates repeated. So a square will have 5 points.
bound = list(c( 154, -38), c(140, -38), c( 140, -28), c( 154, -28), c( 154, -38)) %>%
  unlist() %>%
  matrix(ncol = 2,
         byrow = TRUE) %>% 
  st_linestring %>% 
  st_cast('POLYGON') %>% 
  st_sfc(crs = 4326)

# now clip the nsw polygon using st_intersection
bound = st_intersection(nsw, bound)

# lastly, clip flora records by nsw boundary- minus islands
flora2 = st_as_sf(flora, coords = c("Longitude_GDA94", "Latitude_GDA94"), crs = 4326)
flora2 = flora2[bound, ]
flora_treesonly = flora2 %>% 
  filter(GrowthForm != "Mallee")

st_write(flora2, "species_sample.shp", delete_layer = TRUE)
st_write(flora_treesonly, "species_sample_treesonly.shp", delete_layer = TRUE)

# -------------- generate presence-absence values ####
library(tidyverse)
library(sf)

# load datasets
records = read.csv("BioNet_allfloralsurvey_cleaned.csv") %>% 
  dplyr::select(-DateFirst, -DateLast)
nomen = read.csv("candidate_species.csv") %>% 
  unique()
names(nomen) = c("ScientificName", "Assgn_ScientificName", "NicolleName")
traits = read_sf("species_sample.shp")
traits_trees = read_sf("species_sample_treesonly.shp")
veg = raster("fuels_30m.tif") # data from Kenny and Roberts 2016 "Building a comprehensive fuel map- from research to operational use"

# modify traits to include alternative names for species with more than one growth form
traits = traits %>% 
  st_transform(crs = st_crs(veg))
traits$lon = st_coordinates(traits)[,1]
traits$lat = st_coordinates(traits)[,2]
st_geometry(traits) = NULL
# subset out species with multiple growth forms
selection = unique(traits[,c(8, 9)]) %>% 
  group_by(NicllNm) %>% 
  tally() %>% 
  filter(n>1) %>% 
  as.data.frame() 
growthforms = unique(traits[traits$NicllNm %in% selection$NicllNm,][,c(8, 9, 12)])
growthforms$NicllNm_form = paste(growthforms$NicllNm, growthforms$mallee, sep = "_")
# recombine datasets
growthforms = growthforms %>% 
  left_join(traits)
names(growthforms)[c(1,4)] = c("NicolleName", "NicllNm")
traits$NicolleName = traits$NicllNm
traits = rbind(traits[!(traits$NicllNm %in% selection$NicllNm),], growthforms)

# create unified reference sheet for site-name-bark-type application to BioNet data
traits = left_join(traits, nomen) %>% 
  dplyr::select(-NicolleName)

# rename BioNet species
# first reproject coordinates, then turn back into data frame for efficiency
records = records %>% 
  st_as_sf(coords = c("Longitude_GDA94", "Latitude_GDA94"), crs = 4326) %>% 
  st_transform(crs = st_crs(veg))
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
records_all = records %>% 
  left_join(traits)
for(i in c(1:nrow(records_all))){
  if(is.na(records_all$NicllNm[i]) == FALSE){
    records_all$Assgn_ScientificName[i] = records_all$NicllNm[i]
  }
}
records_all = records_all[,c(1:4, 6:7)]

# convert back into sf and extract veg types
records_all = records_all %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(veg))
records_all = cbind(records_all, fuel = raster::extract(veg, st_coordinates(records_all), methods = 'simple'))
records_all = drop_na(records_all)

# generate presence/absence data per species 
# fill species columns with number of occurrences per unique combination of lon + lat
# remove geometry again
records_all$lon = st_coordinates(records_all)[,1]
records_all$lat = st_coordinates(records_all)[,2]
st_geometry(records_all) = NULL
# generate stats
PA = dcast(records_all, lon + lat ~ Assgn_ScientificName, fill = 0, value.var = "Assgn_ScientificName")

# select only species from Horsey selection
eucs = grep("Eucalyptus", colnames(PA))
angs = grep("Angophora", colnames(PA))
cors = grep("Corymbia", colnames(PA))
PA = PA[,c(1, 2, eucs, angs, cors)]

# replace non-zero observations with "1"
PA[,c(3:ncol(PA))] = PA[,c(3:ncol(PA))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))

# melt to long form
longf = melt(PA, id.vars = c("lon", "lat"))

# add bark traits back to long form df
longf = longf %>% 
  left_join(unique(traits[,c(8:12),]), by = c("variable" = "NicllNm"))
longf = longf %>% 
  filter(is.na(Hrsyb1_) == FALSE)

# bark types P-A
# select unique bark observations only from site trait observations
traits2 = traits %>% 
  dplyr::select(Hrsyb1_,
                Hrsyb2_,
                lon,
                lat)
names(traits2) = c("b1", "b2", "lon", "lat")
traits3 = traits %>% 
  dplyr::select(frmng__,
                lon,
                lat)
names(traits3) = c("ribbon", "lon", "lat")

# generate stats for bark traits
PA2 = dcast(traits2, lon + lat ~ b1, fill = 0, value.var = "b1")
PA2[,c(3:ncol(PA2))] = PA2[,c(3:ncol(PA2))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))
names(PA2) = c("lon",
               "lat",
               "halfbark",
               "ironbark",
               "smooth",
               "smooth_stocking",
               "stringybark",
               "s_box",
               "s_peppermint",
               "s_rough",
               "s_stringy",
               "s_tessellated")

# write to disk
write.csv(PA2, "site-specific_P-A_barks.csv", row.names = FALSE)

# ribboning P-A 
traits3 = traits %>% 
  dplyr::select(frmng__,
                lon,
                lat)
names(traits3) = c("ribbon", "lon", "lat")

# generate stats for bark traits
PA3 = dcast(traits3, lon + lat ~ ribbon, fill = 0, value.var = "ribbon")
PA3[,c(3:ncol(PA3))] = PA3[,c(3:ncol(PA3))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))
names(PA3) = c("lon",
               "lat",
               "none",
               "some",
               "high")

# write to disk
write.csv(PA3, "site-specific_P-A_ribbons.csv", row.names = FALSE)

# predictor layers ####
# -------------- rasterize Murphy et al. 2013 map ####
library(raster)
library(sf)
library(tidyverse)
library(parallel)
library(rgdal)

veg = raster("fuels_30m.tif")
nsw = st_read("NSW_sans_islands.shp") %>% 
  st_transform(crs = st_crs(veg))
murphy = st_read("Fire_regime_niches.shp") %>% # data from Murphy et al. 2013, Journal of Biogeography
  st_transform(crs = st_crs(veg)) |> 
  st_intersection(nsw) |> 
  dplyr::select(NAME)
targetcrs = st_crs(veg)
df = data.frame(ID = c(1:length(murphy$NAME)),
                murph = murphy$NAME)

# create raster template for rasterizing layers
iso_r = raster(ext = extent(veg), res = 30, crs = veg)

# convert murphy to Spatial format and rasterize in parallel
murphy_sp = as(as(murphy,"Spatial"), "SpatialPolygons")

for(x in c(1:23)){
  m_temp <- rasterize(murphy_sp[x,], iso_r, field = x)
  writeRaster(m_temp, paste0("murphy_rast_", x, ".tif"), overwrite = T)
}

l = list()
for(x in c(1:23)){
  l[[x]] <- raster(paste0("murphy_rast_", x, ".tif"))
}

# Merge all raster layers and reclassify based on fire return interval
murphy_r <- do.call(mosaic, unlist(l))
df = matrix(c(0,  1, 2, 3, 4, 5,  6, 7, 8, 9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
              1,  2, 3, 4, 5, 6,  7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
              18, 1, 2, 3, 4, 18, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14,  1, 15,  2,  3, 16,  4, 17), 
            byrow = F, ncol = 3)
r = reclassify(r, rcl = df)
writeRaster(r, "murphy_reclass.tif", over_write = T)

#--------------- convert aspect to northing and easting ####
library(data.table)

aspect = data.table::fread("dem_aspect_30m_forPCA.csv")

northness = aspect
names(northness)[3] = "north"

northness$north = cos(northness$north * pi / 180)

names(northness)[3] = "dem_northness_30m_forPCA"
data.table::fwrite(northness, "dem_northness_30m_forPCA.csv")

eastness = aspect
names(eastness)[3] = "east"

eastness$east = sin(eastness$east * pi / 180)

names(eastness)[3] = "dem_eastness_30m_forPCA"
data.table::fwrite(eastness, "dem_eastness_30m_forPCA.csv")

#--------------- project, mask, and convert layers to data frames ---------------------------
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(snowfall)
library(parallel)
library(tidyverse)
library(data.table)

veg = raster("fuels_30m.tif")

dats = list.files("./data", recursive = FALSE, full.names = TRUE)
dats = dats[!grepl("gri", dats)]

projfun = function(x){
  k = substr(dats[x], 8, nchar(dats[x])-7)
  s = raster(dats[x])
  s = projectRaster(s, veg, method = 'bilinear')
  
  s = mask(s, veg)
  df = as.data.frame(rasterToPoints(s), xy = TRUE)
  write.csv(df, paste0(k, "_forPCA.csv"), row.names = FALSE)
}

sfInit(parallel = TRUE, cpus = 66)
sfExport("veg", "dats", "projfun")
sfLibrary(slga)
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(rgdal)
sfLibrary(gdalUtils)

sfLapply(c(1:66), projfun)

sfStop()

forpca = list.files("./", pattern = "forPCA.csv$")

setDTthreads(8)
pca1 = data.table::fread(forpca[1])
for(i in c(2:length(forpca))){
  x = data.table::fread(forpca[i])
  pca1 = full_join(pca1, x)
}
pca1 = na.omit(pca1)
data.table::fwrite(pca1, "allvalues_forPCA.csv")

#--------------- PCA ------------------------
library(tidyverse)
library(data.table)
library(vegan)
library(factoextra)

# assign number of cores and read in data
setDTthreads(36)
input = data.table::fread("allvalues_forPCA_na.omit.csv")
input$ID = c(1:nrow(input))
xyd = input[,c(1, 2, ncol(input))]

# scale values and write to file
set.seed(225)
scaled = decostand(input[,c(1:(ncol(input)-1))], method = "range")
rm(input)
index = list(1:14, 15:40, 41:65, 66:90, 91:115, 116:ncol(scaled))

for(i in c(1:6)){
  setDTthreads(36)
  data.table::fwrite(scaled[,index[[i]]], paste0("PCA_scaledinputs", i, ".csv"))
}

# run PCA
set.seed(225)
mod = prcomp(scaled, scale = T)
rm(scaled)

# score outputs
sco = as.data.frame(scores(mod))
sco = cbind(xyd, sco)
for(i in c(1:6)){
  setDTthreads(36)
  data.table::fwrite(sco[,index[[i]]], paste0("PCA_values", i, ".csv"))
}
rm(sco)

# figures
tiff("PCA_eigvalfig.tiff", width = 500, height = 500, res = 100)
fviz_eig(mod)
dev.off()
tiff("PCA_sitecos2.tiff", width = 500, height = 500, res = 100)
fviz_pca_ind(mod,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
dev.off()
tiff("PCA_varfig.tiff", width = 500, height = 500, res = 100)
fviz_pca_var(mod,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
dev.off()

# site stats
res.ind = get_pca_ind(mod)
i1 = data.frame(res.ind$contrib)
i2 = data.frame(res.ind$cos2)
nom = c("contrib.", "cos2.")
for(i in c(1:length(names(i1)))){
  names(i1)[i] = paste(nom[1], i)
  names(i2)[i] = paste(nom[2], i)
}
i1 = cbind(xyd, i1)
i2 = cbind(xyd, i2)

setDTthreads(36)
for(i in c(1:6)){
  data.table::fwrite(i1[,index[[i]]], paste0("PCA_sitecontrib", i, ".csv"))
}
setDTthreads(36)
for(i in c(1:6)){
  data.table::fwrite(i2[,index[[i]]], paste0("PCA_sitecos2", i, ".csv"))
}
rm(res.ind, i1, i2)

capture.output(
  paste0("time to extract and write site stats = ", t5),
  file = "PCA_notes.txt",
  append = TRUE
)

## variable stats
eigval = get_eigenvalue(mod)
write.csv(eigval, "PCA_eigenvalues.csv")
rm(eigval)
res.var = get_pca_var(mod)
v1 = data.frame(res.var$coord)
v2 = data.frame(res.var$cor)
v3 = data.frame(res.var$cos2)
v4 = data.frame(res.var$contrib)
vars = list(v1, v2, v3, v4)
nom = c("coords.", "corr.", "cos2.", "contrib.")
for(a in c(1:4)){
  for(i in c(1:length(names(v1)))){
    names(vars[[a]])[i] = paste0(nom[a], i)
  }
}
for(a in c(1:4)){
  data.table::fwrite(vars[[i]], paste0("PCA_var", nom[a], ".csv"))
}
rm(res.var)

# prediction stats
v1 = data.frame(mod$center)
v2 = data.frame(mod$scale)
v3 = data.frame(mod$rotation)
vars = cbind(v1, v2, v3)
write.csv(vars, "PCA_predict.csv")

#--------------- extract PCA values for site data -------------------------
library(raster)
library(sf)

veg = raster("for_fuels_30m.tif")

records = read.csv("site-specific_P-A_barks.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(veg))

for(a in c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")){
  pc = paste0("PCA_", a, ".tif")
  for(i in c(1:6)){
    r = raster(pc[i])
    records = cbind(records,
                    as.numeric(raster::extract(r, st_coordinates(records), method = 'simple')))
    names(records)[ncol(records)-1] = paste0(a)
  }
}

r = raster("mask_proj_firehistory_post70_30m.tif")
records = cbind(records,
                fire = raster::extract(r, st_coordinates(records), method = 'simple'))

records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
write.csv(records, "site-specific_P-A_barks_PCAvalues.csv", row.names = FALSE)

records = na.omit(records)
write.csv(records, "allPCA_valuesforRF.csv", row.names = FALSE)

# ribbons
records = read.csv("site-specific_P-A_ribbons.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(veg))

for(a in c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")){
  pc = paste0("PCA_", a, ".tif")
  r = raster(pc)
  records = cbind(records,
                  as.numeric(raster::extract(r, st_coordinates(records), method = 'simple')))
  names(records)[ncol(records)-1] = paste0(a)
}

r = raster("mask_proj_firehistory_post70_30m.tif")
records = cbind(records,
                fire = raster::extract(r, st_coordinates(records), method = 'simple'))

records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
write.csv(records, "site-specific_P-A_ribbons_PCAvalues.csv", row.names = FALSE)

records = na.omit(records)
write.csv(records, "allPCA_valuesforRF_ribbons.csv", row.names = FALSE)