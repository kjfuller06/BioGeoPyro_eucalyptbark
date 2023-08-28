# script for visualising environmental niches
## note: starting Nov. 12, I divided bioclim temp data by 10
# start ####
library(raster)
library(terra)
library(tidyverse)
library(sf)
# library(rnaturalearth)
library(viridis)
# library(RColorBrewer)
# library(tmap)
library(exactextractr)
# library(patchwork)
# library(ggpubr)
library(grid)
library(gridExtra)
library(cowplot)

# extract env variables ####
setwd("D:/chapter1/data")
# load base maps
veg = raster("fuels_30m.tif")
nsw = st_read("NSW_sans_islands.shp")

setwd("D:/chapter1/data/CGIARCSI data/et0_yr")
r1 = rast("et0_yr.tif")
nsw = st_transform(nsw, crs = st_crs(r1))
r1 = r1 |> 
  crop(nsw) |> 
  project(method = "bilinear", y = veg)
writeRaster(r1, "pet_fordensity.tif", overwrite = T)
setwd("D:/chapter1/data/wc2.1_30s_bio")
r2 = rast("wc2.1_30s_bio_12.tif")
nsw = st_transform(nsw, crs = st_crs(r2))
r2 = r2 |> 
  crop(nsw) |> 
  project(method = "bilinear", y = veg)
writeRaster(r2, "precip_fordensity.tif", overwrite = T)

setwd("D:/chapter1/data")
records = read.csv("allPCA14_valuesforRF.csv")
records = st_as_sf(records, coords = c("lon", "lat"), crs = st_crs(r1))
recbuff = st_buffer(records, dist = 12.5)

records$pet = exact_extract(r1, recbuff, fun = "mode")
records$precip = exact_extract(r2, recbuff, fun = "mode")
st_write(records, "barktype_envdistributions.gpkg", delete_dsn = T)

records = read.csv("allPCA14_valuesforRF_ribbons.csv")
records = st_as_sf(records, coords = c("lon", "lat"), crs = st_crs(r1))
recbuff = st_buffer(records, dist = 12.5)

records$pet = exact_extract(r1, recbuff, fun = "mode")
records$precip = exact_extract(r2, recbuff, fun = "mode")
st_write(records, "ribbons_envdistributions.gpkg", delete_dsn = T)

records = st_read("barktype_envdistributions.gpkg")
st_geometry(records) = NULL
records1 = records |>
  dplyr::select(smooth,
                smooth_stocking,
                halfbark,
                s_box,
                ironbark,
                s_peppermint,
                stringybark,
                s_rough,
                s_stringy,
                s_tessellated,
                PC1:precip)
records1$fire = as.factor(records1$fire)
records = st_read("ribbons_envdistributions.gpkg")
st_geometry(records) = NULL
records2 = records |>
  dplyr::select(high,
                some,
                PC1:precip)
records2$fire = as.factor(records2$fire)
records = inner_join(records2, records1)
records = records[!duplicated(records),]
records = records |>
  dplyr::select(high:some,
                smooth,
                smooth_stocking,
                halfbark,
                s_box,
                ironbark,
                s_peppermint,
                stringybark,
                s_rough,
                s_stringy,
                s_tessellated,
                PC1:precip)
write.csv(records, "envdist_forplots.csv", row.names = F)

# loop plotting ####
records = read.csv("envdist_forplots.csv")
records$fire = as.factor(records$fire)

noms = c("a) Ribboning",
         "b) Some ribboning",
         "c) Smooth",
         "d) Smooth with stocking",
         "e) Halfbark",
         "f) Box",
         "g) Ironbark",
         "h) Peppermint",
         "i) Stringybark",
         "j) Subfibrous - rough",
         "k) Subfibrous - stringy",
         "l) Tessellated")

# climate ####
basic = ggplot(records, aes(x = pet, y = precip)) +
  geom_density_2d_filled(contour_var = "ndensity", breaks = c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1)) +
  scale_fill_viridis(discrete = T, option = "B", begin = 0.1, end = 0.7) +
  ylim(min(records$precip), max(records$precip) + 5) +
  xlim(min(records$pet), max(records$pet))

l = list()
for(i in c(1:12)){
  l[[i]] = basic +
    geom_density_2d(data = records |> filter(across(all_of(names(records)[i]), ~ . == 1)), contour_var = "ndensity", col = "gray50", bins = 4, breaks = c(0.25, 0.5, 0.75, 1), size = 1) +
    geom_density_2d(data = records |> filter(across(all_of(names(records)[i]), ~ . == 1)), contour_var = "ndensity", col = "gray75", bins = 4, breaks = c(0.5, 0.75, 1), size = 1) +
    geom_density_2d(data = records |> filter(across(all_of(names(records)[i]), ~ . == 1)), contour_var = "ndensity", col = "white", bins = 4, breaks = c(0.75, 1), size = 1) +
    ylab("") +
    xlab("") +
    ggtitle(noms[i]) +
    theme(legend.position = "none",
          axis.title.y = element_text(margin = ggplot2::margin(l = -10)))
}
plots = 
  plot_grid(l[[1]] +theme(axis.text.x=element_blank()), 
            l[[2]] +theme(axis.text.y=element_blank(), 
                          axis.title.y = element_text(margin = ggplot2::margin(l = -20)), 
                          axis.text.x=element_blank()), 
            l[[3]] +theme(axis.text.y=element_blank(), 
                          axis.title.y = element_text(margin = ggplot2::margin(l = -20)), 
                          axis.text.x=element_blank()), 
            
            l[[4]] +theme(axis.text.x=element_blank()),
            l[[5]] +theme(axis.text.y=element_blank(), 
                          axis.title.y = element_text(margin = ggplot2::margin(l = -20)), 
                          axis.text.x=element_blank()),  
            l[[6]] +theme(axis.text.y=element_blank(), 
                          axis.title.y = element_text(margin = ggplot2::margin(l = -20)), 
                          axis.text.x=element_blank()), 
            
            l[[7]] +theme(axis.text.x=element_blank()),
            l[[8]] +theme(axis.text.y=element_blank(), 
                          axis.title.y = element_text(margin = ggplot2::margin(l = -20)), 
                          axis.text.x=element_blank()),
            l[[9]] +theme(axis.text.y=element_blank(), 
                          axis.title.y = element_text(margin = ggplot2::margin(l = -20)), 
                          axis.text.x=element_blank()),
            
            l[[10]], 
            l[[11]] +theme(axis.text.y=element_blank(), 
                           axis.title.y = element_text(margin = ggplot2::margin(l = -20))),
            l[[12]] +theme(axis.text.y=element_blank(), 
                           axis.title.y = element_text(margin = ggplot2::margin(l = -20))), 
            align = "hv", axis = "tblr", nrow = 4, ncol = 3)

y.grob <- textGrob(expression("Mean annual precipitation (mm y"^"-1"*")"), 
                   gp=gpar(fontface="bold", fontsize=15), rot=90)
x.grob <- textGrob(expression("Mean annual potential evapotranspiration (mm y"^"-1"*")"), 
                   gp=gpar(fontface="bold", fontsize=15))
plots = arrangeGrob(plots, left = y.grob, bottom = x.grob)
ggsave(plots, filename = "climate.jpeg", width = 9, height = 12)
# ggsave(plots, filename = "climate_test.jpeg", width = 9, height = 12)

# legends ####
basic = ggplot(records, aes(x = pet, y = precip)) +
  geom_density_2d_filled(contour_var = "ndensity", breaks = c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1)) +
  scale_fill_viridis(discrete = T, option = "B", begin = 0.1, end = 0.7, name = "Density") +
  ylim(min(records$precip), max(records$precip) + 5) +
  xlim(min(records$pet), max(records$pet))
ggsave(basic, filename = "envdist_legend_background.jpeg", width = 6, height = 3)

basic = ggplot(records, aes(x = pet, y = precip)) +
  geom_density_2d_filled(contour_var = "ndensity", bins = 4, breaks = c(0.25, 0.5, 0.75, 1)) +
  scale_fill_manual(values = c("gray50", "grey75", "white"), name = "Density") +
  ylim(min(records$precip), max(records$precip) + 5) +
  xlim(min(records$pet), max(records$pet))
ggsave(basic, filename = "envdist_legend_barktypes.jpeg", width = 6, height = 3)