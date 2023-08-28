# start ####
library(raster)
library(sf)
library(viridis)
library(tmap)

# aggregate files for mapping and reverse probabilities of all bark type distribution maps except 'Some ribboning' and 'Peppermint' ####
setwd("D:/chapter1/Major outputs/uncorrected maps")
preds = list.files(pattern = "aoapredmosaic")
preds = preds[grepl(".tif$", preds)]
preds = preds[c(1, 5:12, 2:4)]
labs = substr(preds, 15, 17)
labs[10:12] = c("RF10", "RF11", "RF12")
for(i in c(1:length(preds))){
  if(!file.exists(paste0(labs[i], "_agg100.tif"))){
    print(paste0("aggregating file ", preds[i]))
    r = raster(preds[i])
    r = aggregate(r, fact = 100)
    writeRaster(r, paste0(labs[i], "_agg100.tif"), overwrite = T)
  } else {
    print(paste0("file ", preds[i], " already exists"))
  }
  if(labs[i] != "RF11" | labs[i] == "RF7"){
    values(r) = 1 - values(r)
    writeRaster(r, paste0(labs[1], "_agg100_corrected.tif"), overwrite = T)
  } else {
    writeRaster(r, paste0(labs[1], "_agg100_corrected.tif"), overwrite = T)
  }
}

setwd("D:/chapter1/Major outputs/uncorrected maps")
preds = list.files(pattern = "aoapredmosaic")
preds = preds[grepl(".tif$", preds)]
preds = preds[1]
r = raster(preds)
r = aggregate(r, fact = 10)
values(r) = 1 - values(r)
setwd("D:/chapter1/Major outputs")
writeRaster(r, "RF1_agg10_corrected.tif", overwrite = T)

# mapping prep ####
setwd("D:/chapter1/Major outputs/uncorrected maps")
preds = list.files(pattern = "aoapredmosaic")
preds = preds[grepl(".tif$", preds)]
preds = preds[c(1, 5:12, 2:4)]
labs = substr(preds, 15, 17)
labs[10:12] = c("RF10", "RF11", "RF12")
setwd("D:/chapter1/Major outputs")
r = raster("RF1_agg100_corrected.tif")
setwd("D:/chapter1/data")
nsw = st_read("NSW_sans_islands.shp") |> 
  st_transform(crs = st_crs(r))
nsw_buff = st_buffer(nsw, dist = 50000)
aus = readRDS("gadm36_AUS_1_sp.rds")

# Fig 3 ####
setwd("D:/chapter1/Major outputs")
r = raster("RF1_agg10_corrected.tif")
threshold = 0.21

t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(r) + tm_raster(palette = rev(c("#032363", "#153b59", "#3b605d", "#8ab290", "#b5de9c")), title = "Probability", legend.reverse = TRUE) +
  tm_shape(nsw) +
  tm_borders() +
  tm_graticules(lines = F, labels.size = 1) +
  tm_layout(inner.margins = 0,
            legend.position = c("right","bottom"))
tmap_save(t1, filename = "Fig3_con.jpg")

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig3_cat.jpg")

# Fig 4- rf1 ####
setwd("D:/chapter1/Major outputs")
r = raster("RF1_agg100_corrected.tif")
threshold = 0.21

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = paste0("Fig4_", labs[1], ".jpg"))

t1 =
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_layout(inner.margins = 0,
            legend.position = c("right","bottom")) +
  tm_add_legend(type = "fill", 
                labels = c("Present", "Absent"),
                col = c("#153b59", "#b5de9c"),
                title = "Prediction")
tmap_save(t1, filename = "Fig4_legend.jpg")

t1 =
  tm_shape(nsw, bbox = nsw_buff) +
  tm_borders() +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(text.size = 1.5, position = c("left", "bottom"))
tmap_save(t1, filename = "Fig4_scalebar.jpg")

r = raster("RF2_agg100_corrected.tif")
threshold = 0.215

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF2.jpg")

r = raster("RF3_agg100_corrected.tif")
threshold = 0.375

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF3.jpg")

r = raster("RF4_agg100_corrected.tif")
threshold = 0.28

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF4.jpg")

r = raster("RF5_agg100_corrected.tif")
threshold = 0.4

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF5.jpg")

r = raster("RF6_agg100_corrected.tif")
threshold = 0.285

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF6.jpg")

r = raster("RF7_agg100_corrected.tif")
threshold = 0.13

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF7.jpg")

r = raster("RF8_agg100_corrected.tif")
threshold = 0.17

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF8.jpg")

r = raster("RF9_agg100_corrected.tif")
threshold = 0.175

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF9.jpg")

r = raster("RF10_agg100_corrected.tif")
threshold = 0.18

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF10.jpg")

r = raster("RF11_agg100_corrected.tif")
threshold = 0.525

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF11.jpg")

r = raster("RF12_agg100_corrected.tif")
threshold = 0.24

rpred = r
values(rpred)[values(rpred) < threshold] = 0
values(rpred)[values(rpred) >= threshold] = 1
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_borders() +
  tm_shape(rpred) + tm_raster(palette = rev(c("#153b59", "#b5de9c"))) +
  tm_shape(nsw) +
  tm_borders() +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F, labels.size = 1.5) +
  tm_layout(inner.margins = 0)
tmap_save(t1, filename = "Fig4_RF12.jpg")

# vegetation types map- Figure 2 ####
setwd("D:/chapter1/data")
# veg = raster("fuels_30m.tif") |>
#   aggregate(fact = 50, method = "rbg")
# writeRaster(veg, "fuels_1.5km.tif", overwrite = T)
# veg = raster("fuels_30m.tif")
# df = matrix(c(0, 5, 10, 20, 34, 41, 46, 47, 51,
#               5, 10, 20, 34, 41, 46, 47, 51, 74,
#               1, 2, 3, 4, 5, -Inf, 6, 7, -Inf),
#             ncol = 3, byrow = F)
# fire = reclassify(veg, rcl = df)
# writeRaster(fire, "veg_Fig2_30m.tif", overwrite = T)
# veg = raster("veg_Fig2_30m.tif")
# veg = aggregate(veg, fact = 2)
# writeRaster(veg, "veg_Fig2_60m.tif", overwrite = T)
veg = raster("veg_Fig2_60m.tif")

memory.limit(size=100000000)
t1 =
  tm_shape(aus, bbox = nsw_buff) +
  tm_fill(col = gray(75/100)) +
  tm_borders() +
  tm_shape(nsw) +
  tm_fill(col = "white") +
  tm_shape(veg) +
  tm_raster(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), palette = c(turbo(20)[c(1, 3, 5, 8, 11, 14, 17, 20)])) +
  tm_legend(show = FALSE) +
  tm_graticules(lines = F) +
  tm_shape(nsw) +
  tm_borders() +
  tm_layout(inner.margins = 0,
            legend.position = c("right","bottom"))
tmap_save(t1, filename = "Fig2.jpg")

# Australia with NSW inset ####
t1 =
  tm_shape(aus) + 
  tm_fill(col = gray(80/100)) +
  tm_borders() +
  tm_shape(nsw) + 
  tm_fill(col = gray(90/100)) +
  tm_borders(col = "red")
tmap_save(t1, filename = "Fig2_inset.jpg")
