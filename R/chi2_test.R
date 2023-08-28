library(corrplot)
library(tidyverse)
library(raster)
library(sf)

records = read.csv("allPCA_valuesforRF.csv")
veg = raster("fuels_30m.tif")
murphy = st_read("fires_NSW.gpkg") |>
  st_transform(crs = st_crs(veg))
records = st_as_sf(records, coords = c("lon", "lat"), crs = st_crs(veg))
recbuff = st_buffer(records, dist = 12.5)

records = st_intersection(recbuff, murphy)
names(records)[names(records) == "NAME"] = "murphy"

records = records |>
  dplyr::select(halfbark:s_tessellated, murphy)
st_geometry(records) = NULL

records2 = read.csv("allPCA_valuesforRF_ribbons.csv")
records2 = st_as_sf(records2, coords = c("lon", "lat"), crs = st_crs(veg))
recbuff2 = st_buffer(records2, dist = 12.5)

records2 = st_intersection(recbuff2, murphy)
names(records2)[names(records2) == "NAME"] = "murphy"

records2 = records2 |>
  dplyr::select(high, some, murphy)
st_geometry(records2) = NULL

records = cbind(records, records2 |> dplyr::select(-murphy))
records = records |>
  dplyr::select(high, some, smooth, smooth_stocking, halfbark, s_box, ironbark, s_peppermint, stringybark, s_rough, s_stringy, s_tessellated, murphy)

murphy = read.csv("Murphy_reclassified.csv") # Murphy classes, grouped by frequency and tendency to support crown fires

murphy = records |>
  left_join(murphy)

m1 = aggregate(as.factor(murphy[,1]) ~ murphy$frequency + murphy$crownfires, FUN = function(x) length(x[x == 1]))
names(m1)[3] = paste0(names(murphy)[1], "_presence")
m0 = aggregate(as.factor(murphy[,1]) ~ murphy$frequency + murphy$crownfires, FUN = function(x) length(x[x == 0]))
names(m0)[3] = paste0(names(murphy)[1], "_absence")
df = inner_join(m1, m0)
for(i in c(2:12)){
  m1 = aggregate(as.factor(murphy[,i]) ~ murphy$frequency + murphy$crownfires, FUN = function(x) length(x[x == 1]))
  names(m1)[3] = paste0(names(murphy)[i], "_presence")
  m0 = aggregate(as.factor(murphy[,i]) ~ murphy$frequency + murphy$crownfires, FUN = function(x) length(x[x == 0]))
  names(m0)[3] = paste0(names(murphy)[i], "_absence")
  m_temp= inner_join(m1, m0)
  df = cbind(df, m_temp[,c(3:4)])
}
names(df)[1] = "murphy_frequency"
names(df)[2] = "murphy_crown"
records = df |>
  dplyr::select(high_presence:s_tessellated_absence, murphy_frequency, murphy_crown)

noms = c("Ribboning",
         "Some ribboning",
         "Smooth",
         "Smooth with stocking",
         "Halfbark",
         "Box",
         "Ironbark",
         "Peppermint",
         "Stringybark",
         "Subfibrous - rough",
         "Subfibrous - stringy",
         "Tessellated")

# murphy plots- all bark types Chi^2
records
dt = as.table(as.matrix(records[,c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23)]))
dimnames(dt)[[1]] = c("infrequent-facultative", "frequent-none", "infrequent-none", "rare-none", "infrequent-obligate", "rare-obligate")
chi = chisq.test(dt)
chi_dt = chi$residuals
contrib = 100*chi$residuals^2/chi$statistic
chi_dt = chi_dt[c(4, 3, 2, 1, 6, 5),]
colnames(chi_dt) = noms
rownames(chi_dt) = c("Rare - surface fires only", "Infrequent - surface fires only", "Frequent - surface fires only", "Infrequent - facultative crown fires", "Rare - crown fires only", "Infrequent - crown fires only")

chi_dt = chi_dt[,c(1:5, 7:12, 6)]
jpeg("allbarktypes_fireregimes_chi.jpg", width = 2250, height = 1050, res = 270)
corrplot(as.matrix(chi_dt), is.cor = F, tl.col = "black", tl.srt = 45, number.cex = 0.5)
dev.off()


chi_dt = chi_dt[,-12]
jpeg("allbarktypes_fireregimes_chi_nobox.jpg", width = 2250, height = 1050, res = 270)
corrplot(as.matrix(chi_dt), is.cor = F, tl.col = "black", tl.srt = 45, number.cex = 0.5)
dev.off()