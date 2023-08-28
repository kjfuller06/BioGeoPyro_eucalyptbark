library(tidyverse)
library(sf)
library(cowplot)

records = read.csv("allPCA_valuesforRF_ribbons.csv")
noms = c("a) Ribboning", "b) Some ribboning")

l = list()
barkorder = c(2, 1)
for(i in c(1:2)){
  b1 = i
  num = as.numeric(barkorder[i]+1)
  nom = names(records)[(num)]
  lab = noms[b1]
  
  modelnom = paste0("RF", barkorder[i]+10, "_", nom)
  vars = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "# fires", "Fire regime", "FRI")
  
  final = readRDS(paste0(modelnom, "_final.rds"))
  
  temp = as.data.frame(varImp(final)$importance)
  temp$var = row.names(temp)
  temp$var = vars
  temp = temp[9:1,]
  temp$var = factor(temp$var, levels = temp$var)
  l[[i]] =
    ggplot(temp, aes(x = var, y = Overall)) +
    geom_bar(stat = "identity", width = 0.1) +
    geom_point(size = 1) +
    ylab("Importance") +
    xlab("") +
    coord_flip() + 
    ggtitle(lab) +
    theme_classic()
}

records = read.csv("allPCA_valuesforRF.csv")
records = records |> 
  dplyr::select(smooth,
                smooth_stocking,
                halfbark,
                s_box,
                ironbark,
                s_peppermint,
                stringybark,
                s_rough,
                s_stringy,
                s_tessellated)

noms = c("c) Smooth",
         "d) Smooth with stocking",
         "e) Halfbark",
         "f) Box",
         "g) Ironbark",
         "h) Peppermint",
         "i) Stringybark",
         "j) Subfibrous - rough",
         "k) Subfibrous - stringy",
         "l) Tessellated")

modnums = c(3, 4, 1, 6, 2, 7, 5, 8, 9, 10)

for(i in c(1:10)){
  num = as.numeric(i)
  nom = names(records)[(num)]
  lab = noms[num]
  
  modelnom = paste0("RF", modnums[i], "_", nom)
  vars = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "# fires", "Fire regime", "FRI")
  
  final = readRDS(paste0(modelnom, "_final.rds"))
  
  temp = as.data.frame(varImp(final)$importance)
  temp$var = row.names(temp)
  temp$var = vars
  temp = temp[9:1,]
  temp$var = factor(temp$var, levels = temp$var)
  l[[i+2]] =
    ggplot(temp, aes(x = var, y = Overall)) +
    geom_bar(stat = "identity", width = 0.1) +
    geom_point(size = 1) +
    ylab("Importance") +
    xlab("") +
    coord_flip() + 
    ggtitle(lab) +
    theme_classic()
}

plots = 
  plot_grid(l[[1]], l[[2]], l[[3]], l[[4]], l[[5]], l[[6]], l[[7]], l[[8]], l[[9]], l[[10]], l[[11]], l[[12]], align = "hv", axis = "tblr", nrow = 4, ncol = 3)
ggsave(plot = plots, "RF_varimp.jpg", width = 9, height = 8, dpi = 216)