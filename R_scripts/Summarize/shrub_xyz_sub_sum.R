library(dplyr)
library(tidyr)

# read in xyz data
xyz <- read.csv('data/raw_data_csvs/shrub_xyz_raw.csv') 

# plot area
plot_area = pi*(3^2)

# compute volume
xyz$volume <- xyz$ht*xyz$d1*xyz$d2

# compute load using equation in Uresk et al. 1977 (table 3, total phytomass)
for (i in 1:nrow(xyz)){
  if (xyz$species[i] == "ARTTR"){
      xyz$load[i] <- 196 + (0.00102*(xyz$volume[i]))
  } else if (xyz$species[i] == "ARTRT"){
    xyz$load[i] <- 196 + (0.00102*(xyz$volume[i]))
  } else if (xyz$species[i] == "CHVI8"){
    xyz$load[i] <- 196 + (0.00102*(xyz$volume[i]))
  } else if (xyz$species[i] == "ERNA10"){
    xyz$load[i] <- 196 + (0.00102*(xyz$volume[i]))
  } else {
      xyz$load[i] = 0
  }
  }

# compute load in g/m2
load <- xyz %>% group_by(plot, transect_meter) %>% summarise(g_plot = sum(load))

load$g_m2 <- load$g_plot/plot_area

load$plot_sub = paste(load$plot, load$transect_meter, sep = "")

# compute load in Mg/ha for fire modeling
load$Mg_ha <- (load$g_m2)*0.01

output = load[, c("plot", "plot_sub", "g_m2", "Mg_ha")]

# write csv
write.csv(output, file = "data/summarized_by_transect/shrub_load_tran_sum.csv", row.names = F)
