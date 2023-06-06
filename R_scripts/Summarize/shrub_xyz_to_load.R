library(dplyr)
library(tidyr)

# read in xyz data
xyz <- read.csv('data/raw_data_csvs/shrub_xyz_raw.csv') 

# plot area
plot_area = pi*(3^2)

# compute volume
xyz$volume <- xyz$ht*xyz$d1*xyz$d2

# compute load using equation in Uresk et al. 1977 (table 3, total phytoomass)
xyz$load <- 196 + (0.00102*(xyz$volume))

# compute load in g/m2
load <- xyz %>%
  group_by(plot) %>%
  summarise(g_plot = (sum(load))/5)

load$g_m2 <- load$g_plot/plot_area

# compute load in Mg/ha for fire modeling
load$Mg_ha <- (load$g_m2)*0.01

# write csv
write.csv(load, file = "data/output_sum_data_csvs/shrub_load_sum.csv", row.names = F)
