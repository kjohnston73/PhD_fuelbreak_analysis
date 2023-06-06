library(dplyr)
library(tidyr)

area = 0.006 # transect size in hectares
den <- read.csv('data/raw_data_csvs/shrub_den_raw.csv') 

den$plot_sub <- paste(den$plot, den$transect, sep = "")

den$stems_ha <- (den$count)/area

stems_ha <- den[, c("plot", "plot_sub", "species_size", "stems_ha")]

stems_ha_wide <- pivot_wider(stems_ha, names_from = "species_size",
                           values_from = "stems_ha")

stems_ha_wide$total<- NA

for (i in 1:nrow(stems_ha_wide)){
  stems_ha_wide$total[i] <- sum(stems_ha_wide[i,3:7], na.rm=TRUE)
}

stems_ha_wide$total_live<- NA

for (i in 1:nrow(stems_ha_wide)){
  stems_ha_wide$total_live[i] <- sum(stems_ha_wide[i,3:6], na.rm=TRUE)
}

#write csv
write.csv(stems_ha_wide, file = "data/summarized_by_transect/shrub_density_tran_sum.csv", row.names = F)
