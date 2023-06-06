library(dplyr)
library(tidyr)

area = 0.006 # transect size in hectares
den <- read.csv('data/raw_data_csvs/shrub_den_raw.csv') 

counts <- den %>%
  group_by(plot, species_size) %>%
  summarise(avg_count = (sum(count))/3)



counts$stems_ha <- (counts$avg_count)/area

counts <- counts[-3]

counts_wide <- pivot_wider(counts, names_from = "species_size",
                              values_from = "stems_ha")

counts_wide$total<- NA

for (i in 1:nrow(counts_wide)){
  counts_wide$total[i] <- sum(counts_wide[i,2:6], na.rm=TRUE)
}

#write csv
write.csv(counts_wide, file = "data/output_sum_data_csvs/shrub_density_sum.csv", row.names = F)
