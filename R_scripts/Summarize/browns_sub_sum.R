library(dplyr)
library(tidyr)

# set constant values
constant = 11.64
d2_1hr = 0.028826456
d2_10hr = 0.265319914
d2_100hr = 1.619608707
a = 1.13
s_1hr = 0.855451095
s_10hr = 0.583376339
s_100hr = 0.571547597
l_1_10hr = 6
l_100hr = 12
tac_to_kgha = 2471.0538

# read in data
browns<- read.csv('data/raw_data_csvs/browns_raw.csv') 

# calculate slope correction factor
browns$slope_corr <- sqrt(1+((browns$slope_perc/100)^2))

# calculate 1 hr load in tons/acre
browns$load_1hr <-((constant*d2_1hr*s_1hr*a*(browns$slope_corr)*(browns$count_1hr))/l_1_10hr)

# # compute average for each plot
# avg_1hr <- browns %>%
#   group_by(plot) %>%
#   summarise(load_1hr_tac = mean(load_1hr, na.rm = TRUE))
# # convert tons/acre average to kg/ha
# avg_1hr$load_1hr_kgha <- avg_1hr$load_1hr_tac*tac_to_kgha


# calculate 10 hr load in tons/acre
browns$load_10hr <-((constant*d2_10hr*s_10hr*a*(browns$slope_corr)*(browns$count_10hr))/l_1_10hr)
# # compute average for each plot
# avg_10hr <- browns %>%
#   group_by(plot) %>%
#   summarise(load_10hr_tac = mean(load_10hr, na.rm = TRUE))
# # convert tons/acre average to kg/ha
# avg_10hr$load_10hr_kgha <- avg_10hr$load_10hr_tac*tac_to_kgha


# calculate 100 hr load in tons/acre
browns$load_100hr <-((constant*d2_100hr*s_100hr*a*(browns$slope_corr)*(browns$count_100hr))/l_100hr)
# # compute average for each plot
# avg_100hr <- browns %>%
#   group_by(plot) %>%
#   summarise(load_100hr_tac = mean(load_100hr, na.rm = TRUE))
# # convert tons/acre average to kg/ha
# avg_100hr$load_100hr_kgha <- avg_100hr$load_100hr_tac*tac_to_kgha

browns$plot_sub <- paste(browns$plot, browns$transect, sep = "")

browns_sub <- browns[, c("plot","plot_sub", "slope_perc", "load_1hr", "load_10hr", "load_100hr")]

browns_sub$load_1hr_kgha <- browns_sub$load_1hr*tac_to_kgha
browns_sub$load_10hr_kgha <- browns_sub$load_10hr*tac_to_kgha
browns_sub$load_100hr_kgha <- browns_sub$load_100hr*tac_to_kgha

# create empty columns for totals
browns_sub$totalload_tac<- NA
browns_sub$totalload_kgha<- NA

# compute totals
for (i in 1:nrow(browns_sub)){browns_sub$totalload_tac[i] <- 
  (browns_sub$load_1hr[i] + browns_sub$load_10hr[i] + browns_sub$load_100hr[i])}

for (i in 1:nrow(browns_sub)){
  browns_sub$totalload_kgha[i] <- (browns_sub$load_1hr_kgha[i] + browns_sub$load_10hr_kgha[i] + browns_sub$load_100hr_kgha[i])
}

# convert load in kg/ha to Mg/ha for fire modeling
browns_sub$load_1hr_Mgha <- browns_sub$load_1hr_kgha*0.001
browns_sub$load_10hr_Mgha <- browns_sub$load_10hr_kgha*0.001
browns_sub$load_100hr_Mgha <- browns_sub$load_100hr_kgha*0.001
browns_sub$load_total_Mgha <- browns_sub$totalload_kgha*0.001

# write to csv
write.csv(browns_sub, file = "data/summarized_by_transect/browns_tran_sum.csv", row.names = F)
