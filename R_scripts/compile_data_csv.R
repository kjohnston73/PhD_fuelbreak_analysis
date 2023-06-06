# call libraries
library(dplyr); library(janitor)

# read in all summarized data for MANOVA
biomass<-read.csv(file = 'data/output_sum_data_csvs/biomass_sum.csv')%>% rename(PrimaryKey = plot)
cover<-read.csv(file = 'data/output_sum_data_csvs/cover_gap_2.csv')
max_ht<-read.csv(file = 'data/output_sum_data_csvs/hts_meanmax.csv')
pfg_ht<-read.csv(file = 'data/output_sum_data_csvs/hts_pfg_mean.csv')%>% rename(AG_ht = AG,
                                                                                PG_ht = PG,
                                                                                Forb_ht = Forb)
plot_info<-read.csv(file = 'data/raw_data_csvs/plot_info.csv')%>% rename(PrimaryKey = plot)


## add transformed data
biomass$load_3root <- (biomass$herbload)^(1/3)
max_ht$ht_sqrt <- (max_ht$max_height)^(1/2)
cover$gap_mean_log <- log(cover$gap_mean)

# # soils <- read.csv(file='AERO/aeroinputs/input_data.csv')%>% 
# #   clean_names() %>% rename(plot = "primary_key")
# # #condense by grouping variable to create new dataframe
# # soils_single <- soils %>% group_by(plot, sand, clay) %>% summarize(count=n())
# 
# create MANOVA dataframe and write csv
manova_data<-data.frame()
manova_data<-plot_info[, c("PrimaryKey", "graze")]
manova_data<-merge(manova_data, biomass[, c("PrimaryKey", "load_3root")], by = "PrimaryKey")
manova_data<-merge(manova_data, cover[1:13], by="PrimaryKey")
manova_data<-merge(manova_data, max_ht[1:3], by="PrimaryKey")
manova_data<-merge(manova_data, pfg_ht[1:4], by="PrimaryKey")

write.csv(manova_data, file = "data/manova_data.csv", row.names = F)

# italyposter2022$graze3 <- NA
# italyposter2022[italyposter2022$graze %in% c("ADJN"),
#                 "graze3"] <- "A"
# italyposter2022[italyposter2022$graze %in% c("N"),
#                 "graze3"] <- "NG"
# italyposter2022[italyposter2022$graze %in% c("ADJT"),
#                 "graze3"] <- "A"
# italyposter2022[italyposter2022$graze %in% c("T"),
#                 "graze3"] <- "TG"
# italyposter2022[italyposter2022$graze %in% c("ADJP"),
#                 "graze3"] <- "A"
# italyposter2022[italyposter2022$graze %in% c("P"),
#                 "graze3"] <- "PG"

# italyposter2022<-italyposter2022[-c(7)]
# italyposter2022<-italyposter2022[-c(17)]



## Read in .csv files for GLMM
plot_info<-read.csv(file = 'data/raw_data_csvs/plot_info.csv')#%>% rename(PrimaryKey = plot)
cover_gap <- read.csv(file = 'data/summarized_by_transect/cover_gap_tran2.csv')
ht_load <- read.csv(file = 'data/summarized_by_transect/hts_biomass_tran.csv')

## add transformed data
ht_load$load_3root <- (ht_load$herbload)^(1/3)
ht_load$ht_sqrt <- (ht_load$max_height)^(1/2)
cover_gap$gap_mean_log <- log(cover_gap$gap_mean)

# merge plot info with each data file
glmm_cover_gap_data <- merge(cover_gap, plot_info[6:23], by = "plot")
glmm_ht_load_data <- merge(ht_load, plot_info[6:23], by = "plot")

write.csv(glmm_cover_gap_data, file = "data/glmm_cover_gap_data.csv", row.names = F)
write.csv(glmm_ht_load_data, file = "data/glmm_ht_load_data.csv", row.names = F)
