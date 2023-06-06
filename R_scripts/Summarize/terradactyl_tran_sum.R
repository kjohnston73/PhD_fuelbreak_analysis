#install.packages("devtools")
# Install 'Rtools'
#devtools::install_github('Landscape-Data-Commons/terradactyl')
#install.packages("tidyverse")

library("terradactyl")
library("tidyverse")

# Transform LPI from wide to long format.
# **Only needed the first time running this script or if original lpi csv is altered.**

# # Read in wide LPI csv file
# lpi_wide <- read.csv("data/raw_data_csvs/LPI_raw.csv")
# # Transform to tall LPI
# lpi_tall <- pivot_longer(lpi_wide, cols = 5:10, names_to = "layer",
#                           values_to = "code", values_drop_na = TRUE)
# 
# # Add and populate LineKey & PointLoc columns
# lpi_tall$LineKey = paste(lpi_tall$PrimaryKey, lpi_tall$Line, sep = "")
# lpi_tall$PointLoc = lpi_tall$PointNbr
# 
# # Write tall LPI csv file
# write.csv(lpi_tall, file = 'data/special_input_csvs/LPI_Tall.csv', row.names = FALSE)


### Cover Indicators ###
#Read in tall LPI csv file
lpi_tall <- read.csv("data/special_input_csvs/LPI_Tall.csv")
lpi_tall$LineKey = lpi_tall$Line


#Calculate species percent cover
cover_species <- pct_cover(lpi_tall = lpi_tall,
          tall = F,
          hit = "any",
          by_line=TRUE,
          code
          )

cover_species <- cover_species[cover_species$LineKey !=0,]

cover_kopr80 <- cover_species[c('PrimaryKey', 'LineKey', 'KOPR80')]

# write species cover csv file
# write.csv(cover_species, file = 'data/summarized_by_transect/cover_by_species_transect.csv', row.names = F)

# # To pull out cover by single species code
# cover_kopr80 <-
#   pct_cover(lpi_tall = lpi_tall,
#             tall = T,
#             hit = "any",
#             by_line = T,
#             code) %>%
#   # subset to only cover of exposed soil
#   dplyr::filter(indicator == "KOPR80")

# To pull out cover by group of species combined
# define a grouping variable for IAG vs weed vs seeded vs other plant vs none percent cover
lpi_tall$Type <- "Other"
lpi_tall[lpi_tall$code %in% c("BRTE", "VUBR"),
         "Type"] <- "IAG"
lpi_tall[lpi_tall$code %in% c("SATR12", "SIAL2", "BASC5", "CENTA","CETE5", "CHJU", 
                              "DESO2", "LASE", "LEPE2", "POBU", "TAOF", "TRDU"),
         "Type"] <- "Weed"
lpi_tall[lpi_tall$code %in% c("N"),
         "Type"] <- "gap"
lpi_tallJA <- filter(lpi_tall, Region =='JA')
lpi_tallSH<- filter(lpi_tall, Region =='SH')
lpi_tallJA[lpi_tallJA$code %in% c("ACMI2", "AGROP", "KOPR80", "POSE", "LILE3"),
         "Type"] <- "Seeded"
lpi_tall<-rbind(lpi_tallJA, lpi_tallSH)

#Create new dataframe without the non-plant codes
library(dplyr)
lpi_plants_only <- filter(lpi_tall, !(code %in% c("S","L","W","R","DF","DG","DS")))

#condense by grouping variable to create new dataframe
lpi_groups <- lpi_plants_only %>% group_by(PrimaryKey, LineKey, Line, PointLoc, PointNbr,
                                    Region, Type) %>% summarize(count=n())
#remove 'count' column
lpi_groups <- subset(lpi_groups, select = -c(count))
#Rename 'Type' column to 'code'
names(lpi_groups)[names(lpi_groups) == 'Type'] <- 'code'
#add 'layer' column back in
lpi_groups$layer <- NA
# write csv file for ease later
write.csv(lpi_groups, file = "data/special_input_csvs/lpi_weed_groups.csv", row.names = FALSE)

#read in lpi_weed_groups csv file
lpi_weed_groups <- read.csv("data/special_input_csvs/lpi_weed_groups.csv")

# calculate cover by weed, seeded, and other species
cover_weed_groups <- pct_cover(lpi_tall = lpi_weed_groups,
                           tall = F,
                           hit = "any",
                           by_line=T,
                           code
                           )

cover_weed_groups <- cover_weed_groups[cover_weed_groups$LineKey !=0,]
cover_weed_groups$plot <- cover_weed_groups$PrimaryKey

ch1data<-read.csv("data/ch1_data.csv")
cover_weed_groups <- merge(cover_weed_groups, ch1data[, c('plot', 'region')], by = 'plot')
cover_weed_groups[cover_weed_groups$region %in% c("SH"), "SEEDED"] <- NA

cover_weed_groups$PrimaryKey <- paste(cover_weed_groups$PrimaryKey,cover_weed_groups$LineKey, sep = "_")
cover_weed_groups <- cover_weed_groups[, c(2,3,1,4,5,6,7,8)]

# write weed/seed/other cover csv
# write.csv(cover_weed_groups, file = "data/summarized_by_transect/WeedSeedOther_cover_transect.csv", row.names = F)

# #Calculate bare soil percent cover
# cover_baresoil <- pct_cover_bare_soil(lpi_tall = lpi_tall, tall = F, by_line = T) 
# 
# # write baresoil csv
# write.csv(cover_baresoil, file = "../data/output_sum_data_csvs/baresoil_cover.csv", row.names = F)

# calculate percent cover between plants                 
# L: Herbaceous litter. WL: Woody litter. S: Bare soil. R: Rock.
# see the Monitoring Manual (Herrick et al 2017) for code definitions
# cover_between_plants <- 
#   pct_cover_between_plant(lpi_tall = lpi_tall, 
#                           tall = F,
#                           by_line = F)

## Height indicators ###
# Read in wide height csv
# ht<-read.csv("data/raw_data_csvs/hts_wide_raw.csv")
# 
# ht$LineKey = ht$PointLoc
# 
# ht_tall <- pivot_longer(ht, cols = 5:7, names_to = "species",
#                          values_to = "Height", values_drop_na = TRUE)
# 
# # Write tall LPI csv file
# write.csv(ht_tall, file = 'data/special_input_csvs/ht_Tall.csv', row.names = FALSE)

#read tall height csv
ht_tall <- read.csv('data/special_input_csvs/ht_Tall.csv')

# calculate mean height for each plant functional group
ht_pfg_mean <- 
  mean_height(ht_tall,
              method = "mean",
              omit_zero = F,
              by_line = T,
              tall = F,
              species)

ht_pfg_mean[ht_pfg_mean == 0] <-NA

ht_pfg_mean$plot <- ht_pfg_mean$PrimaryKey
ht_pfg_mean$PrimaryKey <- paste(ht_pfg_mean$PrimaryKey,ht_pfg_mean$LineKey, sep = "_")

# write mean height csv
# write.csv(ht_pfg_mean, file = "data/summarized_by_transect/hts_pfg_mean_tran.csv", row.names = F)

# calculate mean of maximum heights at each point
ht_meanmax <-
  mean_height(ht_tall,
              method = "max",
              omit_zero = F,
              by_line = T,
              tall = T)

ht_meanmax$PrimaryKey <- paste(ht_meanmax$PrimaryKey,ht_meanmax$LineKey, sep = "_")

hts_tran<-merge(ht_pfg_mean, ht_meanmax[, c("PrimaryKey", "max_height")], by = "PrimaryKey")

hts_tran <- hts_tran[, c(1,2,6,3,4,5,7)]
hts_tran <- hts_tran %>%
  rename(
    ag_ht = AG,
    pg_ht = PG,
    forb_ht = Forb
  )

biomass <- read.csv("data/summarized_by_transect/biomass_tran_sum.csv")
biomass <- biomass %>%
  rename(
    dag_load = DAG,
    df_load = DF,
    dpg_load = DPG,
    l_load = L,
    lag_load = LAG,
    lf_load = LF,
    lpg_load = LPG
  )
hts_biomass_tran<-merge(hts_tran, biomass[, c(2,3,4,5,6,7,8,9,10,11)], by = "PrimaryKey")


# write mean max height csv
# write.csv(hts_biomass_tran, file = "data/summarized_by_transect/hts_biomass_tran.csv", row.names = F)

### Gap indicators ###
library(stringr)
# # Read in gap csv
# gap<- read.csv("data/raw_data_csvs/gap_raw.csv")
# 
# gap$LineKey = gap$TRANSECT
# gap$PrimaryKey = paste(gap$PrimaryKey, gap$LineKey, sep = "_")
# 
# # calculate gap cover indicators
# gap_out <- gap_cover(gap_tall = gap,
#             tall = F,
#             breaks = c(0),
#             type = "canopy",
#             by_line = T)
# 
# # write gap csv files
# write.csv(gap_out["percent"], file = "data/summarized_by_transect/gap_percent_tran.csv", row.names = F)
# write.csv(gap_out["n"], file = "data/summarized_by_transect/gap_n_tran.csv", row.names = F)
# write.csv(gap_out["length"], file = "data/summarized_by_transect/gap_length_tran.csv", row.names = F)
# 
# gap_percent = read.csv("data/summarized_by_transect/gap_percent_tran.csv")
# gap_n = read.csv("data/summarized_by_transect/gap_n_tran.csv")
# gap_length = read.csv("data/summarized_by_transect/gap_length_tran.csv")
# 
# gap_percent$plot <- gap_percent$percent.PrimaryKey %>% str_replace("_.*", "")
# gap_n$plot <- gap_n$n.PrimaryKey %>% str_replace("_.*", "")
# gap_length$plot <- gap_length$length.PrimaryKey %>% str_replace("_.*", "")
# 
# gap_length <- gap_length %>%
#   rename(
#     gap_length = length..0.1e.05.,
#     PrimaryKey = length.PrimaryKey,
#     lineLength = length.total_line_length
#   )
# 
# gap_n <- gap_n %>%
#   rename(
#     gap_n = n..0.1e.05.,
#     PrimaryKey = n.PrimaryKey,
#     lineLength = n.total_line_length
#   )
# 
# gap_percent <- gap_percent %>%
#   rename(
#     gap_percent = percent..0.1e.05.,
#     PrimaryKey = percent.PrimaryKey,
#     lineLength = percent.total_line_length
#   )
# 
# cover_gap_tran<-merge(cover_weed_groups, gap_length[, c("PrimaryKey", "gap_length")], by = "PrimaryKey")
# cover_gap_tran<-merge(cover_gap_tran, gap_n[, c("PrimaryKey", "gap_n")], by = "PrimaryKey")
# cover_gap_tran<-merge(cover_gap_tran, gap_percent[, c("PrimaryKey", "gap_percent")], by = "PrimaryKey")
# 
# # write.csv(gap_percent, file = "data/summarized_by_transect/gap_percent_tran.csv", row.names = F)
# # write.csv(gap_n, file = "data/summarized_by_transect/gap_n_tran.csv", row.names = F)
# # write.csv(gap_length, file = "data/summarized_by_transect/gap_length_tran.csv", row.names = F)
# write.csv(cover_gap_tran, file = "data/summarized_by_transect/cover_gap_tran.csv", row.names = F)


# # remove gaps <20cm and calculate gap cover indicators again
# aim_gaps <- gap[gap$Gap >=20, ]
# aim_gap_out <- 
#   gap_cover(gap_tall = aim_gaps,
#             tall = F,
#             breaks = c(0),
#             type = "canopy",
#             by_line = F)
# 
# # write aim gap csv files
# write.csv(aim_gap_out["percent"], file = "data/output_sum_data_csvs/aim_gap_percent.csv", row.names = F)
# write.csv(aim_gap_out["n"], file = "data/output_sum_data_csvs/aim_gap_n.csv", row.names = F)
# write.csv(aim_gap_out["length"], file = "data/output_sum_data_csvs/aim_gap_length.csv", row.names = F)

## final gaps
# Read in gap csv
gap<- read.csv("data/raw_data_csvs/gap_raw.csv")

gap$LineKey = gap$TRANSECT
gap$PrimaryKey = paste(gap$PrimaryKey, gap$LineKey, sep = "_")

# calculate gap cover indicators
gap_out <- gap_cover(gap_tall = gap,
                     tall = F,
                     breaks = c(10, 25, 51, 101, 201),
                     type = "canopy",
                     by_line = T)
gap_out2 <- gap_cover(gap_tall = gap,
                     tall = F,
                     breaks = c(0),
                     type = "canopy",
                     by_line = T)

# write gap csv files
write.csv(gap_out["percent"], file = "data/summarized_by_transect/gap_percent_tran_bins.csv", row.names = F)
write.csv(gap_out2["n"], file = "data/summarized_by_transect/gap_n_tran_2.csv", row.names = F)
write.csv(gap_out2["length"], file = "data/summarized_by_transect/gap_length_tran_2.csv", row.names = F)
write.csv(gap_out2["percent"], file = "data/summarized_by_transect/gap_total_percent_transect.csv", row.names = F)

gap_percent = read.csv("data/summarized_by_transect/gap_percent_tran_bins.csv")
gap_n = read.csv("data/summarized_by_transect/gap_n_tran_2.csv")
gap_length = read.csv("data/summarized_by_transect/gap_length_tran_2.csv")
gap_tot_perc = read.csv("data/summarized_by_transect/gap_total_percent_transect.csv")

gap_percent$plot <- gap_percent$percent.PrimaryKey %>% str_replace("_.*", "")
gap_n$plot <- gap_n$n.PrimaryKey %>% str_replace("_.*", "")
gap_length$plot <- gap_length$length.PrimaryKey %>% str_replace("_.*", "")

gap_length <- gap_length %>%
  rename(
    gap_length = length..0.1e.05.,
    PrimaryKey = length.PrimaryKey,
  )

gap_n <- gap_n %>%
  rename(
    gap_n = n..0.1e.05.,
    PrimaryKey = n.PrimaryKey,
  )

gap_mean<-merge(gap_length, gap_n[, c("PrimaryKey", "gap_n")], by = "PrimaryKey")
gap_mean$gap_mean <- gap_mean$gap_length/gap_mean$gap_n


gap_total_percent <- gap_tot_perc %>%
  rename(
    gap_percent = percent..0.1e.05.,
    PrimaryKey = percent.PrimaryKey,
    lineLength = percent.total_line_length
  )

gap_percent <- gap_percent %>%
  rename(
    gap_percent_lt25 = percent..10.25.,
    gap_percent_25_50 = percent..25.51.,
    gap_percent_51_100 = percent..51.101.,
    gap_percent_101_200 = percent..101.201.,
    gap_percent_gt200 = percent..201.1e.05.,
    PrimaryKey = percent.PrimaryKey,
    lineLength = percent.total_line_length
  )


cover_gap_tran2<-merge(cover_weed_groups, gap_mean[, c("PrimaryKey", "gap_mean")], by = "PrimaryKey")
cover_gap_tran2<-merge(cover_gap_tran2, gap_total_percent[, c("PrimaryKey", "gap_percent")], by = "PrimaryKey")
cover_gap_tran2<-merge(cover_gap_tran2, gap_percent[, c("PrimaryKey", "gap_percent_lt25",
                                                        "gap_percent_25_50", "gap_percent_51_100",
                                                        "gap_percent_101_200", "gap_percent_gt200")], by = "PrimaryKey")

write.csv(cover_gap_tran2, file = "data/summarized_by_transect/cover_gap_tran2.csv", row.names = F)
