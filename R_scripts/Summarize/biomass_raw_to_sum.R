library(dplyr)
library(tidyr)

quad = 0.000025 #size of quadrat in hectares
herbload <- read.csv('data/raw_data_csvs/biomass_raw.csv') 
herbload$dry_tare <- (herbload$DRY - herbload$TARE)
herbload$load_kg_ha <- (herbload$dry_tare/1000)/quad

avgherbload <- herbload %>%
                  group_by(plot, VEG) %>%
                  summarise(loadkgha = mean(load_kg_ha, na.rm = TRUE))

herb_load_wide <- pivot_wider(avgherbload, names_from = "VEG",
                         values_from = "loadkgha")

herb_load_wide$totalload<- NA
herb_load_wide$herbload<- NA

for (i in 1:nrow(herb_load_wide)){
  herb_load_wide$totalload[i] <- sum(herb_load_wide[i,2:8], na.rm=TRUE)
}

for (i in 1:nrow(herb_load_wide)){
  herb_load_wide$herbload[i] <- ((sum(herb_load_wide[i,2:8], na.rm = TRUE)) - herb_load_wide[i,5])
}

# add herbload and litter load in Mg/ha for fire modeling
herb_load_wide$herbload_Mgha<- NA
herb_load_wide$litter_Mgha<- NA

herb_load_wide$herbload <- as.numeric(unlist(herb_load_wide$herbload))

for (i in 1:nrow(herb_load_wide)){
  herb_load_wide$herbload_Mgha[i] <- (herb_load_wide$herbload[i]*0.001)
}

for (i in 1:nrow(herb_load_wide)){
  herb_load_wide$litter_Mgha[i] <- (herb_load_wide$L[i]*0.001)
}


#herb_load_wide <- apply(herb_load_wide,2,as.character)

# herb_load_wide[herb_load_wide == 0] <- NA

#write csv
write.csv(herb_load_wide, file = "data/output_sum_data_csvs/biomass_sum.csv", row.names = F)

##############################
biomass <- herb_load_wide %>%
  rename(
    dag_load = DAG,
    df_load = DF,
    dpg_load = DPG,
    l_load = L,
    lag_load = LAG,
    lf_load = LF,
    lpg_load = LPG
  )

ht_meanmax <- ht_meanmax %>%
  rename(
    plot = PrimaryKey
  )

ht_pfg_mean <- ht_pfg_mean %>%
  rename(
    plot = PrimaryKey,
    ag_ht = AG,
    pg_ht = PG,
    forb_ht = Forb
  )

biomass_hts_plot <- merge(biomass, ht_meanmax[, c(1,2)], by = "plot")
biomass_hts_plot <- merge(biomass_hts_plot, ht_pfg_mean, by = "plot")

pfg_cover<- read.csv("data/output_sum_data_csvs/WeedSeedOther_cover.csv")
gap_len <- read.csv("data/output_sum_data_csvs/gap_length.csv")
gap_n <- read.csv("data/output_sum_data_csvs/gap_n.csv")
gap_perc<-read.csv("data/output_sum_data_csvs/gap_percent.csv")

pfg_cover <- pfg_cover[1:6]
pfg_cover <- pfg_cover %>%
  rename(
    plot = PrimaryKey
  )

gap_len <- gap_len %>%
  rename(
    gap_length = length..0.1e.05.,
    plot = length.PrimaryKey
  )

gap_n <- gap_n %>%
  rename(
    gap_n = n..0.1e.05.,
    plot = n.PrimaryKey
  )

gap_perc <- gap_perc %>%
  rename(
    gap_percent = percent..0.1e.05.,
    plot = percent.PrimaryKey
  )

data_plot <- merge(biomass_hts_plot, pfg_cover, by = "plot")
data_plot <- merge(data_plot, gap_len[c(1,3)], by = "plot")
data_plot <- merge(data_plot, gap_n[c(1,3)], by = "plot")
data_plot <- merge(data_plot, gap_perc[c(1,3)], by = "plot")

write.csv(data_plot, file = "data/output_sum_data_csvs/data_plot.csv", row.names = F)
