library(dplyr)
library(tidyr)

quad = 0.000025 #size of quadrat in hectares
herbload <- read.csv('data/raw_data_csvs/biomass_raw.csv') #read in raw biomass data
herbload$dry_tare <- (herbload$DRY - herbload$TARE) # compute dry weight minus tare weight
herbload$load_kg_ha <- (herbload$dry_tare/1000)/quad # compute load in kg/ha for each veg type, subplot

herbload$PrimaryKey <- paste(herbload$plot, herbload$point, sep = "_") # create variable of plot & subplot

herbload_transect<-data.frame()
herbload_transect<-herbload[, c("plot","PrimaryKey", "VEG", "load_kg_ha")]

herbload_wide <- pivot_wider(herbload_transect, names_from = "VEG",
                              values_from = "load_kg_ha")

herbload_wide$totalload<- NA
herbload_wide$herbload<- NA

for (i in 1:nrow(herbload_wide)){herbload_wide$totalload[i] <- sum(herbload_wide[i,3:9], na.rm=TRUE)}

for (i in 1:nrow(herbload_wide)){herbload_wide$herbload[i] <- ((sum(herbload_wide[i,3:9], na.rm = TRUE)) - herbload_wide[i,6])}

# add herbload and litter load in Mg/ha for fire modeling
herbload_wide$herbload_Mgha<- NA
herbload_wide$litter_Mgha<- NA

herbload_wide$herbload <- as.numeric(unlist(herbload_wide$herbload))

for (i in 1:nrow(herbload_wide)){herbload_wide$herbload_Mgha[i] <- (herbload_wide$herbload[i]*0.001)}

for (i in 1:nrow(herbload_wide)){herbload_wide$litter_Mgha[i] <- (herbload_wide$L[i]*0.001)}


#herb_load_wide <- apply(herb_load_wide,2,as.character)

# herb_load_wide[herb_load_wide == 0] <- NA

#write csv
write.csv(herbload_wide, file = "data/summarized_by_transect/biomass_tran_sum.csv", row.names = F)
