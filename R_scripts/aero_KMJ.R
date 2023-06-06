library(terradactyl)


#Read in texture layer, save as .Rdata
texture<-raster::stack("./AERO/soil_grids_sand_clay/sand_M_sl1_100m_reproject.tif",
                       "./AERO/soil_grids_sand_clay/clay_M_sl1_100m_reproject.tif")
names(texture)<-c("sand", "clay")
saveRDS(texture, "./AERO/aerodata/texture_raster.rdata")

#Read in lpi data, save as .Rdata
lpi <- read.csv("./data/special_input_csvs/LPI_Tall.csv")
saveRDS(lpi, "./AERO/aerodata/lpi_tall.rdata")

#Read in header file, save as .Rdata
header <- read.csv("./AERO/aerodata/sagestep_lat_long.csv")
saveRDS(header, "./AERO/aerodata/sagestep_header.rdata")

#Read in height data, save as .Rdata
ht_tall <- read.csv("./data/special_input_csvs/ht_Tall.csv")
saveRDS(ht_tall, "./AERO/aerodata/height_tall.rdata")

#Read in gap data, save as .Rdata
gap <- read.csv("./data/raw_data_csvs/gap_raw.csv")
saveRDS(gap, "./AERO/aerodata/gap_tall.rdata")

terradactyl::aero(lpi_tall = readRDS("./AERO/aerodata/lpi_tall.Rdata"),
                  header = readRDS("./AERO/aerodata/sagestep_header.Rdata"), 
                  height_tall = readRDS("./AERO/aerodata/height_tall.Rdata"),
                  gap_tall = readRDS("./AERO/aerodata/gap_tall.Rdata"),
                  texture_file = "./AERO/aerodata/texture_raster.Rdata",
                  folder_location="./AERO/aeroinputs/")

# terradactyl::aero(lpi_tall = readRDS("~/DataCommons/NRI/ManipulatedData/NRI_2004-2018/lpi_tall.Rdata"),
#                   header = readRDS("~/DataCommons/NRI/ManipulatedData/NRI_2004-2018/header.Rdata"), 
#                   height_tall = readRDS("~/DataCommons/NRI/ManipulatedData/NRI_2004-2018/height_tall.Rdata"),
#                   gap_tall = readRDS("~/DataCommons/NRI/ManipulatedData/NRI_2004-2018/gap.Rdata"),
#                   folder_location="~/DataCommons/AERO/aero_nri_2020-03-23/", 
#                   texture_raster = readRDS("~/DataCommons/AERO\\SOILGRIDS/texture_raster.Rdata"))
