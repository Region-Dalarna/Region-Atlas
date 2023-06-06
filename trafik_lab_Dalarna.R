#Från Trafiklab GTFS data

# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, sp, httr, mapview, leaflet)

# # clean
# rm(list = ls())
# invisible(gc())

source("G:/skript/func/func_GIS.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_postgis.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, 
                     username = key_list(service = "auth")$username, password = key_get("auth", key_list(service = "auth")$username)))
set_config(config(ssl_verifypeer = 0L))

options(dplyr.summarise.inform = FALSE)

# avoid scientific notation
options(scipen=999)

# create directory
dir.create("data_input")
dir.create("data_output")
dir.create("output")

wd = getwd()

data_input = paste0(wd,"/data_input")
data_output = paste0(wd,"/data_output")
output = paste0(wd,"/output")

# ### url for GTFS
# # Specify RKM. 

rkm = "dt" # !!!!!! Specify RKM. Available values : sl, ul, sormland, otraf, krono, klt, gotland, blekinge, skane, halland, vt, varm, orebro, vl, dt, xt, dintur, sj

lan_kod = "20" # !!!!!! Specify län kod, Uppsala = 03, Dalarna = 20

# todays date, used as filter
today = str_remove_all(Sys.Date(), "-")

# ============================== ladda ner gtfs från Trafiklab, Samtrafiken =================================

gtfs_regional_fil <- paste0(data_input, "/trafiklab_", rkm, ".zip")            # sätt ihop filnamn för gtfs-filen

## static GTFS timetable data from Trafiklab
url_regional <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", key_get("API_trafiklab_token", "GTFS_Regional"))

GET(url_regional, write_disk(gtfs_regional_fil, overwrite=TRUE))           # här laddar vi ned filen

# läs vilka filer som ingår i gtfs-zipfilen
gtfs_regional_filer <- unzip(zipfile = gtfs_regional_fil, list = TRUE)$Name 
#---------------------------------------------------------------------------------------------------
# Fetch DeSO and filter shapefile 
#---------------------------------------------------------------------------------------------------
deso_fil <- "G:/Samhällsanalys/GIS/grundkartor/deso/DeSO_2018_v2.gpkg"

deso <- st_read(deso_fil, crs = 3006)

deso = filter(deso, lan == lan_kod) # extract data for Uppsala län

mapview(deso)
