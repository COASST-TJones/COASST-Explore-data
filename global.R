##### Libraries
library(shiny)
library(shinyjs)
library(shinyalert)
library(plyr)
library(sp)
library(DT)
library(lubridate)
library(data.table)
library(here)

######################################################################
######################################################################
# read in data 

ctot <- read.csv(here::here("data","COASST_full.csv")) 
species.index <- read.csv(here::here("data","species_index.csv")) 
R.df <- read.csv(here::here("data","Regional_encounter_rate_data.csv")) 
BaseDF <- read.csv(here::here("data","Baseline_encounter_rate_data.csv")) 
BarPlotDATA <- read.csv(here::here("data","Barplot_data.csv")) 
TempTab <- read.csv(here::here("data","MiniTable_data.csv")) 

Species_list_drop <- unique(R.df$Species)
Full.Species.List <- c("All","Black-legged Kittiwake","Short-tailed Shearwater", 
                       "Common Murre", "Northern Fulmar", "Glaucous-winged Gull", 
                       "Sooty Shearwater", "Large Immature Gull", 
                       "Rhinoceros Auklet", "Surf Scoter", "White-winged Scoter",
                       "Cassin\'s Auklet", "Glaucous Gull", "Western Gull", 
                       "Brandt\'s Cormorant", "Common Loon") 

prior.list <- c("Available","Currently monitored")
Full.Year.list <- c("All", rev(seq(from=1999, to=year(Sys.Date()), by=1)))
######################################################################

######################################################################
######################################################################
# Define lists of species by region

SPECTAB <- list("Chukchi Sea"=c("All"),
"Bering Sea"=c("All"), 
"Aleutian Islands" = c("All"),
"Gulf of AK" = c("All"),
"Strait of Juan de Fuca" = c("All"), 
"Northern WA" = c("All","Common Murre","Northern Fulmar","Large Immature Gull","Rhinoceros Auklet","Glaucous-winged Gull","Sooty Shearwater"), 
"Southern WA" = c("All","Common Murre","Northern Fulmar","Large Immature Gull","Rhinoceros Auklet","Glaucous-winged Gull","Sooty Shearwater"), 
"Northern OR" = c("All","Common Murre","Northern Fulmar","Large Immature Gull","Rhinoceros Auklet","Glaucous-winged Gull","Sooty Shearwater"), 
"Southern OR" = c("All","Common Murre","Northern Fulmar","Large Immature Gull","Rhinoceros Auklet","Glaucous-winged Gull","Sooty Shearwater"), 
"Northern CA: Humboldt"=c("All","Common Murre","Northern Fulmar","Large Immature Gull","Rhinoceros Auklet","Glaucous-winged Gull","Sooty Shearwater"),
"Northern CA: Mendocino"=c("All","Common Murre","Northern Fulmar","Large Immature Gull","Rhinoceros Auklet","Glaucous-winged Gull","Sooty Shearwater"))

# Define start year/date for different regions
Region.MAPLS <- c("All","Chukchi Sea","Bering Sea","Aleutian Islands","Gulf of AK","Southeast AK",
	"San Juan Islands","Puget Sound","Strait of Juan de Fuca", 
	"Northern WA", "Southern WA", "Northern OR", "Southern OR", "Northern CA: Humboldt", "Northern CA: Mendocino")
YearBegin <- c(1999,2009,2006,2006,2005,2006,2000,2001,1999,1999,1999,2001,2001,2006,2014)
DateBegin <- as.Date(c("01-12-99","01-06-09","01-06-06","01-05-06","01-08-05","01-06-06","01-07-00","01-05-01","01-12-99", "01-12-99","01-12-99","01-01-01","01-11-01","01-09-06","01-05-14"), format="%d-%m-%y")
YBegin.df <- data.frame(Region=Region.MAPLS, YearBegin, DateBegin)

##############################################################################################################
##############################################################################################################
##############################################################################################################

ORDREG <- c("Chukchi Sea","Bering Sea","Aleutian Islands","Gulf of AK","Southeast AK",
	"San Juan Islands","Puget Sound","Strait of Juan de Fuca", 
	"Northern WA", "Southern WA", "Northern OR", "Southern OR", "Northern CA: Humboldt", "Northern CA: Mendocino")

########################################################
# Read in poly coords
poly_coords <- read.csv(here::here("data","poly_coords.csv"))

CH=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="CH"]), c(poly_coords$lat[poly_coords$reg=="CH"])))
BS=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="BS"]), c(poly_coords$lat[poly_coords$reg=="BS"])))
AI=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="AI"]), c(poly_coords$lat[poly_coords$reg=="AI"])))
GOA=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="GOA"]), c(poly_coords$lat[poly_coords$reg=="GOA"])))
SEAK=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="SEAK"]), c(poly_coords$lat[poly_coords$reg=="SEAK"])))
PS=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="PS"]), c(poly_coords$lat[poly_coords$reg=="PS"])))
SJ=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="SJ"]), c(poly_coords$lat[poly_coords$reg=="SJ"])))
ST=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="ST"]), c(poly_coords$lat[poly_coords$reg=="ST"])))
NC=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="NC"]), c(poly_coords$lat[poly_coords$reg=="NC"])))
SC=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="SC"]), c(poly_coords$lat[poly_coords$reg=="SC"])))
ON=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="ON"]), c(poly_coords$lat[poly_coords$reg=="ON"])))
OS=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="OS"]), c(poly_coords$lat[poly_coords$reg=="OS"])))
HU=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="HU"]), c(poly_coords$lat[poly_coords$reg=="HU"])))
ME=Polygon(cbind(c(poly_coords$long[poly_coords$reg=="ME"]), c(poly_coords$lat[poly_coords$reg=="ME"])))

CH1 = Polygons(list(CH), "CH1")
BS1 = Polygons(list(BS), "BS1")
AI1 = Polygons(list(AI), "AI1")
GOA1 = Polygons(list(GOA), "GOA1")
SEAK1 = Polygons(list(SEAK), "SEAK1")

PS1 = Polygons(list(PS), "PS1")
SJ1 = Polygons(list(SJ), "SJ1")
ST1 = Polygons(list(ST), "ST1")
NC1 = Polygons(list(NC), "NC1")
SC1 = Polygons(list(SC), "SC1")
ON1 = Polygons(list(ON), "ON1")
OS1 = Polygons(list(OS), "OS1")
HU1 = Polygons(list(HU), "HU1")
ME1 = Polygons(list(ME), "ME1")

SpP.AK = SpatialPolygons(list(CH1,BS1, AI1, GOA1, SEAK1), 1:5)
SpDf.AK <- SpatialPolygonsDataFrame(SpP.AK, data=data.frame(ID=c("Chukchi Sea", "Bering Sea", "Aleutian Islands", "Gulf of AK", "Southeast AK")), match.ID = F)


SpP.L48 = SpatialPolygons(list(SJ1,PS1, ST1, NC1, SC1, ON1, OS1, HU1, ME1), 1:9)
SpDf.L48 <- SpatialPolygonsDataFrame(SpP.L48, data=data.frame(ID=c("San Juan Islands", "Puget Sound", "Strait of Juan de Fuca", "Northern WA", "Southern WA", "Northern OR", "Southern OR", "Northern CA: Humboldt", "Northern CA: Mendocino")), match.ID = F)

################################
################################

rownames(TempTab) <- TempTab$reg.vis	
	
	
	