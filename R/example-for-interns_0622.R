# Example of broadband shape plot, property data query and linking the two together

# packages / install if necessary 
library(readxl)
library(sf)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)

# set a working directory
setwd("~/git/rural_broadband/src")

#######################
# PLOT PROGRAM SHAPES 
#######################

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# read in shapes: 

# BIP 

# working
BIP_project_data <- read_xlsx(paste0(path, "interns_0622/bip_data/BIP_R1-R2AwardeeMaster_10-29-10_updNetObl_7-3-19.xlsx"), sheet = 2)
# add ID variable in the same format as in the shape file for merging 
BIP_project_data['rusid'] <- paste0(BIP_project_data$`RUS Award No.`, "-", BIP_project_data$`RUS\r\nLoan-Grant No.`)
BIP_shapes <- load(paste0(path, "interns_0622/bip_data/BIP_New.rds"))

###### FYI #####
# original
BIP_shapes_orig <- st_read(paste0(path, "RUS_BIP_Shapefiles_April2020/"),"200409_BIP_ServAr_ID",
                      int64_as_string = TRUE, stringsAsFactors = FALSE) 

# BI porjects shapes are organized by borrower and need to be re-assembaled to project areas shapes
# when ProjectID is missing, fill in the RUSID instead
BIP_shapes_orig$ProjectID[is.na(BIP_shapes_orig$ProjectID)] <- BIP_shapes_orig$RUS_ID[is.na(BIP_shapes_orig$ProjectID)]

# aggregate geometry by ProjectID (1168 shapes, 228 unique ProjectIDs, 20 additional RUSIDs)
add_BIP_shapes <- BIP_shapes_orig %>% select(ProjectID, geometry) %>%
  aggregate(by=list(BIP_shapes_orig$ProjectID), first) %>% dplyr::select(-Group.1) %>% st_buffer(0)
###### FYI ######

# CommunityConnect shapefiles
CCG_project_data <- read_excel(paste0(path, "RUS_CC_Shapefiles_Nov2021/211103 Approved CC_Approved.xlsx"))

CCG_2013_20 <- st_read(paste0(path, "RUS_CC_Shapefiles_Nov2021/CC_2013-2019-83_09042020/"),"CC 2013_2019_83 09042020",
                         int64_as_string = TRUE)
CCG_2021 <- st_read(paste0(path,"RUS_CC_Shapefiles_Nov2021/FY21_CC_Approved_PFSA_Layer/"),"FY21_CC_Approved_PFSA_Layer",
                      int64_as_string = TRUE)

# ReConnect
RCP_round1 <- st_read(paste0(path, "RUS_RC_Shapefiles_August2021/ReConnect_984_PFSAs/"),"USDARD_PFSA_ReConnect",
                  int64_as_string = TRUE, stringsAsFactors = F)
RCP_round2 <- st_read(paste0(path, "RUS_RC_Shapefiles_August2021/ReConnect_Round_2_Final/"),"ReConnect_Round_2_Final",
                  int64_as_string = TRUE)

RCP_project_data <- read_excel(paste0(path, "RUS_RC_Shapefiles_August2021/Round 1 and 2 Submitted ReConnect Applications_8-6-2021.xlsx"))

#####################
# STATES SERVED

# sf settings to avoid Loop 0 error
sf_use_s2(TRUE)
# get US state shapefile
states <- st_as_sf(states())

# transform into the same coordinates system
newbip_union$geometry <- st_transform(newbip_union$geometry,st_crs(states))
# indeces of states which contain a geopoint
inds <- st_intersects(newbip_union$geometry, states$geometry, sparse=T)
newbip_union['stateID'] <- NA
newbip_union['stateABBR'] <- NA
for (i in 1:length(inds)){
  if (identical(states$NAME[inds[[i]]],character(0))){
    newbip_union$stateID[i] <- NA}
  else{
    newbip_union$stateID[i] <- list(states$GEOID[inds[[i]]])
    newbip_union$stateABBR[i] <- list(states$STUSPS[inds[[i]]])
  }}


#####################
# PLOT PROGRAM SHAPE 

# select a shape
shape <- newbip_union[newbip_union$ProjectID == "CO1105-A39",] 


# plot
mat = matrix(1:2, nr = 1, nc = 2, byrow = T)

layout(mat,
       widths = c(2, 2),
       heights = c(3, 3))

# plot 
ggplot() + geom_sf(data = states$geometry[states$GEOID %in% shape$stateID[[1]]], fill = shape$stateID[[1]]) +
  geom_sf(data = shape$geometry, col = "white")


#################################
# QUERY AND CLEAN CORELOGIC DATA
#################################

###### FYI #######
# working Corelogic dataset
# if(!"state_data.keep" %in% ls()) load(paste0(path, "interns_0622/bip_data/usda_er_bb_data1.rdata"))
###### FYI #######

# Database query for served states
# connect to DB
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = Sys.getenv("db_pwd")) 
# list to save data
datalist = list()

state_fips <- shape$stateID[[1]]

for (state_id in state_fips){
q = paste0("SELECT geoid_cnty, geoid_blk, sale_price, sale_date,
      property_centroid_latitude, property_centroid_longitude
FROM
    corelogic_usda.current_tax_200627_latest_all_add_vars_add_progs_geom_blk
WHERE  property_indicator = '10'
AND    transaction_type != '9'
AND   property_centroid_longitude IS NOT NULL
AND   property_centroid_latitude IS NOT NULL
AND   sale_date IS NOT NULL
AND   sale_price IS NOT NULL
AND (building_square_feet IS NOT NULL OR living_square_feet IS NOT NULL)
AND (acres IS NOT NULL OR land_square_footage IS NOT NULL)
AND (year_built IS NOT NULL OR effective_year_built IS NOT NULL)
AND geoid_cnty LIKE '", state_id, "%'")

# get the data 
rows <- dbGetQuery(con, q)
datalist<- rbind(datalist,rows)
}
dbDisconnect(con)

#################################################
# FIND ALL PROPERTIES WIHIN 10mi OF PROGRAM AREA
#################################################

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape <- st_transform(shape, 5070)

# property datapoint into coordinates
coords_CL <- datalist %>%
  st_as_sf(coords = c("property_centroid_longitude", "property_centroid_latitude"), crs = 4326)

# transform into the same projection as program shape
coords_CL <- st_transform(coords_CL, 5070)

# get CoreLogic lat/lons that fall within the boundary and a 10mi radius outside
# lists to save data
CL_inside <- list()
CL_outside <- list()

# extend the shape bounsary by 10 mi
outer_shape <- st_difference(st_buffer(shape, dist=1609.34*10), st_buffer(shape,0) )
outer_shape <- st_transform(outer_shape, 5070)

pts_inside <- st_within(coords_CL$geometry, shape$geometry)
ind_pts_inside <- which( sapply(pts_inside,length) > 0 )

CL_inside <- datalist[which( sapply(pts_inside,length) > 0 ),]

pts_outside <- st_within(coords_CL,outer_shape,sparse=TRUE)
CL_outside <- datalist[which( sapply(pts_outside,length) > 0 ),]

# plot with outer border and propery sales
ggplot() + geom_sf(data = states$geometry[states$GEOID %in% shape$stateID[[1]]], fill = shape$stateID[[1]]) +
  geom_sf(data = shape$geometry, col = "white") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=CL_outside, aes(x=property_centroid_longitude, y=property_centroid_latitude), color="orange", size=0.5) +
  geom_point(data=CL_inside, aes(x=property_centroid_longitude, y=property_centroid_latitude), color="lightgreen", size=0.5) 
  


