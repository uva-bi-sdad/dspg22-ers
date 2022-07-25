# Get the counts of properties inside and outside the sevice area years before and after the program

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

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# read in shapes: 

# BIP 
BIP_shapes <- load(paste0(path, "interns_0622/bip_data/BIP_New.rds"))

#####################
# STATES SERVED

# sf settings to avoid Loop 0 error
sf_use_s2(FALSE)
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

#############################################################################
# FOR EACH PROG FIND HOW MANY PROPERTIES ARE INSIDE AND OUTSIDE THE PROG AREA
#############################################################################

# initialize lists to save out data
tot_in_count <- c()
tot_out_count <- c()
bef_in_count <- c()
bef_out_count <- c()
aft_in_count <- c()
aft_out_count <- c()

for(k in 226:length(newbip_union$ProjectID)){
  
  shape <- subset(newbip_union, ProjectID==newbip_union$ProjectID[k])
  print(paste0("Working on program ", newbip_union$ProjectID[k]))

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
      property_centroid_latitude, 
      property_centroid_longitude,
      DATE_PART('year', CAST(sale_date as TIMESTAMP)) AS sale_year
FROM
    corelogic_usda.current_tax_200627_latest_all_add_vars_add_progs_geom_blk
WHERE  property_indicator = '10'
AND    transaction_type != '9'
AND geoid_cnty LIKE '", state_id, "%'")
    # get the data 
    rows <- dbGetQuery(con, q)
    datalist<- rbind(datalist,rows)
  }
  dbDisconnect(con)
  
  # project in meters to compute distance, lat/lon to check against CoreLogic
  # Albers: 5070
  # WGS84: 4326
  shape <- st_transform(shape, 5070)
  
  #drop if coords are missing 
  datalist <- datalist[!is.na(datalist$property_centroid_latitude),]
  datalist <- datalist[!is.na(datalist$property_centroid_longitude),]
  
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
  
  tot_in_count <- append(tot_in_count, length(CL_inside$sale_price))
  tot_out_count <- append(tot_out_count, length(CL_outside$sale_price))
  
  bef_out_count <- append(bef_out_count, length(CL_outside$sale_price[CL_outside$sale_year <= 2010]))
  bef_in_count <- append(bef_in_count, length(CL_inside$sale_price[CL_inside$sale_year <= 2010]))
  
  aft_out_count <- append(aft_out_count, length(CL_outside$sale_price[CL_outside$sale_year > 2010]))
  aft_in_count <- append(aft_in_count, length(CL_inside$sale_price[CL_inside$sale_year > 2010]))
  
}

out_data <- list(state = newbip_union$stateABBR,
                 BIP_ID = newbip_union$ProjectID,
                 tot_in_20m = tot_in_count,
                 tot_out_20m = tot_out_count,
                 bef_in_20m = bef_in_count, 
                 bef_out_20m = bef_out_count,
                 aft_in_20m = aft_in_count,
                 aft_out_20m = aft_out_count
)
out_df <- as.data.frame(out_data)


