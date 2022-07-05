projectID <- "NC1401-A23"
library(readxl)
library(sf)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
setwd("~/git/rural_broadband/src")
path <- "~/../../../../project/biocomplexity/sdad/projects_data/usda/dspg_2022/"

CCG_2013_20 <- st_read(paste0(path, "RUS_CC_Shapefiles_Nov2021/CC_2013-2019-83_09042020/"),
                       "CC 2013_2019_83 09042020", int64_as_string = TRUE)
CCG_2018 <- CCG_2013_20[CCG_2013_20$OBLFY == '2018',]

CCG_project <- data.frame(matrix(ncol = 1, nrow = 1))
CCG_project_columns <- c("RUSID")      
colnames(CCG_project) <- CCG_project_columns
CCG_project$RUSID <- c(projectID)
CCG_row_of_interest <- CCG_2018[CCG_2018$RUSID==projectID, ]
CCG_project$geometry <- CCG_row_of_interest[1, 20]

# sf settings to avoid Loop 0 error
sf_use_s2(FALSE)
# get US state shapefile
states <- st_as_sf(states())

CCG_project$geometry <- st_transform(CCG_project$geometry,st_crs(states))


CCG_project['stateID'] <- NA
CCG_project['stateABBR'] <- NA

inds <- st_intersects(CCG_project$geometry, states$geometry, sparse=T)
for (i in 1:length(inds)){
  if (identical(states$NAME[inds[[i]]],character(0))){
    CCG_project$stateID[i] <- NA}
  else{
    CCG_project$stateID[i] <- list(states$GEOID[inds[[i]]])
    CCG_project$stateABBR[i] <- list(states$STUSPS[inds[[i]]])
  }}

shape <- CCG_project[CCG_project$RUSID == projectID,] 

mat = matrix(1:2, nr = 1, nc = 2, byrow = T)

layout(mat,
       widths = c(2, 2),
       heights = c(3, 3))

# plot 
ggplot() + geom_sf(data = states$geometry[states$GEOID %in% shape$stateID[[1]]], fill = shape$stateID[[1]]) +
  geom_sf(data = shape$geometry, col = "white")



#################################################
# FIND ALL PROPERTIES WIHIN 20mi OF PROGRAM AREA
#################################################

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape$geometry <- st_transform(shape$geometry, 5070)

# extend the shape boundary by 20 mi
outer_shape <- st_difference(st_buffer(shape$geometry, dist=1609.34*20), 
                             st_buffer(shape$geometry,0) )


outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(states))
inds <- st_intersects(outer_shape$geometry, states$geometry, sparse=T)
for (i in 1:length(inds)){
  if (identical(states$NAME[inds[[i]]],character(0))){
    outer_shape$stateID[i] <- NA}
  else{
    outer_shape$stateID[i] <- list(states$GEOID[inds[[i]]])
    outer_shape$stateABBR[i] <- list(states$STUSPS[inds[[i]]])
  }}
state_fips <- outer_shape$stateID[[1]]

# list to save data
datalist = list()

con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = Sys.getenv("db_pwd")) 

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

# property datapoint into coordinates
coords_CL <- datalist %>%
  st_as_sf(coords = c("property_centroid_longitude", "property_centroid_latitude"), crs = 4326)

# transform into the same projection as program shape
coords_CL <- st_transform(coords_CL, 5070)

# get CoreLogic lat/lons that fall within the boundary and a 10mi radius outside
# lists to save data
CL_inside <- list()
CL_outside <- list()

pts_inside <- st_within(coords_CL$geometry, shape$geometry)
ind_pts_inside <- which( sapply(pts_inside,length) > 0 )

CL_inside <- datalist[which( sapply(pts_inside,length) > 0 ),]

outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(coords_CL$geometry))
pts_outside <- st_within(coords_CL$geometry,outer_shape$geometry,sparse=TRUE)
CL_outside <- datalist[which( sapply(pts_outside,length) > 0 ),]

# plot with outer border and property sales
state_fips <- as.data.frame(state_fips)
ggplot() + geom_sf(data = states$geometry[states$GEOID %in% state_fips$state_fips], fill = state_fips$state_fips) +
  geom_sf(data = shape$geometry, col = "white") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=CL_outside, aes(x=property_centroid_longitude, y=property_centroid_latitude), color="orange", size=0.5) +
  geom_point(data=CL_inside, aes(x=property_centroid_longitude, y=property_centroid_latitude), color="lightgreen", size=0.5) 



