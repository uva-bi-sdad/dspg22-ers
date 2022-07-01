#######################################################################################
#Corelogic x Community Connect (new data set - seems to be filtered)
#######################################################################################

# setting up environment and libraries
library(readxl)
library(sf)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)
setwd("~/git/dspg22-ers/R")
path <- "~/../../../../project/biocomplexity/sdad/projects_data/usda/dspg_2022/"


# setting variables of interest
county_interest <- "VA1412-A23"
year_interest <- "2018"

con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = "kno5cac") 



#######################################################################################
# Gathering data
#######################################################################################

# County Shape from Community Connect
#######################################################################################
CCG_project_data <- read_excel(paste0(path, "RUS_CC_Shapefiles_Nov2021/211103 Approved CC_Approved.xlsx"))

CCG_2013_20 <- st_read(paste0(path, "RUS_CC_Shapefiles_Nov2021/CC_2013-2019-83_09042020/"),"CC 2013_2019_83 09042020",
                       int64_as_string = TRUE)

CCG_year <- CCG_2013_20[CCG_2013_20$OBLFY == year_interest,]
CCG_year_county <- CCG_year[CCG_year$RUSID == county_interest,]


#####################
# plotting a specific project area

# sf settings to avoid Loop 0 error
sf_use_s2(F)
# get US state shapefile
states <- st_as_sf(states())

CCG_county <- data.frame(matrix(ncol = 2, nrow = 1))
CCG_county_columns <- c("RUSID", "geometry")
colnames(CCG_county) <- CCG_county_columns
CCG_county$RUSID <- c(county_interest)
CCG_county$geometry <- CCG_year_county[1, 20]

shape <- CCG_county[CCG_county$RUSID == county_interest,]

mat = matrix(1:2, nr = 1, nc = 2, byrow = T)

layout(mat,
       widths = c(2, 2),
       heights = c(3, 3))

# plot
ggplot() + geom_sf(data = states$geometry[states$GEOID %in% shape$stateID[[1]]], fill = shape$stateID[[1]]) +
  geom_sf(data = shape$geometry, col = "pink")



# Property Data within 20 mi
#######################################################################################

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326

shape$geometry <- st_transform(shape$geometry, 5070)

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

for (state_id in state_fips){
  q = paste0("SELECT fips_code, block_level_latitude, block_level_longitude, assessed_total_value
             FROM
             corelogic_usda.combined_propertybasic_full
             WHERE  property_indicator_code = '10'
             AND block_level_latitude IS NOT NULL 
             AND block_level_latitude != ''
             AND block_level_longitude IS NOT NULL 
             AND block_level_longitude != ''
             AND fips_code LIKE '", state_id, "%'
             ")
  
  # get the data
  rows <- dbGetQuery(con, q)
  datalist<- rbind(datalist,rows)
}


# property datapoint into coordinates
coords_CL <- datalist %>%
  st_as_sf(coords = c("block_level_longitude", "block_level_latitude"), crs = 4326)

# transform into the same projection as program shape
coords_CL <- st_transform(coords_CL, 5070)


# get CoreLogic lat/lons that fall within the boundary and a 10mi radius outside
# lists to save data
CL_inside <- list()
CL_outside <- list()

pts_inside <- st_within(coords_CL$geometry, shape$geometry,sparse=TRUE)
ind_pts_inside <- which( sapply(pts_inside,length) > 0 )
CL_inside <- datalist[which( sapply(pts_inside,length) > 0 ),]

###################
# set sparse to false to see all corelogic in dataset, otherwise set to true to see for specific project area
###################
outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(coords_CL$geometry))
pts_outside <- st_within(coords_CL$geometry,outer_shape$geometry,sparse=T)
CL_outside <- datalist[which( sapply(pts_outside,length) > 0 ),]


# plot with outer border and propery sales
state_fips <- as.data.frame(state_fips)

ggplot() + geom_sf(data = states$geometry[states$GEOID %in% state_fips$state_fips], fill = state_fips$state_fips) +
  geom_sf(data = shape$geometry, col = "white") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=CL_outside, aes(x=as.numeric(block_level_longitude), y=as.numeric(block_level_latitude)), color="orange", size=0.5) +
  geom_point(data=CL_inside, aes(x=as.numeric(block_level_longitude), y=as.numeric(block_level_latitude)), color="lightgreen", size=0.5) 


dbDisconnect(con)
