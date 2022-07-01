# pull Ookla tile data 2019-2021 from database
library(readxl)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)
library(sf)
library(RPostgreSQL)

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }


list_db_schemas <- function(db_con) {
  result <- DBI::dbGetQuery(db_con, "select schema_name from information_schema.schemata")
  DBI::dbDisconnect(db_con)
  return(result)
}

list_schema_tables <- function(db_con, db_schema) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_name FROM information_schema.tables
                                           WHERE table_schema='", db_schema, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}

list_table_columns <- function(db_con, db_schema, db_table) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_schema, table_name, column_name, data_type
                                           FROM information_schema.columns
                                           WHERE table_schema = '", db_schema, "'",
                                           " AND table_name = '", db_table, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}


# EXAMPLES ----
# List Schemas
con <- get_db_conn()
list_db_schemas(con)

# List Tables in a Schema
con <- get_db_conn()
list_schema_tables(con, "dc_working")

# List Columns in a Table
con <- get_db_conn()
list_table_columns(con, "dc_working", "us_tile_ookla_2019_2021")

#con <- get_db_conn()
#ookla_us <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 LIMIT 100")
#DBI::dbDisconnect(con)
# read in all Ookla records by quarter from 2019 to 2021
con <- get_db_conn()
ookla_2019q1 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '1'")
ookla_2021q4 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '4'")
DBI::dbDisconnect(con)

setwd("~/git/rural_broadband/src")
path <- "~/../../../../project/biocomplexity/sdad/projects_data/usda/dspg_2022/"
CCG_2013_20 <- st_read(paste0(path, "RUS_CC_Shapefiles_Nov2021/CC_2013-2019-83_09042020/"),
                       "CC 2013_2019_83 09042020", int64_as_string = TRUE)
CCG_2018 <- CCG_2013_20[CCG_2013_20$OBLFY == '2018',]
CCG_VA1412 <- data.frame(matrix(ncol = 1, nrow = 1))
CCG_VA1412_columns <- c("RUSID")
colnames(CCG_VA1412) <- CCG_VA1412_columns
CCG_VA1412$RUSID <- c("VA1412-A23")
CCG_VA1412$geometry <- CCG_2018[13, 20]


# sf settings to avoid Loop 0 error
sf_use_s2(FALSE)

# get US state shapefile
states <- st_as_sf(states())
CCG_VA1412$geometry <- st_transform(CCG_VA1412$geometry,st_crs(states))
CCG_VA1412['stateID'] <- NA
CCG_VA1412['stateABBR'] <- NA
inds <- st_intersects(CCG_VA1412$geometry, states$geometry, sparse=T)

for (i in 1:length(inds)){
  if (identical(states$NAME[inds[[i]]],character(0))){
    CCG_VA1412$stateID[i] <- NA}
  else{
    CCG_VA1412$stateID[i] <- list(states$GEOID[inds[[i]]])
    CCG_VA1412$stateABBR[i] <- list(states$STUSPS[inds[[i]]])
  }}

shape <- CCG_VA1412[CCG_VA1412$RUSID == "VA1412-A23",]

mat = matrix(1:2, nr = 1, nc = 2, byrow = T)

layout(mat,
       widths = c(2, 2),
       heights = c(3, 3))

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

outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(ookla_2019q1$geometry))
tiles_outside1 <- st_within(ookla_2019q1$geometry, outer_shape$geometry)
ind_tiles_outside1 <- which( sapply(tiles_outside1,length) > 0 )
ookla_outside_initial <- ookla_2019q1[ind_tiles_outside1,]

shape$geometry <- st_transform(shape$geometry,st_crs(ookla_2019q1$geometry))
tiles_inside1 <- st_within(ookla_2019q1$geometry, shape$geometry)
ind_tiles_inside1 <- which( sapply(tiles_inside1,length) > 0 )
ookla_inside_initial <- ookla_2019q1[ind_tiles_inside1,]

outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(ookla_2021q4$geometry))
tiles_outside2 <- st_within(ookla_2021q4$geometry, outer_shape$geometry)
ind_tiles_outside2 <- which( sapply(tiles_outside2,length) > 0 )
ookla_outside_post <- ookla_2021q4[ind_tiles_outside2,]

shape$geometry <- st_transform(shape$geometry,st_crs(ookla_2021q4$geometry))
tiles_inside2 <- st_within(ookla_2021q4$geometry, shape$geometry)
ind_tiles_inside2 <- which( sapply(tiles_inside2,length) > 0 )
ookla_inside_post <- (ookla_2021q4[ind_tiles_inside2,])

shape$geometry <- st_transform(shape$geometry,st_crs(ookla_2021q4$geometry))
tiles_border2 <- st_intersects(ookla_2021q4$geometry, shape$geometry)
ind_tiles_border2 <- which( sapply(tiles_border2,length) > 0 )

ookla_border_post <- ookla_2021q4[ind_tiles_border2,]
tiles_border2 <- st_intersects(ookla_border_post$geometry, outer_shape$geometry)
ind_tiles_border2 <- which( sapply(tiles_border2,length) > 0 )
ookla_border_post <- ookla_border_initial[ind_tiles_border2,]

# plot
ggplot() +
  geom_sf(data = shape$geometry, col = "black") +
  geom_sf(data = ookla_outside_initial$geometry, col = "orange", fill=NA) +
  geom_sf(data = ookla_inside_initial$geometry, col = "lightgreen", fill=NA) +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA)




#EDA ookla_inside_initial ookla_outside_initial ookla_inside_post ookla_outside_post

# create EDA dataframe
EDA_full <- list()

dataframe_of_interest <- list()
dataframe_of_interest <- as.data.frame(ookla_outside_post)
dataframe_of_interest <- dataframe_of_interest[1:3]
EDA <- data.frame(matrix(ncol = 9, nrow = ncol(dataframe_of_interest)))
EDA_columns <- c("dataframe", "variable", "min", "first_quartile", "median",
                 "third_quartile", "max", "mean", "range")
colnames(EDA) <- EDA_columns
EDA$variable <- as.vector(column_names)
EDA[, 2] <- colnames(dataframe_of_interest)
EDA[, 1] <- "ookla_outside_post"

# remove scientific notation
options(scipen = 100, digits = 4)

# eda - loop through each column and then append to EDA dataframe
for(i in 1:ncol(dataframe_of_interest)) {       # for-loop over columns
  temporary_list <- list()
  temporary_list <- as.numeric(unlist(dataframe_of_interest[ , i]))
  EDA[i, 3] <- min(temporary_list, na.rm = TRUE) # minimum
  EDA[i, 5] <- median(temporary_list, na.rm = TRUE) # median
  EDA[i, 7] <- max(temporary_list, na.rm = TRUE) # maximum
  EDA[i, 8] <- mean(temporary_list, na.rm = TRUE) # mean
  EDA[i, 9] <- max(temporary_list, na.rm = TRUE)-min(temporary_list, na.rm = TRUE)
  
  #code for quartile 1 and 3 #from https://stats.stackexchange.com/questions/134229/finding-quartiles-in-r
  temporary_list <- sort(temporary_list, na.last = NA)
  length_tl <- length(temporary_list)
  middle <- (length_tl+1)/2
  if (floor(middle) != middle) {
    l <- middle-1/2; u <- middle+1/2
  } else {
    l <- middle-1; u <- middle+1
  }
  EDA[i, 4] <- median(temporary_list[1:l]) # Quartile 1
  EDA[i, 6] <- median(temporary_list[u:length_tl]) # Quartile 3
}

EDA_full <- rbind(EDA_full, EDA)



