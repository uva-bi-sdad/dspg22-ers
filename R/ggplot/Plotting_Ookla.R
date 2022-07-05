projectID <- "NC1401-A23"
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

######2019Q1######
outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(ookla_2019q1$geometry))
tiles_outside1 <- st_within(ookla_2019q1$geometry, outer_shape$geometry)
ind_tiles_outside1 <- which( sapply(tiles_outside1,length) > 0 )
ookla_outside_initial <- ookla_2019q1[ind_tiles_outside1,]

shape$geometry <- st_transform(shape$geometry,st_crs(ookla_2019q1$geometry))
tiles_inside1 <- st_within(ookla_2019q1$geometry, shape$geometry)
ind_tiles_inside1 <- which( sapply(tiles_inside1,length) > 0 )
ookla_inside_initial <- ookla_2019q1[ind_tiles_inside1,]

shape$geometry <- st_transform(shape$geometry,st_crs(ookla_2019q1$geometry))
tiles_border1 <- st_intersects(ookla_2019q1$geometry, shape$geometry)
ind_tiles_border1 <- which( sapply(tiles_border1,length) > 0 )
ookla_border_initial <- ookla_2019q1[ind_tiles_border1,]
tiles_border1 <- st_intersects(ookla_border_initial$geometry, outer_shape$geometry)
ind_tiles_border1 <- which( sapply(tiles_border1,length) > 0 )
ookla_border_initial <- ookla_border_initial[ind_tiles_border1,]

###Defining these areas makes it easier to differentiate tiles on borders for plotting###
outer_shape$geometry <- st_transform(outer_shape$geometry, 5070)
area_of_disinterest1 <- st_difference(st_buffer(outer_shape$geometry, dist=1609.34), 
                             st_buffer(outer_shape$geometry,0) )
area_of_disinterest1 <- as.data.frame(area_of_disinterest1)
area_of_disinterest1$geometry <- st_transform(area_of_disinterest1$geometry,st_crs(ookla_2019q1))
shape$geometry <- st_transform(shape$geometry, 5070)
area_of_disinterest2 <- st_difference(st_buffer(shape$geometry, dist=1609.34*25), 
                                      st_buffer(shape$geometry,1609.34) )
area_of_disinterest2 <- as.data.frame(area_of_disinterest2)
area_of_disinterest2$geometry <- st_transform(area_of_disinterest2$geometry,st_crs(ookla_2019q1))
######

outer_shape$geometry <- st_transform(outer_shape$geometry, st_crs(ookla_2019q1))
tiles_outerborder1 <- st_intersects(ookla_2019q1$geometry, outer_shape$geometry)
ind_tiles_outerborder1 <- which( sapply(tiles_outerborder1,length) > 0 )
ookla_outerborder_initial <- ookla_2019q1[ind_tiles_outerborder1,]
tiles_outerborder1 <- st_intersects(ookla_outerborder_initial$geometry, area_of_disinterest1$geometry)
ind_tiles_outerborder1 <- which( sapply(tiles_outerborder1,length) > 0 )
ookla_outerborder_initial <- ookla_outerborder_initial[ind_tiles_outerborder1,]
tiles_outerborder1 <- st_intersects(ookla_outerborder_initial$geometry, area_of_disinterest2$geometry)
ind_tiles_outerborder1 <- which( sapply(tiles_outerborder1,length) > 0 )
ookla_outerborder_initial <- ookla_outerborder_initial[ind_tiles_outerborder1,]

######2021Q4######
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
ookla_border_post <- ookla_border_post[ind_tiles_border2,]

###Defining these areas makes it easier to differentiate tiles on borders for plotting###
outer_shape$geometry <- st_transform(outer_shape$geometry, 5070)
area_of_disinterest1 <- st_difference(st_buffer(outer_shape$geometry, dist=1609.34), 
                                      st_buffer(outer_shape$geometry,0) )
area_of_disinterest1 <- as.data.frame(area_of_disinterest1)
area_of_disinterest1$geometry <- st_transform(area_of_disinterest1$geometry,st_crs(ookla_2021q4))
shape$geometry <- st_transform(shape$geometry, 5070)
area_of_disinterest2 <- st_difference(st_buffer(shape$geometry, dist=1609.34*25), 
                                      st_buffer(shape$geometry,1609.34) )
area_of_disinterest2 <- as.data.frame(area_of_disinterest2)
area_of_disinterest2$geometry <- st_transform(area_of_disinterest2$geometry,st_crs(ookla_2021q4))
######

outer_shape$geometry <- st_transform(outer_shape$geometry, st_crs(ookla_2021q4))
tiles_outerborder2 <- st_intersects(ookla_2021q4$geometry, outer_shape$geometry)
ind_tiles_outerborder2 <- which( sapply(tiles_outerborder2,length) > 0 )
ookla_outerborder_post <- ookla_2021q4[ind_tiles_outerborder2,]
tiles_outerborder2 <- st_intersects(ookla_outerborder_post$geometry, area_of_disinterest1$geometry)
ind_tiles_outerborder2 <- which( sapply(tiles_outerborder2,length) > 0 )
ookla_outerborder_post <- ookla_outerborder_post[ind_tiles_outerborder2,]
tiles_outerborder2 <- st_intersects(ookla_outerborder_post$geometry, area_of_disinterest2$geometry)
ind_tiles_outerborder2 <- which( sapply(tiles_outerborder2,length) > 0 )
ookla_outerborder_post <- ookla_outerborder_post[ind_tiles_outerborder2,]

###PLOTTING###
#plot 
state_fips <- as.data.frame(state_fips)
ggplot() + geom_sf(data = states$geometry[states$GEOID %in% state_fips$state_fips], fill = state_fips$state_fips) +
  geom_sf(data = shape$geometry, col = "white") + 
  geom_sf(data = ookla_outside_initial$geometry, col = "orange", fill=NA) +
  geom_sf(data = ookla_inside_initial$geometry, col = "lightgreen", fill=NA) +
  geom_sf(data = ookla_border_initial$geometry, col = "red", fill=NA) +
  geom_sf(data = ookla_outerborder_initial$geometry, col = "purple", fill=NA) +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA)

ggplot() + geom_sf(data = states$geometry[states$GEOID %in% state_fips$state_fips], fill = state_fips$state_fips) +
  geom_sf(data = shape$geometry, col = "white") + 
  geom_sf(data = ookla_outside_post$geometry, col = "orange", fill=NA) +
  geom_sf(data = ookla_inside_post$geometry, col = "lightgreen", fill=NA) +
  geom_sf(data = ookla_border_post$geometry, col = "red", fill=NA) +
  geom_sf(data = ookla_outerborder_post$geometry, col = "purple", fill=NA) +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA)

#plot without state boundaries
ggplot() + 
  geom_sf(data = shape$geometry, col = "white") + 
  geom_sf(data = ookla_outside_initial$geometry, col = "orange", fill=NA) +
  geom_sf(data = ookla_inside_initial$geometry, col = "lightgreen", fill=NA) +
  geom_sf(data = ookla_border_initial$geometry, col = "red", fill=NA) +
  geom_sf(data = ookla_outerborder_initial$geometry, col = "purple", fill=NA) +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA)

ggplot() + 
  geom_sf(data = shape$geometry, col = "white") + 
  geom_sf(data = ookla_outside_post$geometry, col = "orange", fill=NA) +
  geom_sf(data = ookla_inside_post$geometry, col = "lightgreen", fill=NA) +
  geom_sf(data = ookla_border_post$geometry, col = "red", fill=NA) +
  geom_sf(data = ookla_outerborder_post$geometry, col = "purple", fill=NA) +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA)


