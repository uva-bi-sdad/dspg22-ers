projectYR <- "2017"
# pull Ookla tile data 2019-2021 from database
library(readxl)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)
library(sf)
library(RPostgreSQL)
library(Hmisc)
library(tidycensus)
library(viridis)
library(rgdal)
sf_use_s2(FALSE)
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
con <- get_db_conn()
ookla_1 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '1'")
ookla_2 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '2'")
ookla_3 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '3'")
ookla_4 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '4'")
ookla_5 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '1'")
ookla_6 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '2'")
ookla_7 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '3'")
ookla_8 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '4'")
ookla_9 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '1'")
ookla_10 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '2'")
ookla_11 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '3'")
ookla_12 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '4'")
DBI::dbDisconnect(con)

setwd("~/git/rural_broadband/src")
path <- "~/../../../../project/biocomplexity/sdad/projects_data/usda/dspg_2022/"

CCG_2013_20 <- st_read(paste0(path, "RUS_CC_Shapefiles_Nov2021/CC_2013-2019-83_09042020/"),
                       "CC 2013_2019_83 09042020", int64_as_string = TRUE)
CCG_YR <- CCG_2013_20[CCG_2013_20$OBLFY == projectYR,]

projectID <- "WV1406-A23"
Ookla_2017_CCG <-list()
CCG_project <- data.frame(matrix(ncol = 1, nrow = 1))
CCG_project_columns <- c("RUSID")      
colnames(CCG_project) <- CCG_project_columns
CCG_project$RUSID <- c(projectID)
CCG_row_of_interest <- CCG_YR[CCG_YR$RUSID==projectID, ]
CCG_project$geometry <- CCG_row_of_interest[1, 20]
  
# sf settings to avoid Loop 0 error
sf_use_s2(FALSE)
  
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
###
Ookla_EDA <- data.frame(matrix(nrow = 60, ncol = 9))
colnames(Ookla_EDA) <- c("quarter", "area", "avg_d_kbps", "d_kbps_std_dev", 
                         "avg_u_kbps", "u_kbps_std_dev", "avg_lat_ms", "lat_ms_std_dev", "devices")

WV_tracts <-  st_read("~/git/dspg22-ers/R/Shapefile_WV_2015/",
                      "tl_2015_54_tract", int64_as_string = TRUE)
WV_tracts$geometry <- st_transform(WV_tracts$geometry,st_crs(shape$geometry))
VA_tracts <- st_read("~/git/dspg22-ers/R/Shapefile_VA_2015/",
                     "tl_2015_51_tract", int64_as_string = TRUE)
VA_tracts$geometry <- st_transform(VA_tracts$geometry,st_crs(shape$geometry))
combined_tracts <- rbind(WV_tracts, VA_tracts)

sf_use_s2(F)

tiles_WV <- st_intersects(WV_tracts$geometry, shape$geometry)
ind_tiles_WV <- which( sapply(tiles_WV,length) > 0 )
WV_tracts_intersect <-WV_tracts[ind_tiles_WV,]

combined_tracts <- st_transform(combined_tracts, st_crs(outer_shape))

outer_tracts <- st_intersects(combined_tracts$geometry, outer_shape$geometry)
ind_outer <- which( sapply(outer_tracts,length) > 0 )
outer_intersect <- combined_tracts[ind_outer,]

ggplot() + 
  geom_sf(data = outer_shape$geometry, col = "black") +
  geom_sf(data = shape$geometry, col = "blue") +
  geom_sf(data = outer_intersect$geometry, col = " dark green", fill=NA) +
  geom_sf(data = WV_tracts_intersect$geometry, col = "yellow", fill=NA) +
  geom_sf(data = ookla_outside$geometry, col = "orange", fill=NA) +
  geom_sf(data = ookla_inside$geometry, col = "lightgreen", fill=NA) +
  geom_sf(data = ookla_border$geometry, col = "red", fill=NA)

Project_Union <- st_union(shape$geometry, outer_shape$geometry)
WV_counties <- counties("West Virginia", cb = TRUE)
VA_counties <- counties("Virginia", cb = TRUE)
counties <- rbind(WV_counties, VA_counties)
counties$geometry <- st_transform(counties$geometry,st_crs(outer_shape$geometry))



tiles_C <- st_intersects(counties$geometry, Project_Union$geometry)
ind_tiles_C <- which( sapply(tiles_C,length) > 0 )
counties_intersect <-counties[ind_tiles_C,]

ggplot() + 
  geom_sf(data = Project_Union$geometry, col = "black") +
  geom_sf(data = counties_intersect$geometry, col = "yellow", fill=NA) +
  geom_sf(data = ookla_outside$geometry, col = "orange", fill=NA) +
  geom_sf(data = ookla_inside$geometry, col = "lightgreen", fill=NA) +
  geom_sf(data = ookla_border$geometry, col = "red", fill=NA)

variables <- read.csv(paste0("/project/biocomplexity/sdad/projects_data/usda/dspg_2022/interns_0622/bip_data/",
                       "broadband_tract_eligibility12-15-20.csv"))

filtered_variables <- variables %>% 
  filter(GEOID %in% outer_intersect$GEOID)

for (i in 1:56){
  if (filtered_variables$BIP_IN[i] == FALSE){
    filtered_variables$BIP_IN[i] <- 0
  }else{
    filtered_variables$BIP_IN[i] <- 1}}

write.csv(filtered_variables,"~/git/dspg22-ers/R/filtered_variables.csv", row.names = FALSE)

variables_for_regression <- filtered_variables[,-c(5:12, 14:55, 71:100, 102:107)]

variables_for_regression$GEOID <- as.character(variables_for_regression$GEOID)
variables_for_regression$COUNTYFP <- as.character(variables_for_regression$COUNTYFP)
variables_for_regression$STATEFP <- as.character(variables_for_regression$STATEFP)
variables_for_regression$TRACTCE <- as.character(variables_for_regression$TRACTCE)
joined_variables <- right_join(variables_for_regression, outer_intersect, by="GEOID")
joined_variables <- joined_variables[,-c(1, 2, 3, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)]
joined_variables <- na.omit(joined_variables)


ookla_WV1406_A23 <- data.frame(matrix(nrow = 0, ncol = 10))
for (i in 1:12){
  shape$geometry <- st_transform(shape$geometry, 5070)
  outer_shape <- st_difference(st_buffer(shape$geometry, dist=1609.34*1), 
                               st_buffer(shape$geometry,0))
  outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(ookla_1$geometry))
  shape$geometry <- st_transform(shape$geometry,st_crs(ookla_1$geometry))
  tiles_inside <- st_within(eval(parse(text = paste0("ookla_",as.character(i))))$geometry, shape$geometry)
  ind_tiles_inside <- which( sapply(tiles_inside,length) > 0 )
  ookla_inside <- eval(parse(text = paste0("ookla_",as.character(i))))[ind_tiles_inside,]
  tiles_border <- st_intersects(eval(parse(text = paste0("ookla_", as.character(i))))$geometry, shape$geometry)
  ind_tiles_border <- which( sapply(tiles_border,length) > 0 )
  if (length(ind_tiles_border) > 0){
    ookla_border <-  eval(parse(text = paste0("ookla_", as.character(i))))[ind_tiles_border,]
    tiles_border <- st_intersects(ookla_border$geometry, outer_shape$geometry)
    ind_tiles_border <- which( sapply(tiles_border,length) > 0 )}
  if (length(ind_tiles_border) > 0){
    ookla_border <- ookla_border[ind_tiles_border,]
    shape$geometry <- st_make_valid(shape$geometry)
    ookla_border$inside <- NA
    for (k in 1:length(ookla_border$geometry)){
      ookla_border$geometry[k] <- st_make_valid(ookla_border$geometry[k])
      intersection <- st_intersection(ookla_border$geometry[k], shape$geometry)
      intersection <- st_make_valid(intersection)
      weight <- st_area(intersection)/st_area(ookla_border$geometry[k])
      ookla_border$inside[k] <- weight}}
  shape$geometry <- st_transform(shape$geometry, 5070)
  outer_shape <- st_difference(st_buffer(shape$geometry, dist=1609.34*5), 
                               st_buffer(shape$geometry,0))
  outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(ookla_1$geometry))
  tiles_outside <- st_within(eval(parse(text = paste0("ookla_",as.character(i))))$geometry, outer_shape$geometry)
  ind_tiles_outside <- which( sapply(tiles_outside,length) > 0 )
  ookla_outside <- eval(parse(text = paste0("ookla_",as.character(i))))[ind_tiles_outside,]
  shape$geometry <- st_transform(shape$geometry,st_crs(ookla_1$geometry))
  ookla_inside$inside <- 1
  ookla_outside$inside <- 0
  print(paste0("print q", as.character(i), " 1"))
  ookla_quarter <- rbind(ookla_inside, ookla_outside, ookla_border)
  print(paste0("print q", as.character(i)," 2"))
  ookla_WV1406_A23 <- rbind(ookla_WV1406_A23, ookla_quarter)
  print(paste0("print q", as.character(i), " 3"))}  

ookla_WV1406_A23 <- ookla_WV1406_A23[,-c(4, 10)]

Project_Union <- st_union(shape$geometry, outer_shape$geometry)
WV_tracts <- st_read("~/git/dspg22-ers/R/Shapefile_WV_2015/",
                     "tl_2015_54_tract", int64_as_string = TRUE)
WV_tracts$geometry <- st_transform(WV_tracts$geometry,st_crs(shape$geometry))
tiles_WV <- st_intersects(WV_tracts$geometry, Project_Union$geometry)
ind_tiles_WV <- which( sapply(tiles_WV,length) > 0 )
WV_tracts_intersect <-WV_tracts[ind_tiles_WV,]

ggplot() + 
  geom_sf(data = Project_Union$geometry, col = "black") +
  geom_sf(data = WV_tracts_intersect$geometry, col = "green", fill=NA)

ookla_WV1406_A23$GEOID <- NA
for (i in 1:length(ookla_WV1406_A23$avg_d_kbps)){
  tract_weight_list <- list()
  for (j in 1:length(WV_tracts_intersect$GEOID)){
    intersection <- st_intersection(WV_tracts_intersect$geometry[j], 
                                    ookla_WV1406_A23$geometry[i])
    weight <- st_area(intersection)/st_area(ookla_WV1406_A23$geometry[i])
    tract_weight_list[[j]] <- weight}
  ookla_WV1406_A23$GEOID[i] <- WV_tracts_intersect$GEOID[which.max(tract_weight_list)]}

st_write(ookla_WV1406_A23, dsn = "~/git/dspg22-ers/R/", layer = "ookla_WV1406_A23_save", 
         driver = "ESRI Shapefile") 

ookla_WV1406_A23_save <-st_read("~/git/dspg22-ers/R/",
                                "ookla_WV1406_A23_save", int64_as_string = TRUE) 

tracts_dataframe <- as.data.frame(joined_variables)
ookla_WV1406_A23 <- rename(ookla_WV1406_A23, GEOID = tract)
tracts_dataframe <- tracts_dataframe[,-c(14, 15, 18)]
print(colnames(tracts_dataframe))

tract_tile_merge <- left_join(ookla_WV1406_A23, tracts_dataframe, by = "GEOID")
tract_tile_merge$q <- (tract_tile_merge$year - 2019) * 4 + tract_tile_merge$quarter
tract_tile_merge <- tract_tile_merge[,-c(5, 6)]
tract_tile_merge$factor_RUCC_6 <- NA
tract_tile_merge$factor_RUCC_7 <- NA
tract_tile_merge$factor_RUCC_8 <- NA
tract_tile_merge$factor_RUCC_9 <- NA
for (i in 1:length(tract_tile_merge$avg_d_kbps)){
  if (tract_tile_merge$RUCC_2013[i] == 6){
    tract_tile_merge$factor_RUCC_6[i] <- 1
    tract_tile_merge$factor_RUCC_7[i] <- 0
    tract_tile_merge$factor_RUCC_8[i] <- 0
    tract_tile_merge$factor_RUCC_9[i] <- 0
  }
  if (tract_tile_merge$RUCC_2013[i] == 7){
    tract_tile_merge$factor_RUCC_6[i] <- 0
    tract_tile_merge$factor_RUCC_7[i] <- 1
    tract_tile_merge$factor_RUCC_8[i] <- 0
    tract_tile_merge$factor_RUCC_9[i] <- 0
  }
  if (tract_tile_merge$RUCC_2013[i] == 8){
    tract_tile_merge$factor_RUCC_6[i] <- 0
    tract_tile_merge$factor_RUCC_7[i] <- 0
    tract_tile_merge$factor_RUCC_8[i] <- 1
    tract_tile_merge$factor_RUCC_9[i] <- 0
  }
  if (tract_tile_merge$RUCC_2013[i] == 9){
    tract_tile_merge$factor_RUCC_6[i] <- 0
    tract_tile_merge$factor_RUCC_7[i] <- 0
    tract_tile_merge$factor_RUCC_8[i] <- 0
    tract_tile_merge$factor_RUCC_9[i] <- 1
  }
}
tract_tile_merge <- tract_tile_merge[,-c(7)]
tract_tile_merge <- tract_tile_merge[,-c(7, 8)]
tract_tile_merge$time_inside_relation <- tract_tile_merge$q* tract_tile_merge$inside
model_d <- lm(data = tract_tile_merge, avg_d_kbps ~ inside + hs_or_less_2015 + 
              renters_2015 + poverty_2015 + age_65_older_2015 + hispanic_2015 +
              black_2015 + family_2015 + foreign_2015 + unemployment_2015 + median_income_adj_2015 +
              median_value_adj_2015 + q + time_inside_relation + factor_RUCC_6 + factor_RUCC_7 +
              factor_RUCC_8 + factor_RUCC_9, weights=devices)
summary(model_d)
step_model_d <- step(model_d)
summary(step_model_d)

model_u <- lm(data = tract_tile_merge, avg_u_kbps ~ inside + hs_or_less_2015 + 
                renters_2015 + poverty_2015 + age_65_older_2015 + hispanic_2015 +
                black_2015 + family_2015 + foreign_2015 + unemployment_2015 + median_income_adj_2015 +
                median_value_adj_2015 + q + time_inside_relation + factor_RUCC_6 + factor_RUCC_7 +
                factor_RUCC_8 + factor_RUCC_9, weights=devices)
summary(model_u)
step_model_u <- step(model_u)
summary(step_model_u)

model_l <- lm(data = tract_tile_merge, avg_lat_ms ~ inside + hs_or_less_2015 + 
                renters_2015 + poverty_2015 + age_65_older_2015 + hispanic_2015 +
                black_2015 + family_2015 + foreign_2015 + unemployment_2015 + median_income_adj_2015 +
                median_value_adj_2015 + q + time_inside_relation + factor_RUCC_6 + factor_RUCC_7 +
                factor_RUCC_8 + factor_RUCC_9, weights=devices)
summary(model_l)
step_model_l <- step(model_l)
summary(step_model_l)
