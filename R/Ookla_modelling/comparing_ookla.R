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

projectID <- as.list(CCG_YR$RUSID)
Ookla_2017_CCG <-list()
for (n in 1:length(projectID)){
  CCG_project <- data.frame(matrix(ncol = 1, nrow = 1))
  CCG_project_columns <- c("RUSID")      
  colnames(CCG_project) <- CCG_project_columns
  CCG_project$RUSID <- c(projectID[[n]])
  CCG_row_of_interest <- CCG_YR[CCG_YR$RUSID==projectID[[n]], ]
  CCG_project$geometry <- CCG_row_of_interest[1, 20]
  
  # sf settings to avoid Loop 0 error
  sf_use_s2(FALSE)
  
  shape <- CCG_project[CCG_project$RUSID == projectID[[n]],] 
  
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
  
  for (i in 1:12){
    for (j in list(0, 5, 10, 15, 20)){
      if (j == 0){
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
          for (k in 1:length(ookla_border$geometry)){
            ookla_border$geometry[k] <- st_make_valid(ookla_border$geometry[k])
            intersection <- st_intersection(ookla_border$geometry[k], shape$geometry)
            intersection <- st_make_valid(intersection)
            weight <- st_area(intersection)/st_area(ookla_border$geometry[k])
            ookla_border$devices[k] <- weight*ookla_border$devices[k]}
          ookla_inside <- rbind(ookla_inside, ookla_border)}  
        downloads <- c()
        uploads <- c()
        latency <- c()
        devices <- 0
        if (length(ookla_inside$devices) > 0){
          for (k in 1:length(ookla_inside$devices)){
            devices <- devices + ookla_inside$devices[k]}
          mean_d <- weighted.mean(ookla_inside$avg_d_kbps, ookla_inside$devices)
          mean_u <- weighted.mean(ookla_inside$avg_u_kbps, ookla_inside$devices)
          mean_l <- weighted.mean(ookla_inside$avg_lat_ms, ookla_inside$devices)}
        if (length(ookla_inside$devices) > 1){
          stddev_d <- sqrt(wtd.var(ookla_inside$avg_d_kbps, ookla_inside$devices))
          stddev_u <- sqrt(wtd.var(ookla_inside$avg_d_kbps, ookla_inside$devices))
          stddev_l <- sqrt(wtd.var(ookla_inside$avg_lat_ms, ookla_inside$devices))}
        Ookla_EDA[i,] <- list(i, "inside", mean_d, stddev_d, 
                              mean_u, stddev_u, mean_l, stddev_l, devices)
      }else{
        shape$geometry <- st_transform(shape$geometry, 5070)
        outer_shape <- st_difference(st_buffer(shape$geometry, dist=1609.34*j), 
                                     st_buffer(shape$geometry,0))
        outer_shape$geometry <- st_transform(outer_shape$geometry,st_crs(ookla_1$geometry))
        tiles_outside <- st_within(eval(parse(text = paste0("ookla_",as.character(i))))$geometry, outer_shape$geometry)
        ind_tiles_outside <- which( sapply(tiles_outside,length) > 0 )
        ookla_outside <- eval(parse(text = paste0("ookla_",as.character(i))))[ind_tiles_outside,]
        shape$geometry <- st_transform(shape$geometry,st_crs(ookla_1$geometry))
        tiles_border <- st_intersects(eval(parse(text = paste0("ookla_", as.character(i))))$geometry, shape$geometry)
        ind_tiles_border <- which( sapply(tiles_border,length) > 0 )
        if (length(ind_tiles_border) > 0){
        ookla_border <-  eval(parse(text = paste0("ookla_", as.character(i))))[ind_tiles_border,]
        tiles_border <- st_intersects(ookla_border$geometry, outer_shape$geometry)
        ind_tiles_border <- which( sapply(tiles_border,length) > 0 )}
        if (length(ind_tiles_border) > 0){
          ookla_border <- ookla_border[ind_tiles_border,]
          shape$geometry <- st_make_valid(shape$geometry)
          for (k in 1:length(ookla_border$geometry)){
            ookla_border$geometry[k] <- st_make_valid(ookla_border$geometry[k])
            intersection <- st_intersection(ookla_border$geometry[k], outer_shape$geometry)
            intersection <- st_make_valid(intersection)
            weight <- st_area(intersection)/st_area(ookla_border$geometry[k])
            ookla_border$devices[k] <- weight*ookla_border$devices[k]}
          ookla_outside <- rbind(ookla_outside, ookla_border)}  
        downloads <- c()
        uploads <- c()
        latency <- c()
        devices <- 0
        if (length(ookla_outside$devices) > 0){
          for (k in 1:length(ookla_outside$devices)){
            devices <- devices + ookla_outside$devices[k]}
          mean_d <- weighted.mean(ookla_outside$avg_d_kbps, ookla_outside$devices)
          mean_u <- weighted.mean(ookla_outside$avg_u_kbps, ookla_outside$devices)
          mean_l <- weighted.mean(ookla_outside$avg_lat_ms, ookla_outside$devices)}
        if (length(ookla_outside$devices) > 1){
          stddev_d <- sqrt(wtd.var(ookla_outside$avg_d_kbps, ookla_outside$devices))
          stddev_u <- sqrt(wtd.var(ookla_outside$avg_d_kbps, ookla_outside$devices))
          stddev_l <- sqrt(wtd.var(ookla_outside$avg_lat_ms, ookla_outside$devices))}
        Ookla_EDA[i + (12*j)/5,] <- list(i, (paste0(as.character(j)," miles")), mean_d, stddev_d, 
                                         mean_u, stddev_u, mean_l, stddev_l, devices)}
      print(paste0("q", as.character(i), " mile ", as.character(j), " area ", as.character(n)))
    }}    
  ##Plotting
  Ookla_2017_CCG[[(4*(n-1) + 1)]] <- ggplot(data = Ookla_EDA, aes(quarter, devices, color = area)) +
    geom_point() +
    scale_color_manual(values = c("inside" = "blue", "5 miles" = "green", "10 miles" = "yellow", 
                                  "15 miles" = "orange", "20 miles" = "red")) +
    geom_smooth(method='lm', formula = y ~ x, se = F) +
    labs(title = paste0("Devices ", projectID[[n]]), y = "Devices", x = "quarter") +
    scale_x_continuous(breaks = seq(0, 12, by = 1))
  
  Ookla_EDA <- na.omit(Ookla_EDA)
  Ookla_2017_CCG[[(4*(n-1) + 2)]] <- ggplot(data = Ookla_EDA, aes(quarter, avg_d_kbps, color = area)) +
                                      geom_point() +
                                      scale_color_manual(values = c("inside" = "blue", "5 miles" = "green", "10 miles" = "yellow", 
                                                                    "15 miles" = "orange", "20 miles" = "red")) +
                                      geom_smooth(method='lm', formula = y ~ x) +
                                      labs(title = paste0("Download Speed ", projectID[[n]]), y = "kilobits per second", x = "quarter") +
                                      scale_x_continuous(breaks = seq(0, 12, by = 1))
    
  Ookla_2017_CCG[[(4*(n-1) + 3)]] <- ggplot(data = Ookla_EDA, aes(quarter, avg_u_kbps, color = area)) +
                                      geom_point() +
                                      scale_color_manual(values = c("inside" = "blue", "5 miles" = "green", "10 miles" = "yellow", 
                                                                    "15 miles" = "orange", "20 miles" = "red")) +
                                      geom_smooth(method='lm', formula = y ~ x) +
                                      labs(title = paste0("Upload Speed ", projectID[[n]]), y = "kilobits per second", x = "quarter") +
                                      scale_x_continuous(breaks = seq(0, 12, by = 1))
  
  Ookla_2017_CCG[[(4*(n-1) + 4)]] <- ggplot(data = Ookla_EDA, aes(quarter, -avg_lat_ms, color = area)) +
                                      geom_point() +
                                      scale_color_manual(values = c("inside" = "blue", "5 miles" = "green", "10 miles" = "yellow", 
                                                                    "15 miles" = "orange", "20 miles" = "red")) +
                                      geom_smooth(method='lm', formula = y ~ x) +
                                      labs(title = paste0("Negative Latency ", projectID[[n]]), y = "milliseconds", x = "quarter") +
                                      scale_x_continuous(breaks = seq(0, 12, by = 1))}
##########
#ookla_1_area <- ookla_1
#ookla_1_area$area <- st_area(ookla_1_area$geometry)
#ookla_1_area <- ookla_1_area[order(ookla_1_area$area),]
#ookla_1_area$geometry <- st_transform(ookla_1_area$geometry, 4326)
#ookla_1_area$tile_centroid_26918 <- st_transform(ookla_1_area$tile_centroid_26918, 4326)
#ookla_1_area$centroidc <- as.character(ookla_1_area$tile_centroid_26918)
#for (i in 1:length(ookla_1_area$centroidc)){
#ookla_1_area$lat[i] <- eval(parse(text = ookla_1_area$centroidc[i]))[2]}
#for (i in 1:length(ookla_1_area$centroidc)){
#ookla_1_area$mercator_area[i] <- (ookla_1_area$area[i])/(cos((3.1416/180)*(ookla_1_area$lat[i]))**2)}
#########
#geom_errorbar(aes(ymin=avg_d_kbps - d_kbps_std_dev, ymax=avg_d_kbps + d_kbps_std_dev), width=.2,
#position=position_dodge(.9)) 
#geom_errorbar(aes(ymin=avg_u_kbps - u_kbps_std_dev, ymax=avg_u_kbps + u_kbps_std_dev), width=.2,
#position=position_dodge(.9)) 
#geom_errorbar(aes(ymin=avg_lat_ms - lat_ms_std_dev, ymax=avg_lat_ms + lat_ms_std_dev), width=.2,
#position=position_dodge(.9))} 