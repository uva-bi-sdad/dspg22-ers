# pull Ookla tile data 2019-2021 from database

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
ookla_2019q2 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '2'")
ookla_2019q3 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '3'")
ookla_2019q4 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2019' AND quarter = '4'")
ookla_2020q1 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '1'")
ookla_2020q2 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '2'")
ookla_2020q3 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '3'")
ookla_2020q4 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2020' AND quarter = '4'")
ookla_2021q1 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '1'")
ookla_2021q2 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '2'")
ookla_2021q3 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '3'")
ookla_2021q4 <- sf::st_read(dsn=con,query="SELECT * FROM dc_working.us_tile_ookla_2019_2021 WHERE year = '2021' AND quarter = '4'")
DBI::dbDisconnect(con)


setwd("~/git/rural_broadband/ookla")



# Example: intersect ookla_us Q1 2019 with Round 1 ReConnect shapefiles
round1 <- st_read("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/ReConnect_984_PFSAs/","USDARD_PFSA_ReConnect",
                  int64_as_string = TRUE)
#round2 <- st_read("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/ReConnect_Round_2_Final/","ReConnect_Round_2_Final",
#                  int64_as_string = TRUE)

round1 <- st_transform(round1,st_crs(ookla_2019q1))

intersections <- st_intersects(round1,ookla_2019q1)
# to get area of intersection, use the function st_intersection()

# Question: What is the coverage of Ookla in the ReConnect project areas?
# how many ookla tiles (Q1 2019) fall within each project?
table( sapply(intersections,length) )

# 610 PFSAs have at least one tile with speedtests (out of 984 in reconnect Round 1)
sum( sapply(intersections,length) >= 1 )

# 74 PFSAs have 20+ tiles with speedtests
sum( sapply(intersections,length) >= 20 )

# 23 PFSAs have 50+ tiles with speedtests
sum( sapply(intersections,length) >= 50 )

