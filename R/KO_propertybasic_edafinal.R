#######################################################################################
# Kristian Olsson - ERS data EDA final
#######################################################################################

#######################################################################################
# packages
library(readxl)
library(sf)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
options(tigris_use_cache = TRUE)

# set a working directory
setwd("~/git/dspg22-ers/R")
path <- "~/../../../../project/biocomplexity/sdad/projects_data/usda/dspg_2022/"

#######################################################################################
#Exploratory Data Analysis
#######################################################################################

con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = "kno5cac") 

# gather data
query_columns = paste0("SELECT *
                       FROM
                       corelogic_usda.combined_propertybasic_limited
                       WHERE 1 = 0")

column_names <- as.list(names(dbGetQuery(con, query_columns)))

column_names[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 18, 19, 20, 21, 22,
               23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 48,
               49, 63, 64, 65, 66)] = NULL

query_datalist = paste0("SELECT ", column_names, "
                     FROM
                     corelogic_usda.combined_propertybasic_limited
                     WHERE property_indicator_code = '10'")

query_datalist2 = paste0("SELECT sale_amount
                       FROM
                       corelogic_usda.combined_propertybasic_full
                       WHERE property_indicator_code = '10'
                       ")

datalist <- dbGetQuery(con, query_datalist)
datalist2 <- dbGetQuery(con, query_datalist2)
datalist$sale_value <- datalist2

# create EDA dataframe
EDA <- data.frame(matrix(ncol = 10, nrow = 0))
EDA_columns <- c("variable", "min", "first_quartile", "median",
                 "third_quartile", "max", "mean", "range", "# of NAs", "completeness")
colnames(EDA) <- EDA_columns
EDA[nrow(EDA) + 33,] <- NA
column_names <- append(column_names, "sale_value")
EDA$variable <- as.vector(column_names)

# remove scientific notation
options(scipen = 100, digits = 4)

# eda - loop through each column and then append to EDA dataframe
for(i in 1:ncol(datalist)) {       # for-loop over columns
  temporary_list = list()
  temporary_list <- as.numeric(unlist(datalist[ , i]))
  EDA[i, 2] <- min(temporary_list, na.rm = TRUE) # minimum
  EDA[i, 4] <- median(temporary_list, na.rm = TRUE) # median
  EDA[i, 6] <- max(temporary_list, na.rm = TRUE) # maximum
  EDA[i, 7] <- mean(temporary_list, na.rm = TRUE) # mean
  EDA[i, 8] <- max(temporary_list, na.rm = TRUE)-min(temporary_list, na.rm = TRUE)
  EDA[i, 9] <- sum(is.na(temporary_list)) # number of NA
  EDA[i, 10] <- (1-(sum(is.na(temporary_list))/nrow(datalist))) # percent completeness
  
  
  #code for quartile 1 and 3 #from https://stats.stackexchange.com/questions/134229/finding-quartiles-in-r
  temporary_list <- sort(temporary_list, , na.last = NA)
  length_tl <- length(temporary_list)
  middle <- (length_tl+1)/2
  if (floor(middle) != middle) {
    l <- middle-1/2; u <- middle+1/2
  } else {
    l <- middle-1; u <- middle+1
  }
  EDA[i, 3] <- median(temporary_list[1:l]) # Quartile 1
  EDA[i, 5] <- median(temporary_list[u:length_tl]) # Quartile 3
}

#save EDA datatable
dbWriteTable(con,
             SQL('propertybasic_limited_eda'),
             EDA
)

dbDisconnect(con)

#######################################################################################
# Understanding Data and Cleaning
#######################################################################################

count()
























