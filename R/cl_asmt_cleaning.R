# Corelogic data cleaning: Example using Palm Beach, FL 

# packages 
library(DBI)
library(RPostgreSQL)
library(dplyr)

# establish connection to DB
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = Sys.getenv("db_pwd")) 

# all cols in CL asmt table
CL_assmt_col_names <- dbGetQuery(con,"SELECT column_name FROM information_schema.columns
                  WHERE table_schema = 'corelogic_usda'
                  AND table_name   = 'current_tax_200627_typed'")

# all cols in CL raw table
CL_raw_col_names <- dbGetQuery(con,"SELECT column_name FROM information_schema.columns
                  WHERE table_schema = 'corelogic_usda'
                  AND table_name   = 'current_tax_200627_raw'")


# Palm Beach, FL 12099 CL Asmt query
CL_asmt_PB <- dbGetQuery(con, "SELECT *
                        FROM corelogic_usda.current_tax_200627_typed
                        WHERE  property_indicator = '10'
                        AND (tax_year IS NOT NULL OR assessed_year IS NOT NULL)
                        AND total_value_calculated IS NOT NULL
                        AND (building_square_feet IS NOT NULL OR living_square_feet IS NOT NULL)
                        AND (acres IS NOT NULL OR land_square_footage IS NOT NULL)
                        AND (year_built IS NOT NULL OR effective_year_built IS NOT NULL)
                        AND (full_baths IS NOT NULL OR qtr_baths IS NOT NULL OR 
                        thrqtr_baths IS NOT NULL OR 
                        half_baths IS NOT NULL OR total_baths IS NOT NULL)
                        AND bedrooms IS NOT NULL
                        AND (LENGTH(situs_address) > 2 OR (property_centroid_latitude IS NOT NULL
                                                            AND property_centroid_longitude IS NOT NULL))
                        AND geoid_cnty = '12099'")


# extra asmt values
# CL_raw_PB <- dbGetQuery(con, "SELECT fips_code, p_id_iris_frmtd,
#                    assd_total_value, mkt_total_value, appr_total_value
#                    FROM corelogic_usda.current_tax_200627_raw
#                    WHERE fips_code = '12099'")

# disconnect from DB!
dbDisconnect(con)

#####################
# PRE-PROCESSING
#####################

# number of baths according to Fannie Mai/Freddie Mac
# Using the Fannie Mae and Freddie Mac Uniform Appraisal Dataset Specification to calculate 
# total bathrooms. 3/4 baths count as full baths, 1/4 baths are dropped, half bath is .1 of 
# full bath, so 1 x full bath & 1 x 3/4 bath & 1 x half bath = 2.1 baths (2 full baths, 1 
# half bath).
CL_asmt_PB["nbaths"] <- coalesce(CL_asmt_PB$full_baths, 0) + coalesce(CL_asmt_PB$thrqtr_baths, 0) +
  (.1 * coalesce(CL_asmt_PB$half_baths, 0)) 
CL_asmt_PB$nbaths[CL_asmt_PB$nbaths == 0] <- NA 

# age (replace with tax/assesed build/effective year if NAs)
CL_asmt_PB$assessed_year <- substr(CL_asmt_PB$assessed_year, 1,4)
CL_asmt_PB["age"] <- as.numeric(CL_asmt_PB$assessed_year)-as.numeric(CL_asmt_PB$effective_year_built)
CL_asmt_PB$age[is.na(CL_asmt_PB$age)==T] = as.numeric(CL_asmt_PB$assessed_year)-as.numeric(CL_asmt_PB$year_built)

# ratio of building area to living area
CL_asmt_PB["sqft.ratio"] <- as.numeric(CL_asmt_PB$building_square_feet)/as.numeric(CL_asmt_PB$living_square_feet)

# building code:
bldg_code_ext <- list("A0G","A0Z", "AB0", "AY0", "AYA", "AYG", "M00",
                      "M02", "M03", "M04", "M05", "M06", "M0A", "M0B", "M0M", "M0T",
                      "M40", "MA0", "MAA", "MAB", "MAC", "MAS", "MAT", "MC0", "MCA", "MCB",
                      "MCE", "MCM", "MCT", "MCV", "MD0", "MDC", "MDE", "MDF", "MRD", "MS0", "MST", "MT0",
                      "R00", "R0C", "R0F", "R0Q", "R10", "R20", "R30", "R40", "R60", "R80",
                      "RC0", "RCA", "RG0", "RM0", "RM1", "RM2", "RMB", "RQ0", "RS0", "RSF", "RT0", "RU0",
                      "RW0", "RY0", "X01", "X0M", "X0N", "X0Z", "XF0", "XH0", "XNM", "XRM", "Y1L",
                      "YCR", "YDA", "YOR", "YQ1", "YQB", "YQR", "YQS", "YRA", "YSA", "YSR")

#building_code <- list("RS0", "R00") # only resedential properties
CL_asmt_PB$bldg_code <- as.character(CL_asmt_PB$bldg_code)

# age non-negative
CL_asmt_PB <- CL_asmt_PB %>%
  filter(bldg_code %in% bldg_code_ext) %>%
  filter(age >= 0)

###########################
# Check validity of values
###########################

###########################
# Redefine types of vars
###########################

