#  Linear regression analysis efefcts of BIP on property sale prices

# packages
library(data.table)
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(haven)
library(Matching)
library(tibble)
library(purrr)

# set working directory
setwd("~/git/dspg22-ers/R")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

### !!!!!!! ##
# this a working dataset of housing prices/ you should have a similar one but for assessed values 
dataset <- readRDS(paste0(path, "BIP_linear_models/housing_BIP_060622.RDS"))

#########################################
# COVARIATES MATRIX (You may have a different set of independent variables but for year and inside/outside dummies )
#########################################

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(dataset$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              dataset$bip,
              dataset$age, # age of the house
              dataset$nbaths, # number of baths
              dataset$sqft_ratio, # living square ft/building square ft.
              log(dataset$living_square_feet), # living sq ft 
              log(dataset$land_square_footage), # land suqare footage
              dataset$bedrooms, # number of bedrooms
              dataset$ftth, # dummy =1 of BIP provides FFTH technology
              dataset$wireless) # dummy =1 of BIP provides wireless technology
cnames <- c() 

levels.x <- names(table(year.class, useNA = "ifany"))
id.yr = length(levels.x)
flag.in = 0 %in% levels.x

# year dummies
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <- cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(year.class)==x)
  })) 
  cnames <- c(cnames,paste("year.cl:",levels.x[-1],sep=""))
}

# transaction type dummies
levels.x <- names(table(as.character(dataset$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(dataset$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(dataset$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies: <--- main interest

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}


# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", "living_area_ft", "bedrooms",
                    "ftth", "wireless", cnames)


####################################
# REGRESSION MODELS
####################################

####################################
# LINEAR MODEL
####################################

dist_bip <- dataset$dist_bip

# filters for samples of properties ouside program area <- this may not be necessary, do all 10 or 20mi first
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) ) # 0-10mi
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) ) # 5-15mi
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) ) #10-20

get_lmresults <- function(filter){
  dataset_f <- dataset[filter,]; Xmat_f <- Xmat[filter,]
  # pooled regression with technology and award quantiles, no acs, no zoning and no bldg code, no prog ids, no census tract dummies
  model = try(lm(log(dataset_f$sale_price) ~ -1 + Xmat_f))
  model_summ = coef(summary(model))
  rownames(model_summ) = sapply(rownames(model_summ), function(k) substr(k, start = 5, stop = nchar(k)))
  model_summ <- cbind(" "=rownames(model_summ), model_summ)
  model_summ <- cbind(model_summ, confint(model, level = 0.90))
  return(model_summ)  
}
lm_results <- list()
lm_results[[1]] <- get_lmresults(close_filter)
lm_results[[2]] <- get_lmresults(med_filter)
lm_results[[3]] <- get_lmresults(far_filter)

saveRDS(lm_results,file="lm_results.RDS")

####################################
# LINEAR MODEL w/ fixed tract effects
####################################

# census tract geoids
dataset$geoid_tr <- substr(dataset$geoid_blk,1,11)

# length(unique(dataset$geoid_tr))
# 7011 tracts

# restrict to tracts that have at least 10 sales inside and outside the BIP <- this may not be necessary if you son't have many tracts
dataset_tract <- dataset %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(dataset$geoid_tr %in% tracts_include)
dataset2 <- dataset[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

# linear fixed effects model
get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
  model_summ_tr = coef(summary(model_tract))
  rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
  model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)
  confint <- as.data.frame(confint(model_tract, level = 0.90))
  model_summ_tr <- as.data.frame(model_summ_tr)
  model_tr_res <- list(model_summ_tr, confint) %>% 
    map(~ .x %>% 
          as.data.frame %>%
          rownames_to_column('rn')) %>% 
    reduce(left_join, by = 'rn') %>%
    column_to_rownames('rn')
  return(model_tr_res)
}
lm_fixedresults <- list()
lm_fixedresults[[1]] <- get_lmfixedresults(close_filter)
lm_fixedresults[[2]] <- get_lmfixedresults(med_filter)
lm_fixedresults[[3]] <- get_lmfixedresults(far_filter)

saveRDS(lm_fixedresults,file="lm_fixedresults.RDS")

#################################
# PLOT OF ESTIMATED COEFFICIENTS
#################################

# load in files: linear model, linear model w/ fixed tract effects
lm_fit <- readRDS("lm_results.RDS")
lmfixed_fit <- readRDS("lm_fixedresults.RDS")

yearnames <- c("07-08","09-10","11-12","13-14","15+")

lm_10 <- as.data.frame( lm_fit[[1]][19:23,c(2,6,7)] )
rownames(lm_10) <- yearnames
names(lm_10) <- c("est","lower","upper")
lm_10$years <- yearnames; lm_10$source <- "Linear Model"
lm_15 <- as.data.frame( lm_fit[[2]][19:23,c(2,6,7)] )
rownames(lm_15) <- yearnames
names(lm_15) <- c("est","lower","upper")
lm_15$years <- yearnames; lm_15$source <- "Linear Model"
lm_20 <- as.data.frame( lm_fit[[3]][19:23,c(2,6,7)] )
rownames(lm_20) <- yearnames
names(lm_20) <- c("est","lower","upper")
lm_20$years <- yearnames; lm_20$source <- "Linear Model"

lmfixed_10 <- as.data.frame( lm_fit[[1]][19:23,c(2,6,7)] )
rownames(lm_10) <- yearnames
names(lm_10) <- c("est","lower","upper")
lm_10$years <- yearnames; lm_10$source <- "Linear Model"
lm_15 <- as.data.frame( lm_fit[[2]][19:23,c(2,6,7)] )
rownames(lm_15) <- yearnames
names(lm_15) <- c("est","lower","upper")
lm_15$years <- yearnames; lm_15$source <- "Linear Model"
lm_20 <- as.data.frame( lm_fit[[3]][19:23,c(2,6,7)] )
rownames(lm_20) <- yearnames
names(lm_20) <- c("est","lower","upper")
lm_20$years <- yearnames; lm_20$source <- "Linear Model"


lmfixed_10 <- as.data.frame( lmfixed_fit[[1]][19:23,c(2,6,7)] )
rownames(lmfixed_10) <- yearnames
names(lmfixed_10) <- c("est","lower","upper")
lmfixed_10$years <- yearnames; lmfixed_10$source <- "LM fixed tracts"
lmfixed_10 <- data.frame(lapply(lmfixed_10, as.character), stringsAsFactors=FALSE)

lmfixed_15 <- as.data.frame( lmfixed_fit[[2]][19:23,c(2,6,7)] )
rownames(lmfixed_15) <- yearnames
names(lmfixed_15) <- c("est","lower","upper")
lmfixed_15$years <- yearnames; lmfixed_15$source <- "LM fixed tracts"
lmfixed_15 <- data.frame(lapply(lmfixed_15, as.character), stringsAsFactors=FALSE)

lmfixed_20 <- as.data.frame( lmfixed_fit[[3]][19:23,c(2,6,7)] )
rownames(lmfixed_20) <- yearnames
names(lmfixed_20) <- c("est","lower","upper")
lmfixed_20$years <- yearnames; lmfixed_20$source <- "LM fixed tracts"
lmfixed_20 <- data.frame(lapply(lmfixed_20, as.character), stringsAsFactors=FALSE)


plotmodels10 <- rbind(lm_10,lmfixed_10)
plotmodels10$est <- exp( as.numeric(plotmodels10$est) )
plotmodels10$lower <- exp( as.numeric(plotmodels10$lower) )
plotmodels10$upper <- exp( as.numeric(plotmodels10$upper) )

plotmodels15 <- rbind(lm_15,lmfixed_15)
plotmodels15$est <- exp( as.numeric(plotmodels15$est) )
plotmodels15$lower <- exp( as.numeric(plotmodels15$lower) )
plotmodels15$upper <- exp( as.numeric(plotmodels15$upper) )

plotmodels20 <- rbind(lm_20,lmfixed_20)
plotmodels20$est <- exp( as.numeric(plotmodels20$est) )
plotmodels20$lower <- exp( as.numeric(plotmodels20$lower) )
plotmodels20$upper <- exp( as.numeric(plotmodels20$upper) )

# create plots
p1 <- ggplot(plotmodels10, aes(x=years)) + 
  geom_point(aes(y = est, color = source),
             position=position_dodge(0.2)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = source), width=.2,
                position=position_dodge(0.2)) +
  ylim(0.8, 1.20) +
  labs(title = "Interactions between BIP and 2-year Indicators",
       subtitle = "Comparing sales 0-10 miles outside the BIP",
       x="BIP x 2-Year Indicators", 
       y = "Estimate", color = "Model")+
  theme_bw() + theme(legend.position="bottom")

p2 <- ggplot(plotmodels15, aes(x=years)) + 
  geom_point(aes(y = est, color = source),
             position=position_dodge(0.2)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = source), width=.2,
                position=position_dodge(0.2)) +
  ylim(0.80, 1.20) +
  labs(title = "Interactions between BIP and 2-year Indicators",
       subtitle = "Comparing sales 5-15 miles outside the BIP",
       x="BIP x 2-Year Indicators", 
       y = "Estimate", color = "Model")+
  theme_bw() + theme(legend.position="bottom")

p3 <- ggplot(plotmodels20, aes(x=years)) + 
  geom_point(aes(y = est, color = source),
             position=position_dodge(0.2)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = source), width=.2,
                position=position_dodge(0.2)) +
  ylim(0.80, 1.20) +
  labs(title = "Interactions between BIP and 2-year Indicators",
       subtitle = "Comparing sales 10-20 miles outside the BIP",
       x="BIP x 2-Year Indicators", 
       y = "Estimate", color = "Model")+
  theme_bw() + theme(legend.position="bottom")







