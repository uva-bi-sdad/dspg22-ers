# Threshold for for the number of properties 

# packages
library(dplyr)
library(plyr)
library(readr)
require(INLA) # required for the spatial models
require(sp)
library(mcmcr)
require(geosphere) # haversine distance
# function to compute distance
find_min_dist <- function(site, sites) {
  min(distHaversine(site, sites,r = 6371)) # in miles
}

setwd("~/git/rural_broadband/src/")

# Add individual progs plots here ....
source('~/git/rural_broadband/src/BIP_analysis/spPlot.r') # for spatial interpolation and mapping
usa_shape <- raster::getData("GADM",country="USA", level=1)

# BB elibibility and ACS vars
# use if required
if(!"bb_eligibility"%in% ls())bb_eligibility <- read.csv("~/git/rural_broadband/src/BIP_analysis/broadband_tract_eligibility12-15-20.csv")


# Supply states and groups of states for the RCP area
state.symb <- c("VA") # "GA", "VA", "AZ" "NC"
state <- c("Virginia") # "Georgia",  "Virginia", "Arizona", North Carolina
fp_code <- c(51) # 13 51 04 37

# RCP geoms and project IDs
RCP_geom <- readRDS(paste0("~/R/broadband/RCP_shapes_by_state/",state.symb,"_rcp_shapes.rds")) 
#RCP_geom <- readRDS(paste0("~/R/broadband/RCP_shapes_by_state_round2/",state.symb,"_rcp_shapes.rds")) 
# Project ID
rcp_objectid <- "39364" # "35309", "39364", TojuG

# geography coordinates format
proj_crs <- CRS(" +proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
rcp_geom <- sf::st_transform(RCP_geom,crs=sf::st_crs(usa_shape))

# Plot By Project/RUS ID
rus_shape <- subset(rcp_geom, OBJECTID_1==rcp_objectid)
# rus_shape <- subset(rcp_geom, progID==rcp_objectid)
state.symb <- rus_shape$stateABBR

obl_date <- as.Date(rus_shape$`Obligation/Rejection Date`, format = "%Y-%m-%d")

# load housing data
if(length(state.symb[[1]])==1){
  load(paste0("~/R/broadband/BK_by_state/BK_",state.symb,".RData"))}else{ 
    for(i in 1:length(state.symb[[1]])){
      print(state.symb[[1]][i])
      if (!exists("dataset")){
        load(paste0("~/R/broadband/BK_by_state/BK_",state.symb[[1]][i],".RData"))
        dataset <- state_data.keep
        
      }
      
      # if data already exist, then append it together
      if (exists("dataset")){
        load(paste0("~/R/broadband/BK_by_state/BK_",state.symb[[1]][i],".RData"))
        dataset <-unique(rbind(dataset, state_data.keep))
        rm(state_data.keep)
      }
    }
    state_data.keep <- dataset
  }

# NAs
state_data.keep  <- state_data.keep[!is.na(state_data.keep$property_address_latitiude),]
id.st <- state_data.keep$state%in%state.symb[[1]]
id.na <- rowSums(is.na(state_data.keep[id.st,c("property_address_longitude","property_address_latitiude")]))==2
id.st <- id.st[!id.na]

plot(subset(usa_shape,NAME_1%in%state))
out.plot <- try(points(rus_shape$geometry[[1]][[1]]))
lines(rus_shape$geometry[[1]][[1]])

if(class(out.plot)=="try-error"){
  points(rus_shape$geometry[[1]][[1]][[1]])
  lines(rus_shape$geometry[[1]][[1]][[1]])
}else{lines(rus_shape$geometry[[1]][[1]])}

if(class(out.plot)=="try-error"){
  dat <- data.frame(rus_shape$geometry[[1]][[1]][[1]])
}else dat <- data.frame(rus_shape$geometry[[1]][[1]])

colnames(dat) <- c("long","lat");  coordinates(dat) <- ~long+lat

coords.data <- data.frame(apply(state_data.keep[id.st, 
                                                c("property_address_latitiude","property_address_longitude")],2,function(x) as.numeric(x)))
row.has.na <- apply(coords.data, 1, function(x){any(is.na(x))})
coords.data <- coords.data[!row.has.na, ]
coords.data <- data.frame(coords.data)

coordinates(coords.data) <- ~property_address_longitude+property_address_latitiude

proj4string(dat) <- sp::proj4string(coords.data)  <- sp::proj4string(usa_shape)

dat.p <- Polygon(dat)
dat.ps <- Polygons(list(dat.p),1)
dat.sps <- SpatialPolygons(list(dat.ps))
dat.sps.enlarged <- rgeos::gBuffer(dat.sps, byid = T,width = 0.1) 

bbox.bip <- bbox(dat.sps.enlarged)

id.near <- apply(coords.data@coords,1,function(x){
  ifelse(x[1]>=bbox.bip["x","min"] & x[1]<=bbox.bip["x","max"] & x[2]>=bbox.bip["y","min"] & x[2]<=bbox.bip["y","max"] ,T,F)
})
sum(id.near)
coords.data.near <- coords.data[id.near,]

proj4string(dat.sps) <- proj4string(coords.data.near)
points.inBIP <- !is.na(over(coords.data.near,dat.sps))
proj4string(dat.sps.enlarged) <- proj4string(coords.data.near)
points.inBIP.en <- !is.na(over(coords.data.near,dat.sps.enlarged))

coords.bip <- coords.data.near[points.inBIP,]
coords.out <- coords.data.near[ifelse(points.inBIP.en-points.inBIP,T,F),]

# Distance to BIP
dist.bip.in <- apply(coords.bip@coords,1,find_min_dist,sites=dat)
dist.bip.out <- apply(coords.out@coords,1,find_min_dist,sites=dat)

# BIP
id <- as.numeric(rownames(rbind(coords.bip@coords,coords.out@coords)))
# alter master file only run the first time: 
# state_data.keep$dist_bip <- NA

# change program name
rcp_name <- rcp_objectid
state_data.keep[which(id.st)[id],"RCP"] <- c(rep(rcp_name,nrow(coords.bip@coords)),rep(paste("n",rcp_name,sep=""),nrow(coords.out@coords)))
state_data.keep[which(id.st)[id],"dist_rcp"] <- c(dist.bip.in,-dist.bip.out)

# create state data
data.RCP <- state_data.keep[which(id.st)[id],]; dim(data.RCP)
data.RCP$long <- as.numeric(data.RCP$property_address_longitude)
data.RCP$lat <- as.numeric(data.RCP$property_address_latitiude)
data.RCP$rcp <- c(rep(0,nrow(coords.bip@coords)),rep(1,nrow(coords.out@coords)))
shape <- subset(usa_shape, NAME_1%in%state)

################################################################################
# PARTS ABOVE DEAL WITH FINDING PROPERTIES INSIDE AND OUTSIDE THE PROJECT AREA 
################################################################################
#########################################
# PLOTS START HERE
#########################################

bfr_id <- data.RCP$sale_date <= obl_date
aftr_id <- data.RCP$sale_date > obl_date

mat <- matrix(c(1,2),nr=1,nc=2,byrow=T)
layout(mat,
       widths = rep(c(5),ncol(mat)),
       heights = rep(5,ncol(mat)))

pdf(paste("~/Ind-plot-RCP",rcp_objectid,"-",state.symb,".pdf",sep=""))

bfr_data <- cbind(data.RCP$long[bfr_id],data.RCP$lat[bfr_id],
                  log(data.RCP$sales_price[bfr_id]))
spPlot(11,"Spectral",data_frame = bfr_data,
       xlim = bbox(dat.sps.enlarged)["x",],ylim = bbox(dat.sps.enlarged)["y",],
       shape = shape, main="Log-Price (Before)")
#lines(shape,xlim=bbox(dat.sps.enlarged)["x",],ylim=bbox(dat.sps.enlarged)["y",])
plot(dat.sps,add=T,lwd=2,border="white")
plot(dat.sps.enlarged,add=T,lwd=2,border="black")
points(bfr_data[data.RCP$rcp[bfr_id]==0, c(1,2)], cex=0.6,col="green")# 
points(bfr_data[data.RCP$rcp[bfr_id]==1, c(1,2)], cex=0.6,col="orange") #


aftr_data <- cbind(data.RCP$long[aftr_id],data.RCP$lat[aftr_id],
                   log(data.RCP$sales_price[aftr_id]))
spPlot(11,"Spectral",data_frame = aftr_data,
       xlim = bbox(dat.sps.enlarged)["x",],ylim = bbox(dat.sps.enlarged)["y",],
       shape = shape, main="Log-Price (After)")
#plot(shape,xlim=bbox(dat.sps.enlarged)["x",],ylim=bbox(dat.sps.enlarged)["y",])
plot(dat.sps,add=T,lwd=2,border="white")
plot(dat.sps.enlarged,add=T,lwd=2,border="black")
points(aftr_data[data.RCP$rcp[aftr_id]==0, c(1,2)], cex=0.6,col="green")# 
points(aftr_data[data.RCP$rcp[aftr_id]==1, c(1,2)], cex=0.6,col="orange") # 

#spPlot(11,"Spectral",data_frame = bfr_data,
#       xlim = bbox(dat.sps.enlarged)["x",],ylim = bbox(dat.sps.enlarged)["y",],
#       shape = shape, main="Program Shape")
#plot(dat.sps,add=T,lwd=2,border="cyan")
#plot(dat.sps.enlarged,add=T,lwd=2,border="black")

###################################
# THESE ARE THE PLOTS YOU WANT
###################################

# before (2016-18)
plot(data.RCP$dist_rcp[bfr_id],
     log(data.RCP$sales_price[bfr_id]),
     xlab="Distance from RCP (miles)", ylab="Log(Sale Price)", main="Log-Price vs. Distance to RCP (Pre 2019)",
     ylim=c(7,16))
abline(h=median(log(data.RCP$sales_price[bfr_id & data.RCP$rcp==0])), col="green", lwd=1.5)
abline(h=median(log(na.exclude(data.RCP$sales_price[bfr_id & data.RCP$rcp==1]))), col="orange", lwd=1.5)
abline(v=0, col="red",lwd=2.5)
grid()
legend("topleft",inset=0.01,
       legend = c("RCP","Median SP outside", "Median SP inside"),
       lwd=c(2,1.5,1.5),
       col=c("red","orange","green"))

# after (2019-21)
plot(data.RCP$dist_rcp[aftr_id],
     log(data.RCP$sales_price[aftr_id]),pch=16, col="cyan",
     xlab="Distance from RCP (miles)", ylab="Log(Sale Price)", main="Log-Price vs. Distance to RCP (Post 2019)",
     ylim=c(7,16))
abline(h=median(log(data.RCP$sales_price[aftr_id & data.RCP$rcp==0])), col="darkgreen", lwd=1.5)
abline(h=median(log(na.exclude(data.RCP$sales_price[aftr_id & data.RCP$rcp==1]))), col="darkred", lwd=1.5)
abline(v=0, col="red",lwd=2.5)
grid()
legend("topleft",inset=0.01,
       legend = c("RCP", "Median SP outside", "Median SP inside"),
       lwd=c(2,1.5,1.5),
       col=c("red","darkred","darkgreen"))
