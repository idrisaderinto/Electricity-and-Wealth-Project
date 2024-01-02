## ------------------------------------------------------------------------- ##
## Aggregates the household level DHS data for each survey cluster.
## Nigeria 2018 DHS.
##
## Additionally, for each of the 2018 DHS clusters, it computes an average
## Wealth Index of its neighbouring locations.
## ------------------------------------------------------------------------- ##

# set working directory as appropriate
wd <- "/Users/idrisaderinto/Downloads/NG_2018_DHS_11242023_1610_191853/"
setwd(wd)

library(foreign)
library(rgdal)
library(geosphere)


# DHS survey dataset files
# These files can be downloaded from DHS website after registering
# and requesting access:
# https://dhsprogram.com/data/dataset/Nigeria_Standard-DHS_2018.cfm?flag=1

dhs_household_survey_file <- "NGHR7BDT/NGHR7BFL.DTA"
geo_folder <- "NGGE7BFL"
geo_file <- "NGGE7BFL"

# data file
datas <- read.dta(dhs_household_survey_file)

# geo shape files for DHS clusters
geo_datas <- readOGR(dsn = geo_folder, layer = geo_file)

# summary statistics
summary(datas$hv015) # interview status
summary(datas$hv021) # PSU
summary(datas$hv206) # electricity
summary(datas$hv207) # radio
summary(datas$hv208) # TV
summary(datas$hv209) # refrigerator
summary(datas$hv226) # cooking fuel
summary(datas$hv243a) # mobile phone
summary(datas$hv243e) # computer
summary(datas$hv270) # wealth index
summary(datas$hv271) # wealth index factor score
summary(datas$sh121l) # air conditioner
summary(datas$sh121m) # electric iron
summary(datas$sh121n) # generator
summary(datas$sh121o) # fan
summary(datas$sh122i) # tricycle

## aggregate the data we need per cluster level
cluster_data <- data.frame(cluster = as.character(geo_datas@data$DHSCLUST))
cluster_data$num_households <- NA
cluster_data$has_electricity <- NA
cluster_data$has_radio <- NA
cluster_data$has_TV <- NA
cluster_data$has_refrigerator <- NA
cluster_data$has_cooking_fuel <- NA
cluster_data$has_mobile <- NA
cluster_data$has_computer <- NA
cluster_data$cluster_mean_wealth_index <- NA
cluster_data$has_air_conditioner <- NA
cluster_data$has_electric_iron <- NA
cluster_data$has_generator <- NA
cluster_data$has_fan <- NA
cluster_data$has_tricycle <- NA


for (i in 1:nrow(cluster_data)) {
  # the cluster
  clust <- as.character(cluster_data$cluster[i])
  I <- as.character(datas$hv001) == clust
  
  # number households
  num_households <- sum(I)
  cat("Cluster:",clust,"total households:", num_households,"               \r")
  cluster_data$num_households[i] <- num_households
  
  # skip clusters with no households
  if (cluster_data$num_households[i] == 0) { 
    print(paste("Cluster:",clust,"total households:", num_households,"; Skipping cluster."))
    next 
  }
  
  ## asset ownership
  cluster_data$has_electricity[i] <- sum(datas$hv206[I] == "yes")
  cluster_data$has_radio[i] <- sum(datas$hv207[I] == "yes")
  cluster_data$has_TV[i] <- sum(datas$hv208[I] == "yes")
  cluster_data$has_refrigerator[i] <- sum(datas$hv209[I] == "yes")
  cluster_data$has_cooking_fuel[i] <- sum(datas$hv226[I] == "yes")
  cluster_data$has_mobile[i] <- sum(datas$hv243a[I] == "yes")
  cluster_data$has_computer[i] <- sum(datas$hv243e[I] == "yes")
  
  cluster_data$cluster_mean_wealth_index[i] <- mean(datas$hv271[I])
  
  cluster_data$has_air_conditioner[i] <- sum(datas$sh121l[I] == "yes")
  cluster_data$has_electric_iron[i] <- sum(datas$sh121m[I] == "yes")
  cluster_data$has_generator[i] <- sum(datas$sh121n[I] == "yes")
  cluster_data$has_fan[i] <- sum(datas$sh121o[I] == "yes")
  cluster_data$has_tricycle[i] <- sum(datas$sh122i[I] == "yes")
}

# compute fraction of households with different assets
assets <- c("has_electricity","has_radio","has_TV","has_refrigerator","has_cooking_fuel",
            "has_mobile","has_computer","has_air_conditioner",
            "has_electric_iron","has_generator","has_fan","has_tricycle")

for (item in assets) {
  item_frac <- paste(item,"frac",sep="_")
  cluster_data[item_frac] <- cluster_data[,item]/cluster_data$num_households
}

# merge with cluster geographic information
geo_sub <- geo_datas@data[,c("DHSID","DHSCC","DHSYEAR","DHSCLUST",
                             "ADM1DHS","ADM1NAME","DHSREGCO","DHSREGNA","SOURCE","URBAN_RURA","LATNUM","LONGNUM")]
geo_sub$DHSCLUST <- as.character(geo_sub$DHSCLUST)
geo_sub <- merge(x = geo_sub, by.x = "DHSCLUST", 
                 y = cluster_data, by.y = "cluster", all.x = TRUE)

# create the regional indicator variables
dhs_regions <- levels(as.factor(geo_sub$DHSREGNA))
for (reg in dhs_regions) {
  reg_dummy_var <- paste("region_is",reg,sep="_")
  geo_sub[reg_dummy_var] <- ifelse(as.character(geo_sub$DHSREGNA) == reg, 1, 0)
}

# save
write.csv(geo_sub,"Nigeria_dhs_dataset.csv", row.names = FALSE)
