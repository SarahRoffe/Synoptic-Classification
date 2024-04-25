# PCA classification using MSLP, zg500 and 850 temp

#Working directory
setwd("~/Desktop/Ngwako_synoptic_classification")
dir()

#Libraries
library(dplyr)
library(metR)
library(synoptReg)

### Load and tidy data

# zg500
GlanceNetCDF("z_levels_mean_m.nc")
z_levels_mean = ReadNetCDF("z_levels_mean_m.nc", vars = "z")
head(z_levels_mean)

# extract zg500 level and remove level column
zg500 = z_levels_mean %>%
  filter(level==500)

zg500 = dplyr::select(zg500, -c(level))
head(zg500)

zg500$var = "z"
zg500$units = "m"
colnames(zg500)[4] <- "value"
colnames(zg500)[2] <- "y"
colnames(zg500)[3] <- "x"
head(zg500)
summary(zg500) 
unique(zg500$time)

# 850 temp
GlanceNetCDF("temp_mean_degC1.nc")
temp_mean = ReadNetCDF("temp_mean_degC1.nc", vars = "t")
head(temp_mean)

# extract 850 level temp and remove level column
t850 = temp_mean %>%
  filter(level==850)

t850 = dplyr::select(t850, -c(level))
head(t850)

t850$var = "t"
t850$units = "degC"
colnames(t850)[4] <- "value"
colnames(t850)[2] <- "y"
colnames(t850)[3] <- "x"
head(t850)
summary(t850) 
unique(t850$time)

# MSLP
GlanceNetCDF("mslp_mean_hpa1.nc")
mslp_mean = ReadNetCDF("mslp_mean_hpa1.nc", vars = "msl")
head(mslp_mean)

mslp_mean$var = "msl"
mslp_mean$units = "hPa"
colnames(mslp_mean)[4] <- "value"
colnames(mslp_mean)[2] <- "y"
colnames(mslp_mean)[3] <- "x"
head(mslp_mean)
summary(mslp_mean)
unique(mslp_mean$time)

### Merge the atm data for the PCA (t-mode)
atmos_data = dplyr::bind_rows(mslp_mean,zg500,t850)
summary(atmos_data)
str(atmos_data)

### Now to prep for PCA
# Deciding on the number of PC to retain
info = pca_decision(atmos_data, norm = TRUE, matrix_mode = "T-mode")
# Error in cov.wt(z) : 'x' must contain finite values only

info #should print the scree plot etc. which helps to decide on the number of pc's to retain...

# Now run the t-mode PCA
tmode_classification = synoptclas(atmos_data, ncomp = 6, matrix_mode = "T-mode", norm = TRUE) # retain x PCs based on results for pca_decision
tmode_classification

