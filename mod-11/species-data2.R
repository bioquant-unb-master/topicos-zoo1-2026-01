library(rebird)
library(tidyverse)
# Look at eBird taxonomy
rebird:::tax
species_code('Neothraupis fasciata')
# Find all eBird reportings of red kite in Spain in the last 30 days: 
# Find all eBird reportings of red kite in Spain in the last 30 days: 
#ebirdregion(loc = 'ES', species = 'redkit1', back=30, key='yourkey')
# Download eBird detection in Spain for the months April-June 2022:

#ebd_Esp_2022 <- bind_rows(
    # Apply the download function to each day in the provided date sequence and collect output in a list
    #lapply(seq(as.Date('2022/04/01'),as.Date('2022/06/30'),'days'), FUN=function(x){
    #  ebirdhistorical(loc = 'ES', date=x, fieldSet='full', key='yourkey')
 #   }))

#days2=seq(from=as.Date('2022/04/01'),to=as.Date('2022/06/30'),by=1)
#FUN2=function(x){
#  ebirdhistorical(loc = 'ES', date=x, fieldSet='full', key='yourkey')}        
#ebd_Esp_2022 <- bind_rows(
  # Apply the download function to each day in the provided date sequence and collect output in a list
#  lapply(days2,FUN2))
#saveRDS(ebd_Esp_2022, file = "spainebird.rds")
ebd_Esp_2022=spainebird
# Replace any observations from other species than the red kite with zero
redkite_Esp_2022 <- ebd_Esp_2022
redkite_Esp_2022$howMany <- ifelse(redkite_Esp_2022$speciesCode==species_code('Milvus milvus'), redkite_Esp_2022$howMany, 0)
# Add a column for red kite presence
redkite_Esp_2022$RedKite <- ifelse(redkite_Esp_2022$howMany > 0, 1, 0)
# Get mask for Spain
library(geodata)
library(terra)
elev_Esp_1km_masked <- geodata::elevation_30s(country='ESP', path='data', mask=TRUE)

# Aggregate to ca. 5km resolution
elev_Esp_5km_masked <- terra::aggregate(elev_Esp_1km_masked, fact=5)
plot(elev_Esp_5km_masked)
# Create mask of spain
mask_Esp_5km <- elev_Esp_5km_masked
values(mask_Esp_5km)[!is.na(values(mask_Esp_5km))] <- 1
plot(mask_Esp_5km)
# Obtain coordinates and cell ID
redkite_Esp_2022 <- cbind(redkite_Esp_2022, terra::extract(x=mask_Esp_5km, y=redkite_Esp_2022[,c('lng','lat')], ID=F, cells=T, xy=T))
# Make smaller dataset with fewer columns
redkite_det_Esp_2022 <- redkite_Esp_2022[,c('obsDt','cell','x','y','RedKite')]

# Remove NA cells
redkite_det_Esp_2022 <- na.exclude(redkite_det_Esp_2022)
# How many presences do we have in the data set?
sum(redkite_det_Esp_2022$RedKite)
# Add columns for day of year, week, and month
library(lubridate)
redkite_det_Esp_2022$yday <- yday(as.Date(redkite_det_Esp_2022$obsDt, format="%Y-%m-%d"))
redkite_det_Esp_2022$week <- week(as.Date(redkite_det_Esp_2022$obsDt, format="%Y-%m-%d"))
redkite_det_Esp_2022$month <- month(as.Date(redkite_det_Esp_2022$obsDt, format="%Y-%m-%d"))

# Remove duplicate rows that have exact same cell, month, and presence/absence information (meaning only ZEROs or ONEs)
redkite_det_Esp_2022 <- redkite_det_Esp_2022[!duplicated(redkite_det_Esp_2022[,c(2,5,8)]),]

# We now may have some rows left that report contradicting information on presences and absences for the exact same cell and month:
sum(duplicated(redkite_det_Esp_2022[,c('cell','month')]))
duplos <- which(duplicated(redkite_det_Esp_2022[,c('cell','month')]))

# Loop through the entries with the same cell and month combination: 
for (i in duplos) {
  cell_i = redkite_det_Esp_2022[i,'cell']
  month_i = redkite_det_Esp_2022[i,'month']
  
  # If we have a presence and an absence observation on the same day and location, we assume it is a presence:
  redkite_det_Esp_2022[redkite_det_Esp_2022$cell==cell_i & redkite_det_Esp_2022$month==month_i, 'RedKite'] <- 1
} 

# Remove remaining duplicates with exact same cell, month, and presence/absence information (meaning only ZEROs or ONEs) - Note that new duplicates may have arisen as we have overwritten some presence/absence information in the previous loop
redkite_det_Esp_2022 <- redkite_det_Esp_2022[!duplicated(redkite_det_Esp_2022[,c(2,5,8)]),]
# Reshape the dataset from long to wide format:
redkite_det_Esp_2022_wide <- redkite_det_Esp_2022[,c(2:5,8)] %>%
  pivot_wider(
    names_from = month,
    values_from = RedKite,
    names_prefix = 'month',
    values_fill = 0
  )

# How often was the red kite observed over the three visits?
table(rowSums(redkite_det_Esp_2022_wide[,4:6]))
# plot mask of Spain
plot(mask_Esp_5km, col='grey90', legend=F)

# Add points
points(redkite_det_Esp_2022_wide[,2:3],pch=19,col=c('grey50','red','violet','blue')[as.factor(rowSums(redkite_det_Esp_2022_wide[,4:6]))], cex=0.3)

legend('bottomright', legend=0:3, pch=19, col=c('grey50','red','violet','blue'), bty='n')
save(redkite_det_Esp_2022_wide,file='redkite_det_Esp_2022_wide.RData')
# Load species data:
load('redkite_env_thinned.RData')

summary(redkite_env_thinned)
# Read in 5km mask of Spain:
library(terra)
mask_Esp_5km <- terra::rast('mask_Esp_5km.tif')

# Map the data:
plot(mask_Esp_5km, col='grey90', legend=F)
points(redkite_env_thinned[,1:2],pch=19,col=c('grey50','red','violet','blue')[as.factor(rowSums(redkite_env_thinned[,4:6]))], cex=0.3)
# Naive occupancy estimate:
sum(apply(redkite_env_thinned[,4:6], 1, max)) / nrow(redkite_env_thinned)
library(unmarked)

# Create unmarked object containing the detection and non-detection data:
occ_redkite <- unmarkedFrameOccu(y=redkite_env_thinned[,4:6])
# Fitting simple single season occupancy models without survey or site covariates (intercept model):
m_occ_null <- occu(~1 ~ 1, occ_redkite)

# Summarise model output:
summary(m_occ_null)
# Get detection probability estimate:
(p_det <- backTransform(m_occ_null, "det"))
# Get occupancy probability estimate:
(p_psi <- backTransform(m_occ_null, "state"))
# Standardise all covariates to have a mean=0 and standard deviation sd=1:
redkite_env_thinned[,-c(1:7)] <- scale(redkite_env_thinned[,-c(1:7)])

# Create unmarked object containing the detection and non-detection data and the covariate data:
occ_env_redkite <- unmarkedFrameOccu(y = redkite_env_thinned[,4:6], siteCovs = redkite_env_thinned[,-c(1:7)])
# Fitting simple single season occupancy models without survey or site covariates (intercept model):
m_occ_bio1 <- occu(~1 ~ bio_1, occ_env_redkite)

# Summarise model output:
summary(m_occ_bio1)
# Add quadratic term:
occu(~1 ~ bio_1 + I(bio_1^2), occ_env_redkite)

# Add quadratic term using poly():
occu(~1 ~ poly(bio_1, degree=2), occ_env_redkite)

# Add another covariate:
occu(~1 ~ bio_1 + cropland, occ_env_redkite)

# Add quadratic terms for both covariates:
occu(~1 ~ bio_1 + I(bio_1^2) + cropland + I(cropland^2), occ_env_redkite)
(m_occdet_bio1q_cropq <- occu(~ESP_elv ~ bio_1 + I(bio_1^2) + cropland + I(cropland^2), occ_env_redkite))
# Extract AIC
m_occ_bio1@AIC
m_occdet_bio1q_cropq@AIC
# Collect the candidate models in a named list. Try to use meaningful names of the models
cand_models <- fitList(
  "p(1) psi(1)" = occu(~1 ~ 1, occ_env_redkite),
  "p(1) psi(bio1)" = occu(~1 ~ bio_1, occ_env_redkite),
  "p(1) psi(bio1.sqr)" = occu(~1 ~ bio_1 + I(bio_1^2), occ_env_redkite),
  "p(1) psi(cropland)" = occu(~1 ~ cropland, occ_env_redkite),
  "p(1) psi(cropland.sqr)" = occu(~1 ~ cropland + I(cropland^2), occ_env_redkite),
  "p(1) psi(bio1 + cropland)" = occu(~1 ~ bio_1 + cropland, occ_env_redkite),
  "p(1) psi(bio1.sqr + cropland)" = occu(~1 ~ bio_1 + I(bio_1^2) + cropland, occ_env_redkite),
  "p(1) psi(bio1.sqr + cropland.sqr)" = occu(~1 ~ bio_1 +I(bio_1^2) + cropland + I(cropland^2), occ_env_redkite),
  "p(elev) psi(1)" = occu(~ESP_elv ~ 1, occ_env_redkite),
  "p(elev) psi(bio1)" = occu(~ESP_elv ~ bio_1, occ_env_redkite),
  "p(elev) psi(bio1.sqr)" = occu(~ESP_elv ~ bio_1 + I(bio_1^2), occ_env_redkite),
  "p(elev) psi(cropland)" = occu(~ESP_elv ~ cropland, occ_env_redkite),
  "p(elev) psi(cropland.sqr)" = occu(~ESP_elv ~ cropland + I(cropland^2), occ_env_redkite),
  "p(elev) psi(bio1 + cropland)" = occu(~ESP_elv ~ bio_1 + cropland, occ_env_redkite),
  "p(elev) psi(bio1.sqr + cropland)" = occu(~ESP_elv ~ bio_1 + I(bio_1^2) + cropland, occ_env_redkite),
  "p(elev) psi(bio1.sqr + cropland.sqr)" = occu(~ESP_elv ~ bio_1 +I(bio_1^2) + cropland + I(cropland^2), occ_env_redkite)
)

# Compute the summary output for model selection. When explicitly stating which candidate model is the null model, then a Nagelkerke R-square is computed.
(m_sel <- modSel(cand_models, nullmod="p(1) psi(1)"))
