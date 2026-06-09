library(unmarked)

# Load species data:
load('redkite_env_thinned.RData')

# Estimate the covariate means and sd
cov_means <- colMeans(redkite_env_thinned[,-c(1:7)])
cov_sd <- apply(redkite_env_thinned[,-c(1:7)],2,sd)

# Standardise using the covariate means and sd
redkite_env_scaled <- redkite_env_thinned
redkite_env_scaled[,-c(1:7)] <- scale(redkite_env_scaled[,-c(1:7)], center=cov_means, scale=cov_sd)

# Create unmarked object containing the detection and non-detection data and the covariate data:
occ_env_redkite <- unmarkedFrameOccu(y = redkite_env_scaled[,4:6], siteCovs = redkite_env_scaled[,-c(1:7)])
# Fit occupancy-detection model
(m_occdet <- occu(~ ESP_elv ~ bio_3 + cropland + I(cropland^2), occ_env_redkite))
modSel(fitList(
  "our model"= m_occdet, 
  "null" = occu(~ 1 ~ 1, occ_env_redkite)), nullmod="null")
library(AICcmodavg)

# compute observed and expected frequencies of detection histories
mb.chisq(m_occdet)
# perform bootstrapped GoF-test
(gof.results <- mb.gof.test(m_occdet, nsim=1000))
# Create covariate gradients (at original scale):
grad.bio3 <- seq(min(redkite_env_thinned$bio_3), max(redkite_env_thinned$bio_3), length=100)
grad.cropland <- seq(min(redkite_env_thinned$cropland), max(redkite_env_thinned$cropland), length=100)
grad.elev <- seq(min(redkite_env_thinned$ESP_elv), max(redkite_env_thinned$ESP_elv), length=100)

# Standardise them using the stored means and sd of our data
grad.bio3.scaled <- scale(grad.bio3, center=cov_means['bio_3'], scale=cov_sd['bio_3'])
grad.cropland.scaled <- scale(grad.cropland, center=cov_means['cropland'], scale=cov_sd['cropland'])
grad.elev.scaled <- scale(grad.elev, center=cov_means['ESP_elv'], scale=cov_sd['ESP_elv'])

# Make occupancy prediction - predictions are done separately for each gradient, while the other covariate is kept constant at their mean (mean=0 as the covariates were standardised)
dummyData <- data.frame(bio_3=grad.bio3.scaled, cropland=0)
pred.occ.bio3 <- predict(m_occdet, type="state", newdata=dummyData, appendData=TRUE)

dummyData <- data.frame(bio_3=0, cropland=grad.cropland.scaled)
pred.occ.cropland <- predict(m_occdet, type="state", newdata=dummyData, appendData=TRUE)

# Make detectability prediction - here, we only have one gradient in our example
dummyData <- data.frame(ESP_elv=grad.elev.scaled)
pred.det.elev <- predict(m_occdet, type="det", newdata=dummyData, appendData=TRUE)
# partition the graphics device into 2x2 panels (switch back to 1-panel window with par(mfrow = c(1,1)) )
par(mfrow = c(2,2))

# plot the response curves for occupancy probability along bio3 gradient:
plot(pred.occ.bio3[[1]] ~ grad.bio3, type = "n", ylim = c(0,1), ylab = "Pred. occupancy prob.", xlab = "bio3") 
polygon(c(grad.bio3,rev(grad.bio3)), c(pred.occ.bio3[,3],rev(pred.occ.bio3[,4])), col='grey', border=NA)
lines(pred.occ.bio3[[1]] ~ grad.bio3, lwd=3, col='blue')

# plot the response curves for occupancy probability along cropland gradient:
plot(pred.occ.cropland[[1]] ~ grad.cropland, type = "n", ylim = c(0,1), ylab = "Pred. occupancy prob.", xlab = "Fraction of cropland") 
polygon(c(grad.cropland,rev(grad.cropland)), c(pred.occ.cropland[,3],rev(pred.occ.cropland[,4])), col='grey', border=NA)
lines(pred.occ.cropland[[1]] ~ grad.cropland, lwd=3, col='blue')

# plot the response curves for detection probability along elevation gradient:
plot(pred.det.elev[[1]] ~ grad.elev, type = "n", ylim = c(0,1), ylab = "Pred. detection prob.", xlab = "Elevation") 
polygon(c(grad.elev,rev(grad.elev)), c(pred.det.elev[,3],rev(pred.det.elev[,4])), col='grey', border=NA)
lines(pred.det.elev[[1]] ~ grad.elev, lwd=3, col='blue')
# Read environmental layers:
library(terra)
clim_5km <- terra::rast('bioclim_ESP_5km.tif')
crop_5km <- terra::rast('crops_ESP_5km.tif')
elev_5km <- terra::rast('elev_Esp_5km.tif')
# Create data frame with environmental data and coordinates (exclude non-terrestrial cells with NAs):
env_dat <- na.exclude(
  as.data.frame(c(clim_5km,elev_5km,crop_5km), xy=T)
)

# Standardise data (using the previous scaling coefficients!!):
env_dat_scaled <- env_dat
env_dat_scaled <- data.frame(scale(env_dat_scaled[,-c(1:2)], center=cov_means[names(env_dat_scaled[,-c(1:2)])], scale=cov_sd[names(env_dat_scaled[,-c(1:2)])]))
# Make occupancy predictions and append to x/y coordinates of environmental data frame:
pred_occ <- cbind(env_dat[,1:2],predict(m_occdet, newdata=env_dat_scaled, type="state"))

# Make detectability predictions and append to x/y coordinates of environmental data frame:
pred_det <- cbind(env_dat[,1:2],predict(m_occdet, newdata=env_dat_scaled, type="det"))
# Make multi-layer SpatRaster from prediction data frame:
pred_occ_r <- terra::rast(pred_occ[,c('x','y','Predicted','SE')], type='xyz', crs=crs(clim_5km))
pred_det_r <- terra::rast(pred_det[,c('x','y','Predicted','SE')], type='xyz', crs=crs(clim_5km))
# Map predicted occupancy along with its standard error
terra::plot(pred_occ_r, range=c(0,1))
# Map predicted detectability along with its standard error
terra::plot(pred_det_r, range=c(0,0.2))
