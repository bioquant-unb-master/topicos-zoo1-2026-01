#Elevation data tiff
# Get elevation data for Spain. This time without masking!
library(geodata)
elev_Esp_1km <- geodata::elevation_30s(country='ESP', path='data', mask=F)

# Aggregate to ca. 5km resolution
elev_Esp_5km <- terra::aggregate(elev_Esp_1km, fact=5)
plot(elev_Esp_5km)
# write to file:
terra::writeRaster(elev_Esp_5km,filename='elev_Esp_5km.tif')
#
#Climate data
library(terra)

# Download global bioclimatic data from worldclim (you may have to set argument 'download=T' for first download, if 'download=F' it will attempt to read from file):
clim_1km <- geodata::worldclim_country(country='ESP', var = 'bio', download = F, path = 'data')

# Now, let's look at the data:
clim_1km
terra::plot(clim_1km)
# The climate data are provided within a larger spatial extent than the elevation data, and we thus crop them
clim_1km_Esp <- terra::crop(clim_1km, elev_Esp_1km)

# Aggregate to 2.5 min spatial resolution
clim_5km_Esp <- terra::aggregate(clim_1km_Esp, fact=5)

# Plot the climate data
terra::plot(clim_5km_Esp)
names(clim_5km_Esp)
# Overwrite all climate layer names (Careful, you should only do this when you are certain that all variables are ordered in the same way)
names(clim_5km_Esp) <- paste('bio',1:19,sep='_')
names(clim_5km_Esp)
terra::writeRaster(clim_5km_Esp,filename='bioclim_ESP_5km.tif')
#
#
#Tree Cover data
# Download fractional tree cover at 30-sec (roughly 1 km) resolution:
# Please note that you have to set download=T if you haven't downloaded the data before:
trees_1km <- geodata::landcover(var='trees', path='data', download=F)

# Crop to study area extent
trees_1km_Esp <- terra::crop(trees_1km, elev_Esp_1km)

# map the tree cover
plot(trees_1km_Esp)
# Aggregate tree cover to 2.5-min spatial resolution (roughly 5 km)
trees_5km_Esp <- terra::aggregate(trees_1km_Esp, fact=5, fun='mean')

# Map the 2.5-min tree cover (roughly 5 km)
plot(trees_5km_Esp)
# Save to file
terra::writeRaster(trees_5km_Esp,filename='trees_ESP_5km.tif')

#Crops Cover data
# Download fractional tree cover at 30-sec (roughly 1 km) resolution:
# Please note that you have to set download=T if you haven't downloaded the data before:
crops_1km <- geodata::landcover(var='cropland', path='data', download=F)

# Crop to study area extent
crops_1km_Esp <- terra::crop(crops_1km, elev_Esp_1km)

# map the  cover
plot(crops_1km_Esp)
# Aggregate crops cover to 2.5-min spatial resolution (roughly 5 km)
crops_5km_Esp <- terra::aggregate(crops_1km_Esp, fact=5, fun='mean')

# Map the 2.5-min crops cover (roughly 5 km)
plot(crops_5km_Esp)
# Save to file
terra::writeRaster(crops_5km_Esp,filename='crops_ESP_5km.tif')
