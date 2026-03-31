
#Plot Mapbiomas tiff data in rstudio

library(terra)
library(ggplot2)
library(tidyterra)
#Load Data in R
#Download land cover maps in GeoTIFF (.tif) format for your desired region from the MapBiomas platform.
#Download the official MapBiomas legend colors (CSV or palette codes) to match the official colors.
# Load the raster data
mapa <- rast("mapbiomas-df-col-08-2022.tif")

# If you have multiple years, you can inspect them
# plot(mapa)
#Plot using tidyterra
ggplot() +
  geom_spatraster(data = mapa) +
  # Use official MapBiomas color palettes (example for specific class)
  #scale_fill_mapbiomas(palette = "brazil_land_cover") +
  theme_minimal() +
  
  labs(fill = "Land Cover Class")
# How to create a named vector from a csv in R
#Read the CSV.
df <- read.csv("your_file.csv")
#Create the named vector. Replace value_col with the column containing data and name_col with the column containing the labels.
named_vector <- setNames(df$value_col, df$name_col)

