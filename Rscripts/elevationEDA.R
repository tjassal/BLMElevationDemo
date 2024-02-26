#Title: elevationEDA.R
# 25 Feb 2024
#Author: Tim Assal
# objective: pretty stock EDA using the elevatR package; see notes at bottom for 1 m data availibility


#The elevatr package was written to standarize access to elevation data from web APIs.
#As of version 0.4.2, there are several endpoints that elevatr accesses. For point elevation data 
#it uses USGS Elevation Point Query Service (United States only) as well as the Amazon Web Services (AWS) 
#Terrain Tiles from which point elevations are extracted. Raster elevation data 
#(i.e., Digital Elevation Models or DEMs) are available from the AWS Terrain Tiles or from the 
#OpenTopography Global DEM API (https://portal.opentopography.org/apidocs/#/Public/getGlobalDem) .
#Currently, elevatr supports the SRTMGL3, SRTMGL1, AW3D30, and SRTM15Plus datasets.

library(elevatr)
library(sf)
library(terra)

#USGS elevation point query service
# Create an example data.frame
set.seed(65.7)
examp_df <- data.frame(x = runif(3, min = -73, max = -72.5), y = runif(3, min = 42,
                                                                       max = 43))
head(examp_df)
crs_dd <- 4326

# Create and example data.frame with additional columns
cats <- data.frame(category = c("H", "M", "L"))
examp_df2 <- data.frame(examp_df, cats)
head(examp_df2)

# Create an example
examp_sf <- sf::st_as_sf(examp_df2, coords = c("x", "y"), crs = crs_dd)
examp_sf #needs to be an sf 
test_elev_epqs <- get_elev_point(examp_sf, prj = crs_dd, src = "epqs")
#works on sf or df


df_elev_epqs <- get_elev_point(examp_df, prj = crs_dd, src = "epqs")
?get_elev_point #src is either epgs or aws

df2_elev_epqs <- get_elev_point(examp_df2, prj = crs_dd, src = "epqs")

#Point elevation from Amazon Web Service Terrain Tiles
df_elev_aws <- get_elev_point(examp_df, prj = crs_dd, src = "aws")

#An important thing to note, that the elevations will differ, and the prime reason is the resolution of 
#the AWS tiles at the specified zoom. The default zoom of 5 (i.e., z=5) is rather coarse and that is 
#reflected in the elevations.
df_elev_aws$elevation

#A larger zoom results in a smaller pixel size and the two sources converge.

df_elev_aws_z12 <- get_elev_point(examp_df, prj = crs_dd, src = "aws", z = 12)
df_elev_aws_z12$elevation
#compare with USGS data
df2_elev_epqs$elevation

#Determining the correct zoom is a function of the needs of the user and represents a trade off 
#between higher accuracy/longer downloads.

#Get Raster Elevation Data
#Only the Amazon tiles are currently accessible via elevatr.

# sf POLYGON example
data(lake)
elevation <- get_elev_raster(lake, z = 9)
elevation

## Mosaicing & Projecting
## Note: Elevation units are in meters.

plot(elevation)
plot(st_geometry(lake), add = TRUE, col = "blue")

# data.frame example
elevation_df <- get_elev_raster(examp_df, prj = crs_dd, z = 5)

## Mosaicing & Projecting
## Note: Elevation units are in meters.

plot(elevation_df)
plot(examp_sf, add = TRUE, col = "black", pch = 19, max.plot = 1)

# Bounding box on edge
elev_edge <- get_elev_raster(lake, z = 10)

## Mosaicing & Projecting

## Note: Elevation units are in meters.

plot(elev_edge)
plot(st_geometry(lake), add = TRUE, col = "blue")

# Use expand to grab additional tiles
elev_expand <- get_elev_raster(lake, z = 10, expand = 15000)

## Mosaicing & Projecting
## Note: Elevation units are in meters.

plot(elev_expand)
plot(st_geometry(lake), add = TRUE, col = "blue")



lake_buffer <- st_buffer(lake, 1000)

lake_buffer_elev <- get_elev_raster(lake_buffer, z = 9, clip = "locations")

## Mosaicing & Projecting

## Clipping DEM to locations

## Note: Elevation units are in meters.

plot(lake_buffer_elev)
plot(st_geometry(lake), add = TRUE, col = "blue")
plot(st_geometry(lake_buffer), add = TRUE)


library(httr)
# Increase timeout:
get_elev_raster(lake, z = 5, config = timeout(100))


# Increase timeout:
get_elev_raster(lake, z = 5, config = c(verbose(), timeout(5)))

#Access OpenTopography API with get_elev_raster()
#need to get API key

#1. Register with Open topography: https://portal.opentopography.org/newUser
#2. Create an API key: login and request API key
#my blm key: API key:     c926d854ddc1fe80385664dfe73f9d77
#my edu API key:     368954025b8e8e957df405652accfcdc

# elevatr::set_opentopo_key("368954025b8e8e957df405652accfcdc")
# # #need to restart R to take effect (including .Rdata file)
# #since my blm key was used first; I need to try usethis::edit_r_environ().
# 
# usethis::edit_r_environ() #then edit for the new key

#see https://portal.opentopography.org/apidocs/#/Public/getGlobalDem for the source
#note: this is probably a lot more helpful for global work; none appear to be high res.

lake_srtmgl1 <- get_elev_raster(lake, src = "gl1", clip = "bbox", expand = 1000)
plot(lake_srtmgl1)
plot(st_geometry(lake), add = TRUE, col = "blue")


### The 1 m 3DEP data is only available to K-12 and international researchers at this time!!!!
# maybe in the future????
#https://opentopography.org/content/how-access-restricted-data-opentopography

