##### This R Script is to plot CalCOFI data in relation to the dumpsite 

# install and/or load the necessary R packages
if("pacman" %in% installed.packages() == FALSE){install.packages("pacman")}
pacman::p_load(oce, ocedata, marmap, ggOceanMaps, ggOceanMapsData, geojsonR, factoextra,sf,dplyr, 
               ggplot2, maps, fields,raster, lubridate, tidyr,ggh4x,RColorBrewer,ggstar, ggsn,measurements) 

if("pacman" %in% installed.packages() == FALSE){install.packages("pacman")}
pacman::p_load(rgeos, rgdal, foreign, plyr, dplyr, usmap, ggplot2, mapdata, raster, 
               maps, stringr, viridis, rgeos, sf, data.table, splitstackshape, cowplot, ) 


##################################################################################################################
################### Read in inital data   ###################
##################################################################################################################

# Download a shapefile to plot the locations of stations 
mex <- readRDS(here::here("data","shapfiles","gadm36_MEX_1_sp.rds"))
us <- readRDS(here::here("data","shapfiles","gadm36_USA_1_sp.rds"))
CA <- us[us$NAME_1 %in% "California",]
CA = rbind(CA,mex)
CA = raster::crop(CA, extent(-121, -116.5, 32, 35))
CA.fortify = fortify(CA)

##################################################################################################################
################### Create Figure 1 ###################
##################################################################################################################

# Get coastlines 
data("coastlineWorldFine")
coast = as.data.frame(coastlineWorldFine@data)

# Get bathymetry 
bathy.raw <- getNOAA.bathy(lon1 = -123.3, lon2 = max(CA.fortify$long),
                           lat1 = 30.0, lat2 = 34.9, resolution = 1)
bathy <- as_tibble(fortify.bathy(bathy.raw))
bathy <- as_tibble(fortify.bathy(bathy.raw)) %>% 
  mutate(depth_bins = cut(z, breaks = c(Inf, 0, -30, -120, -200, -500, -1000, -Inf)))  

# get dumpsite locations 
shapefile <- sf::st_read(here::here("data","shapfiles","dumpsite_locations","Disposal_Sites_14_SCCWRP_1973.shp"))
dumpsite <- as(shapefile, "Spatial")
dumpsite <- spTransform(dumpsite, crs(CA))
dumpsite <- fortify(dumpsite)


station.loc =  read.csv(here::here("data","CalCOFI_data","CalCOFIStationOrder.csv"))

# plot the map 
ggplot() + 
  geom_contour(data = bathy, aes(x = x, y = y, z = z), 
               breaks= c(Inf, 0, -150, -200, -500, -1000, -2000,-4000,-Inf), alpha=0.35, color="lightgray")+
  geom_raster(data = bathy, aes(x = x, y = y, fill = depth_bins), interpolate = TRUE, alpha = 0.35)+
  scale_fill_manual(values = rev(c("white", brewer.pal(8, "Blues"))), guide = "none") + 
  geom_polygon(data=coast, mapping=aes(x = longitude, y = latitude), color="black", fill="gray") + 
  coord_cartesian(ylim = c(31, 34.6), xlim = c(-122.0, -117.2))  + 
  geom_polygon(data=dumpsite, mapping=aes(x = long, y = lat, group=group), color="black", alpha = 0.5) + 
  geom_point(data=station.loc %>% dplyr::filter(Sta < 90 & Line < 95), mapping=aes(x=Lon..dec., y=Lat..dec.))+
  xlab("")+ylab("") 



