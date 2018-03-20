library(rgdal)
library(maps)
library(raster)

#create coordinates
subway <- read_csv("Data/subway.csv")
# create coordinates
coordinates(subway) <- c("Long", "Lat")
plot(subway)
# read in shapefile
borough <- shapefile("Data/BoroughBoundaries/geo_export_1982dc8b-d741-4211-8549-4d10d2fc3651.shp")
# project subway with borough
proj4string(subway) <- proj4string(borough)

# spatial join
inBorough <- over(subway, as(borough, "SpatialPolygons"))

subway %>%as.data.frame() %>%  mutate(borough = inBorough) -> subway

subway %>% mutate(borough = if_else(borough ==1, "StatenIsland", if_else(borough == 2, "Manhattan", 
                                                                      if_else(borough == 5, "Bronx",
                                                                              if_else( borough == 4, "Queens", "Brooklyn"))))) -> subway

?over
plot(subway)

#write_csv(subway, "Data/subway.csv")
