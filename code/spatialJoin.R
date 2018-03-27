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

borough@data %>% mutate(id = 1:nrow(borough@data)) -> borough@data

# spatial join
inBorough <- over(subway, as(borough, "SpatialPolygons"))

# join back the data from the spatial join.
subway %>%as.data.frame() %>%  mutate(id = inBorough) %>% 
    left_join(borough@data, by=c("id", "id")) -> subway


#write_csv(subway, "Data/subway.csv")

library(tmap)
 tm_shape(borough) + tm_fill() +
     tm_shape(subway) + tm_dots(col = "boro_name") + 
     tm_legend(title = "Boroughs")
