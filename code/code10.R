library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(raster)
library(tmap)
library(spData)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'Data')

names(world)
plot(world)

nybb <- read_sf(file.path(dataDir, "gis", "nybb"))
nybb

st_crs(nybb)

centralPark <- data.frame(lon = -73.9691305,
                          lat = 40.7764627) %>% 
    st_as_sf(coords = c("lon", "lat"))
centralPark
st_is_longlat(centralPark)

centralPark_geo <- st_set_crs(centralPark, 4326)
st_is_longlat(centralPark_geo)
centralPark_geo

tm_shape(nybb) + tm_polygons() +
    tm_shape(centralPark_geo) + tm_symbols()

Area_Albers <- 102008
Area_Equidistant <- 102005
Area_Lambert <- 102004
Area_StatePlane <- 2263

projections <- setNames(c(Area_Albers, Area_Equidistant, Area_Lambert, Area_StatePlane),
                        c("Area_Albers", "Area_Equidistant", "Area_Lambert", "Area_StatePlane"))
   
st_transform(nybb, Area_Albers) %>% st_area()

areaReproj <- function(shape, projection){
    st_transform(shape, projection) %>% st_area()
}

areaCalc <- projections %>% as.list() %>% 
    purrr::map_df(~areaReproj(nybb, .x)) %>% 
    mutate_at(vars(Area_Albers, Area_Equidistant, Area_Lambert), function(x) x*10.7639) %>% 
    mutate(BoroName = nybb$BoroName) %>% 
    mutate(AlbersDiff = Area_Albers - Area_Albers,
           LambertDiff = Area_Albers - Area_Lambert,
           EquidistantDiff = Area_Albers - Area_Equidistant,
           stateDiff = Area_Albers - Area_StatePlane)
areaCalc

areaCalc %>% select(5:9) %>% 
    tidyr::gather(projection, difference, - BoroName) %>% 
    ggplot(aes(projection, difference, fill = difference)) +
    geom_col() + 
    coord_flip() + 
 #   facet_wrap(~BoroName, scales = 'free') + 
    theme_minimal()

methods(class = "sf")

st_drop_geometry(nybb) %>% class()
nybb %>% class()

nybb[1,]
nybb[,c(2,4)]

nybb %>% slice(1:2)

nybb %>% select(2)    
nybb %>% select(BoroName)

nybb %>% filter(Shape_Area >= 1937566944)

library(rvest)
demographics <- read_html("https://en.wikipedia.org/wiki/Demographics_of_New_York_City") %>% 
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% 
    html_table(header = FALSE) %>% "[["(1) %>% 
    slice(4:8) %>% 
    mutate(X1 = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")) %>% 
    select(c(1,3:5))
demographics
names(demographics) <- c("BoroName", "pop", "gdp", "gdpPerCapita")

nybbDemo <- nybb %>% left_join(demographics) %>% 
    mutate_at(vars(pop, gdp, gdpPerCapita), function(x)stringr::str_remove_all(x, ",") %>% as.numeric())

plot(nybbDemo["pop"])
plot(nybbDemo["gdp"])
jul14 <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv") 

jul14 %>% glimpse()

uberJuly <- st_as_sf(jul14, coords = c("Lon", "Lat"), crs = 4326)
set.seed(1234)
uberJuly <- st_transform(uberJuly, 2263) %>% sample_frac(0.2)
set.seed(1234)
uberSamp <- uberJuly %>% sample_n(100)

tm_shape(nybb) + tm_polygons(col ="white")+
    tm_shape(uberSamp) + tm_symbols(size = 0.2)

st_crs(nybb) <- 2263
st_intersects(uberSamp, nybb)
st_intersects(uberSamp, nybb, sparse = FALSE)
st_within(uberSamp, nybb)
sel <- st_is_within_distance(uberSamp, nybb, dist = 1000)
lengths(sel) >0

class(sel)
sel_logical <- lengths(sel) > 0
nycUber <- uberSamp[sel_logical,]

nycUber
tm_shape(nybb) + tm_polygons(col = "white") + 
    tm_shape(nycUber) + tm_symbols(size = 0.2)
 
st_crs(nybbDemo) <- 2263
uberJoin <- st_join(nycUber, nybbDemo)

tm_shape(nybb) + tm_polygons(col = "white") + 
    tm_shape(uberJoin) + tm_symbols(size = 0.2, col = "pop", alpha = 0.5)

uberSamp %>% 
    mutate(cost = rnorm(n = 100, mean = 25, sd = 10)) %>% 
    select(cost) %>% 
    aggregate(by = nybb, FUN = median)

nybbCentroids <- st_centroid(nybb)
nybbCentroids

st_distance(uberSamp[1,], nybbCentroids)
st_distance(uberSamp[1:10,], nybbCentroids)

plot(st_geometry(nybbCentroids))
plot(st_geometry(uberSamp)[1:10], add = TRUE, col = "red")

nyc_simp <- st_simplify(nybb,dTolerance = 500)
plot(nybb["BoroName"])

object.size(nybb)
object.size(nyc_simp)
spData::us_states
plot(us_states["NAME"])
usSimp <- st_simplify(us_states, dTolerance = 2)
plot(usSimp["NAME"])
usShape <- rmapshaper::ms_simplify(us_states[,"NAME"], keep = 0.1, keep_shapes = TRUE)
plot(usShape)



subway <- read_sf(file.path(dataDir, "gis", "SubwayLines")) %>% 
    st_transform(2263)
tm_shape(nybb) + tm_polygons(col = "white") + 
    tm_shape(subway) + tm_lines()

subwayRT <- subway %>% pull(rt_symbol) %>% unique()

splitSubway <- subway %>% tidyr::nest(-rt_symbol) %>% 
    mutate(combine = purrr::map(data, st_combine))
subwayClean <- splitSubway$combine %>% purrr::reduce(c) %>% 
    st_sf() %>% mutate(lines = subwayRT)


subwayCentroids <- st_centroid(subwayClean)
subwaySurface <- st_point_on_surface(subwayClean)

tm_shape(nybb %>% filter(BoroName != "Staten Island")) + tm_polygons(col = "white")+
    tm_shape(subwayClean) + tm_lines(col = "lines") +
    tm_shape(subwayCentroids) + tm_symbols(col = "lines", size = 0.5, shape = 3) +
    tm_shape(subwaySurface) + tm_symbols(col = "lines", size = 0.5)

uberSP <- as(uberJuly %>% select(-`Date/Time`, -Base), "Spatial")
uberSP <- spTransform(uberSP, CRS("+init=epsg:2263"))
rast <- raster(crs = CRS("+init=epsg:2263"))

extent(rast) <- extent(uberSP)
ncol(rast) <-  50
nrow(rast) <- 50

uberRast <- rasterize(uberSP, rast, fun = function(x,...)length(x))
plot(uberRast)

uberSamp$density <- raster::extract(uberRast, uberSamp)

tm_shape(nybb) + tm_polygons(col ="white") + 
    tm_shape(uberSamp) + tm_symbols(size = .3, col = "density")
