install.packages("ggmap")
library(ggmap)
register_stadiamaps('a7b0af86-5a7d-4c79-b8c3-4c3acd629413', FALSE)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stadiamap(us, zoom = 5) %>% ggmap()

uk <- c(left = -10, bottom = 49, right = 2, top = 59)
get_stadiamap(uk, zoom = 6, maptype="stamen_toner_lite") %>% ggmap()

sheffield <- c(left = -1.49, bottom = 53.37, right = -1.45, top = 53.39)
get_stadiamap(sheffield, zoom = 15, maptype="stamen_toner_lite") %>% ggmap()


install.packages("tidyverse")
library(tidyverse)
sheffieldCameras<-read_csv("Sheffield_CCTV_Locations.csv")
sheffield <- c(left = -1.49, bottom = 53.37, right = -1.45, top = 53.39)
get_stadiamap(sheffield, zoom = 15, maptype="stamen_toner_lite") %>%
  ggmap() +
  geom_point(data=sheffieldCameras, aes(x=lon, y=lat), colour="red")+
  labs(title="Position of CCTV cameras in Sheffield city centre in 2017",
       caption="Data: Sheffield city council, 2017")
#Execise 1
install.packages("readr")
library(readr)
library(tidyverse)
library(ggmap)

sheffield_cathedral <- c(
  left = -1.472,
  bottom = 53.382,
  right = -1.462,
  top = 53.386
)


map_cathedral <- get_stadiamap(
  bbox = sheffield_cathedral,
  zoom = 16,
  maptype = "stamen_toner_lite"
)

ggmap(map_cathedral)

sheffieldCameras <- read_csv("Sheffield_CCTV_Locations.csv")
head(sheffieldCameras)
ggmap(map_cathedral) +
  geom_point(
    data = sheffieldCameras,
    aes(x = lon, y = lat),
    colour = "red",
    size = 2
  ) +
  labs(
    title = "CCTV Cameras around Sheffield Cathedral (2017)",
    caption = "Data: Sheffield City Council, 2017"
  )

cathedral_cameras <- sheffieldCameras %>%
  filter(lon > -1.472, lon < -1.462, lat > 53.382, lat < 53.386)

ggmap(map_cathedral) +
  geom_point(
    data = cathedral_cameras,
    aes(x = lon, y = lat),
    colour = "red",
    size = 2
  ) +
  labs(
    title = "CCTV Cameras near Sheffield Cathedral (2017)",
    caption = "Data: Sheffield City Council, 2017"
  )
#End of Execise

install.packages("osmdata")
library(osmdata)
SCC_pubs_restaurants <- getbb("Sheffield city centre")%>%
  opq()%>%
  add_osm_feature(key = "amenity",
                  value = c("restaurant", "pub")) %>%
  osmdata_sf()

View(SCC_pubs_restaurants$osm_points)

install.packages("sf")
library(sf)
base_map <- get_stadiamap(getbb("Sheffield City Centre"), source = "stadia")
ggmap(base_map)+
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          inherit.aes = FALSE,
          colour = "blue",
          fill = "blue",
          alpha = .5,
          size = 1,
          shape = 21)+
  labs(x = "", y = "")

SCC_highways <- getbb("Sheffield city centre")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()
SCC_streets <-getbb("Sheffield city centre")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()
SCC_small_streets <-getbb("Sheffield city centre")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()
SCC_river <- getbb("Sheffield city centre")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()
SCC_railway <- getbb("Sheffield city centre")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

View(SCC_railway$osm_lines)
ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-1.49,-1.45),
           ylim = c(53.36, 53.39),
           expand = FALSE)

ggplot() +
  geom_sf(data = SCC_river$osm_lines, inherit.aes
          = FALSE,
          color = "steelblue", size =
            .8,
          alpha = .3) +
  geom_sf(data = SCC_railway$osm_lines,
          inherit.aes = FALSE,
          color = "black", size =
            .2, linetype="dotdash",
          alpha = .5) +
  geom_sf(data = SCC_streets$osm_lines, inherit.aes =
            FALSE,
          color = "black", size
          = .3,
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines, inherit.aes = FALSE,
          color = "#666666",
          size = .2, alpha =
            .3) +
  geom_sf(data = SCC_highways$osm_lines,
          inherit.aes = FALSE,
          color = "black", size =
            .5,
          alpha = .6) +
  geom_sf(data = SCC_pubs_restaurants$osm_points, inherit.aes =
            FALSE,
          colour = "blue", fill =
            "blue", alpha = .5,
          size = 1,
          shape = 21)+
  theme_void() +
  coord_sf(xlim = c(-1.49,-1.45), ylim =
             c(53.36, 53.39),
           expand = FALSE)+
  theme()+ labs(title = "Restaurants and pubs in Sheffield City Center")

ggplot() +
  geom_sf(data = SCC_river$osm_lines, inherit.aes =
            FALSE,
          color = "steelblue",
          size = .8, alpha =
            .3) +
  geom_sf(data = SCC_railway$osm_lines, inherit.aes =
            FALSE,
          color = "black", size = .2,
          linetype="dotdash", alpha =
            .5) +
  geom_sf(data = SCC_streets$osm_lines, inherit.aes = FALSE,
          color = "black", size =
            .3,
          alpha = .5) +
  geom_sf(data = SCC_small_streets$osm_lines, inherit.aes = FALSE,
          color = "#666666", size =
            .2) +
  geom_sf(data = SCC_highways$osm_lines, inherit.aes = FALSE,
          color = "black", size = .5,
          alpha = .6) +
  geom_sf(data = SCC_pubs_restaurants$osm_points, inherit.aes = FALSE,
          colour = "blue", fill =
            "blue", alpha = .5,
          size = 1,
          shape = 21)+ theme_void() +
  geom_point(data=sheffieldCameras,colour="red",aes(x=lon,y=lat))+ coord_sf(xlim = c(-1.49,-1.45),
                                                                            ylim = c(53.36, 53.39),
                                                                            expand = FALSE)+
  theme(legend.position = "none")+ labs(title = "CCTV coverage, restaurants, pubs and in Sheffield City Center")

#Execise 2
options(timeout = 120)
set_overpass_url("https://overpass.kumi.systems/api/interpreter")

SCC_bbox <- matrix(c(-1.49, 53.36, -1.45, 53.39),
                   ncol = 2,
                   dimnames = list(c("x", "y"), c("min", "max")))

SCC_police <- opq(bbox = SCC_bbox) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf()

SCC_pubs_restaurants <- opq(bbox = SCC_bbox) %>%
  add_osm_feature(key = "amenity", value = c("restaurant", "pub")) %>%
  osmdata_sf()

SCC_river <- opq(bbox = SCC_bbox) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

SCC_streets <- opq(bbox = SCC_bbox) %>%
  add_osm_feature(key = "highway",
                  value = c("primary", "secondary", "tertiary",
                            "residential", "service")) %>%
  osmdata_sf()

sheffieldCameras <- read_csv("Sheffield_CCTV_Locations.csv")

crimes <- read_csv("2025-09-south-yorkshire-street.csv")

crimes_SCC <- crimes %>%
  filter(Longitude > -1.49, Longitude < -1.45,
         Latitude > 53.36, Latitude < 53.39)

ggplot() +
  geom_sf(data = SCC_river$osm_lines,
          color = "steelblue", size = 0.8, alpha = 0.3) +
  geom_sf(data = SCC_streets$osm_lines,
          color = "#888888", size = 0.3, alpha = 0.5) +
  geom_sf(data = SCC_pubs_restaurants$osm_points,
          colour = "blue", fill = "blue", alpha = 0.5, size = 1, shape = 21) +
  geom_sf(data = SCC_police$osm_points,
          colour = "green", fill = "green", alpha = 0.8, size = 2, shape = 23) +
  geom_point(data = sheffieldCameras,
             aes(x = lon, y = lat),
             colour = "red", alpha = 0.8, size = 1) +
  geom_point(data = crimes_SCC,
             aes(x = Longitude, y = Latitude),
             colour = "purple", alpha = 0.4, size = 0.7) +
  coord_sf(xlim = c(-1.49, -1.45), ylim = c(53.36, 53.39), expand = FALSE) +
  theme_void() +
  labs(
    title = "CCTV, Police Stations, and Crime Incidents in Sheffield City Centre (Nov 2020)",
    caption = "Data: OSM, Sheffield City Council, data.police.uk"
  )

#End of Execise

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")+
  theme(panel.background = element_blank())+
  labs(title = "World map", caption="maps package, R")

eu.countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany","Greece", "Hungary", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"
)
eu.map <- map_data("world", region = eu.countries)
ggplot(eu.map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgrey", colour="black")+
  labs(title = "EU map", caption="maps package, R")+
  coord_map()

library(WDI)
new_wdi_cache<-WDIcache()
countryDataWDI<-WDI(indicator=c("NY.GDP.PCAP.KD",
                                "NY.GDP.PCAP.KD.ZG",
                                "SP.POP.TOTL",
                                "SP.DYN.LE00.IN"),
                    start=2019,
                    end=2019,
                    extra= TRUE,
                    cache=new_wdi_cache)

countryDataWDI <- countryDataWDI %>%
  mutate(country=recode(str_trim(country),"United States"="USA",
                        "United Kingdom"="UK"))

countryDataWDIMap <- left_join(world_map,countryDataWDI,by = c("region"="country"))
ggplot(countryDataWDIMap,aes(long,lat,group=group))+
  geom_polygon(aes(fill=NY.GDP.PCAP.KD),colour="white")+
  scale_fill_viridis_c()+
  theme_void()+
  labs(fill="GDP per \ncapita growth",
       title="World map coloured by GDP per capita growth in 2019",
       caption="Data source: World Development Indicators")

#Execise 3
world_map <- map_data("world")
countryDataWDI <- WDI(indicator = "SP.POP.TOTL", start = 2020, end = 2020)

world_map_WDI <- left_join(world_map, countryDataWDI, by = c("region" = "country"))

diff <- setdiff(countryDataWDI$country, world_map$region)
View(diff)

countryDataWDI$country <- recode(countryDataWDI$country,
                                 "United States" = "USA",
                                 "Russian Federation" = "Russia",
                                 "Egypt, Arab Rep." = "Egypt",
                                 "Iran, Islamic Rep." = "Iran",
                                 "Venezuela, RB" = "Venezuela",
                                 "Yemen, Rep." = "Yemen",
                                 "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                                 "Congo, Rep." = "Republic of Congo",
                                 "Gambia, The" = "Gambia",
                                 "Kyrgyz Republic" = "Kyrgyzstan",
                                 "Lao PDR" = "Laos",
                                 "Syrian Arab Republic" = "Syria",
                                 "Slovak Republic" = "Slovakia",
                                 "Bahamas, The" = "Bahamas",
                                 "West Bank and Gaza" = "Palestine"
)

diff <- setdiff(countryDataWDI$country, world_map$region)
length(diff)

world_map_left  <- left_join(world_map, countryDataWDI, by = c("region" = "country"))
world_map_inner <- inner_join(world_map, countryDataWDI, by = c("region" = "country"))

ggplot(world_map_left, aes(long, lat, group = group, fill = SP.POP.TOTL)) +
  geom_polygon() + theme_void() + labs(title = "Left Join Map")

ggplot(world_map_inner, aes(long, lat, group = group, fill = SP.POP.TOTL)) +
  geom_polygon() + theme_void() + labs(title = "Inner Join Map")

# GDP per capita (current US$)
gdp <- WDI(indicator = "NY.GDP.PCAP.CD", start = 2020, end = 2020)

# Life expectancy at birth
lifeexp <- WDI(indicator = "SP.DYN.LE00.IN", start = 2020, end = 2020)

world_map_gdp <- left_join(world_map, gdp, by = c("region" = "country"))

#End of Execise