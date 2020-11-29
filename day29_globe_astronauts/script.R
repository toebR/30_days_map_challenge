#30daysmapchallenge day 29: globes
#November 2020
#script by Tobias Stalder
#tobias-stalder.netlify.app


# load libraries ----------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rworldmap)
library(patchwork)
library(Cairo)
library(extrafont)

loadfonts(device = "win")


# load data ---------------------------------------------------------------

#NASA astronauts database data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#country polygons
world <- ne_countries(scale = "medium", returnclass = "sf")


# data manipulation -------------------------------------------------------

#1 Extract astronaut's country of origin from data including the count
countries = data.frame(table(astronauts$nationality))


#2 change the country expression
countries$Var1 = gsub("Czechoslovakia", "Slovakia", countries$Var1)
countries$Var1 = gsub("Hungry", "Hungary", countries$Var1)
countries$Var1 = gsub("Malysia", "Malaysia", countries$Var1)
countries$Var1 = gsub("Netherland", "Netherlands", countries$Var1)
countries$Var1 = gsub("U.S.S.R/Russia", "Russia", countries$Var1)
countries$Var1 = gsub("U.S.S.R/Ukraine", "Ukraine", countries$Var1)
countries$Var1 = gsub("Korea", "South Korea", countries$Var1)
countries$Var1 = gsub("Republic of South Africa", "South Africa", countries$Var1)
countries$Var1 = gsub("U.K.", "United Kingdom", countries$Var1)
countries$Var1 = gsub("U.S.", "United States of America", countries$Var1)
countries$Var1 = gsub("UAE", "United Arab Emirates", countries$Var1)


#3 filter for double citizenship (we cannot geocode that)
countries = countries %>%
  filter(Var1 != "United Kingdom/United States of America")


#4 define better colnames
colnames(countries) = c("ctry_name", "nr_of_astros")


#5 get worldmap
wmap <- getMap(resolution="high")

#6 get centroids per country polygon
centroids <- gCentroid(wmap, byid=TRUE)

#7 define df with centroids including x and y coordinate
df <- data.frame(centroids)

d <- cbind(rownames(df), data.frame(df, row.names=NULL))
colnames(d) = c("ctry_name", "x", "y")
#now we have a dataframe with longitude and latitude

#8 join df on countries
countries_geo <- left_join(countries, d,
                           by = "ctry_name")

#convert df to sf object
sf::st_as_sf(countries_geo, coords = c("x","y")) -> countries_geo_sf

#change crs
st_crs(countries_geo_sf) = 4326



# dataviz -----------------------------------------------------------------
#make a globe where we see point pattern of countries of origin


ggplot() +
  
  #add country polygons
  geom_sf(data = world, fill = "#FECEAB", color = "#2A363B", size = .2) +
  
  #add point centroid locations
  geom_sf(data = countries_geo_sf, color = "black", size = 3, alpha = .5) +
  geom_sf(data = countries_geo_sf, color = "#99B898", size = 1, alpha = .75) +

  #add title, subtitle, caption
  labs(subtitle = "Astronaut's Countries of Origin\n",
       title = "\nHUMANS IN SPACE",
       caption = paste("\n30 Days Map Challange | Day 29: Globes",
                       "Map by Tobias Stalder",
                       "tobias-stalder.netlify.app",
                       "Source: NASA Astronaut Database",
                       sep = "\n"))+
  
  #get globe projection, set to northpole and europe
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  
  #theming
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#2A363B",
                                       color ="#2A363B"),
        panel.grid = element_line(color = "#99B898"),
        plot.title = element_text(colour = "#99B898",
                                  hjust = .5,
                                  family = "Bell MT",
                                  size = 20),
        plot.subtitle = element_text(colour ="#99BB98",
                                     hjust = .5,
                                     family = "Verdana"),
        plot.caption = element_text(colour ="#99BB98",
                                    hjust = .5,
                                    family = "Verdana",
                                    size = 6),
        panel.grid.major = element_line(size = .1)) -> map

#save plot
ggsave(map, filename = "globe.png", width = 14.8, height = 19, units = "cm", type = "cairo-png",
       path = r"(C:\Users\tobia\Desktop\ownprojects\dataviz\globe_astronauts)", dpi = 300)
