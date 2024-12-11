
#install.packages("sf")
#install.packages("tibble")
#install.packages("dplyr")
#install.packages("leaflet")
#install.packages("ggplot2")
#install.packages("rnaturalearth")
#install.packages("usmap")
#install.packages("mapdata")
#install.packages("maps")
#install.packages("viridis")
#install.packages("spData")

library(sf)
library(tibble)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rnaturalearth)
library(usmap)
library(mapdata)
library(maps)
library(viridis)
library(spData)

#WASTEWATER DATA for NY
#from https://data.ny.gov/Energy-Environment/Wastewater-Treatment-Plants/2v6p-juki/about_data
data_water <- read.csv("data/Wastewater_Treatment_Plants_20241205.csv")
data_water_NY_sf <- data_water %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#Load in border data
#Load in US States data
data(world)
data(us_states)
US_outline <- world %>%
  filter(name_long == "United States") %>%
  st_transform(crs = 4326)

New_York <- us_states %>%
  filter(NAME == "New York") %>%
  st_transform(crs = 4326)

#Plot
  #Plot all facilities by type
  ggplot(data_water_NY_sf) +
  geom_sf(aes(color = `Plant.Type`))

#Filter to largest facilities
  #filter to plants about the mean flow
  summary(data_water_NY_sf$Average.Design.Hydraulic.Flow)

  data_water_NY_above_mean <-
    filter(data_water_NY_sf, Average.Design.Hydraulic.Flow > 13.89)

  #Plot largest facilities
  ggplot(data_water_NY_above_mean) +
    geom_sf(aes(color = `Plant.Type`))

   ggplot(data_water_NY_above_mean) +
        geom_sf(aes(color = `Plant.Type`)) +
      geom_sf(data = New_York, fill = NA)

#CARBON DATA
data <- read.csv("data/ghgp_data_2023_NY.csv")

#Clean up dataset for Entire US
  #change column names and delete NA rows
  colnames <- data[3,]
  data2 <- data[-(1:3),]
  colnames(data2) = colnames

  #Filter out empty rows, and empty long/lat rows
  data_US <- data2[1:6470, ] %>%
   filter(!is.na(Longitude) & !is.na(Latitude)) #too many points

    #check for missing values
    #MISSING_COORD <- is.na(data3$Longitude)
                 is.na(data3$Latitude)
  #sum(MISSING_COORD$TRUE) #0

  #Transform to shapefile with geometry
  data_US_sf <- st_as_sf(data3, coords = c("Longitude", "Latitude"), crs = 4326)

  #filter by emissions type (pure carbon sources)
  data_US_pure_carbon <- data_US_sf %>%
   filter(`Industry Type (subparts)`== "P" | `Industry Type (subparts)`== "G" )
      #no ammonia plants in US?

  ggplot(data_US_pure_carbon) +
    geom_sf() +
    geom_sf(data = US_outline, fill = NA)
      #aes(color = "Industry Type (sectors)"),
            size = "Total reported direct emissions") +
    theme(legend.position = "none")

ggplot() +
  geom_sf(data = US_outline, fill = NA) +
  geom_sf(data = data_US_pure_carbon,
          aes(color = "Industry Type (sectors)",
              size = "Total reported direct emissions")) +
  guides(size = FALSE) +
  labs(title = "")


#Filter dataset to only NY
  #convert data to a sf object
  data_NY <- filter(data2, State == "NY")

  #str(data_NY_sf) #check that is shapefile, coordinate system, bounding box
  data_NY_sf <- st_as_sf(data_NY, coords=c("Longitude", "Latitude"), crs = 4326)

    #filter NY to high purity carbon sources
  data_NY_pure_carbon <- data_NY_sf %>%
    filter(`Industry Type (subparts)`== "P" |
             `Industry Type (subparts)`== "G" |
             `Industry Type (subparts)`== "C,W-NGTC")

#Maps of point data
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = data_NY_sf, color = "blue") %>%
  circleOptions.radius =
  addCircles(data = data_NY_sf, color = "red", radius = "Total reported direct emissions")
  setView(lng = -76.0, lat = 42.0, zoom = 5.5)



states_plot <-
  ggplot(data = states, aes(x=long, y=lat, fill = region, group=group)) +
  geom_polygon(color = "white") +
  guides(fill=FALSE) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  ggtitle('U.S. Map with States') +
  coord_fixed(1.3) +
  theme_minimal()

#Map all of carbon emissions in NY
ggplot() +
  geom_sf(data = New_York, fill = NA) +
  geom_sf(data = data_NY_sf, aes(color = `Industry Type (sectors)`,
              size = `Total reported direct emissions`)) +
  guides(size = FALSE)

#Map of pure carbon emissions in NY
ggplot() +
  geom_sf(data = New_York, fill = NA) +
  geom_sf(data = data_NY_pure_carbon,
          aes(color = `Industry Type (sectors)`,
          size = `Total reported direct emissions`)) +
  guides(size = FALSE) +
  geom_sf(data = data_water_NY_above_mean,
          aes(color = "Plant.Type")) +
  scale_fill_manual(name = "Industry Type",
                    labels = c("Petroleum and Natural Gas Systems: High Purity Source",
                               "High Volume Wastewater Facilities")) +
  labs()



#ggplot(data_NY_sf) +
  geom_histogram(aes(x = ))

#ggplot(data_US_sf) +
  geom_histogram(aes(x = State, y = `Total reported direct emissions`))

B <- ggplot(data_US_sf) +
  geom_sf(aes(color = `Industry Type (sectors)`,
              size = `Total reported direct emissions`)) +
  theme(legend.position = "none")


#Make Histograms of emission type per state, and by US







