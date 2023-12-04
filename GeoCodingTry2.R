require(tidyverse)
require(sf)
require(emojifont)

# Load required packages
library(tmaptools)
library(tmap)
library(geojsonsf)
library(ggthemes)
library(ggrepel) # For geom_text_repel
library(emojifont)

get_congress_map <- function(cong=73) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}

cd73 <- get_congress_map(73)

cd73_district <- 
  cd73 |>
  #filter(DISTRICT != "0") %>%
  mutate(DISTRICT = as.character(DISTRICT)) %>%
  select(DISTRICT)

cd73_district_ne <- cd73 %>% 
  filter((STATENAME == "New Jersey" |
            STATENAME == "Connecticut" | 
            STATENAME == "Massachusetts" | 
            STATENAME == "Maine" | 
            STATENAME == "Pennsylvania" | 
            STATENAME == "New Hampshire" | 
            STATENAME == "Vermont"| 
            STATENAME == "Rhode Island") |
           (STATENAME == "New York" & DISTRICT != "0")) %>%
  mutate(DISTRICT = as.character(DISTRICT)) %>%
  select(STATENAME, DISTRICT)

list_of_nestate <-
  c("New Jersey","New York","Connecticut","Massachusetts", "Maine",
    "Pennsylvania","New Hampshire", "Vermont", "Rhode Island")

cd73_district_ne <- 
  cd73 |>
  filter(STATENAME %in% list_of_nestate) |>
  mutate(DISTRICT = as.character(DISTRICT)) %>%
  select(DISTRICT)

cd73_district_mass <- cd73 %>%
  filter(STATENAME == "Massachusetts") %>%
  filter(DISTRICT != "0") %>%
  mutate(DISTRICT = as.character(DISTRICT)) %>%
  select(DISTRICT)

cd73_district_cali <- cd73 %>%
  filter(STATENAME == "California") %>%
  filter(DISTRICT != "0") %>%
  filter(DISTRICT == "3" | DISTRICT == "4" | DISTRICT == "5" | DISTRICT == "6" | DISTRICT == "8") %>%
  mutate(DISTRICT = as.character(DISTRICT)) %>%
  select(DISTRICT)

# Define all addresses
addresses <- c("700 Washington St, Bath, ME 04530", 
               "395 Bailey Ave, Kittery, ME 03904", 
               "75 Eastern Point Rd, Groton, CT 06340", 
               "4101 Washington Ave, Newport News, VA 23607", 
               "97 E Howard St, Quincy, MA 02169", 
               "307 Holyoke St, San Francisco, CA 94134", 
               "3075 Richmond Terrace, Staten Island, NY 10303", 
               "1 Eastern Road, Kearny, NJ 07032", 
               "2570 Broadway, Camden, NJ 08104", 
               "2700 Portsmouth Blvd, Portsmouth, VA 23704", 
               "Joint Base Pearl Harbor-Hickam, HI 96818", 
               "1400 Farragut St., Bremerton, WA 98314", 
               "7 Moulton St, Charlestown, MA 02129", 
               "2234 S Hobson Ave, North Charleston, SC 29405", 
               "1180 Nimitz Ave, Vallejo, CA 94592", 
               "63 Flushing Ave, Brooklyn, NY 11205", 
               "2100 Kitty Hawk Ave, Philadelphia, PA 19112")

placelabels_all <- c("Bath Iron Works",
"Electric Boat Company",
"Newport News Shipbuilding",
"Bethlehem Quincy",
"Bethlehem San Francisco",
"Bethlehem Staten Island",
"Federal Shipbuilding",
"New York Shipbuilding",
"Norfolk NSY",
"Pearl Harbor NSY",
"Portsmouth NSY",
"Puget Sound NSY",
"Boston NSY",
"Charleston NSY",
"Mare Island NSY",
"New York NSY",
"Philadelphia NSY")
  
placelabels_mass <- c("Bethlehem Quincy",
                     "Boston NSY")

placelabels_ne<- c("Bath Iron Works",
                   "Electric Boat Company",
                   "Bethlehem Quincy",
                   "Bethlehem Staten Island",
                   "Federal Shipbuilding",
                   "New York Shipbuilding",
                   "Portsmouth NSY",
                   "Boston NSY",
                   "New York NSY",
                   "Philadelphia NSY")

placelabels_cali <- c("Bethlehem San Francisco", 
                      "Mare Island NSY")


placelabels_ne_limited<- c("Bath Iron Works",
                   "Electric Boat Company",
                   "Bethlehem Quincy",
                   "Bethlehem Staten Island",
                   "Federal Shipbuilding",
                   "New York Shipbuilding",
                   "Portsmouth NSY",
                   "Boston NSY",
                   "New York NSY",
                   "Philadelphia NSY")

addresses_cali <- c("307 Holyoke St, San Francisco, CA 94134", 
                    "1180 Nimitz Ave, Vallejo, CA 94592")
                      

addresses_northeast<- c("700 Washington St, Bath, ME 04530", 
                        "395 Bailey Ave, Kittery, ME 03904", 
                        "75 Eastern Point Rd, Groton, CT 06340", 
                        "97 E Howard St, Quincy, MA 02169", 
                        "3075 Richmond Terrace, Staten Island, NY 10303", 
                        "1 Eastern Road, Kearny, NJ 07032", 
                        "2570 Broadway, Camden, NJ 08104", 
                        "7 Moulton St, Charlestown, MA 02129", 
                        "63 Flushing Ave, Brooklyn, NY 11205", 
                        "2100 Kitty Hawk Ave, Philadelphia, PA 19112")

addresses_northeast_limited <- c("700 Washington St, Bath, ME 04530", 
                        "395 Bailey Ave, Kittery, ME 03904", 
                        "75 Eastern Point Rd, Groton, CT 06340", 
                        "97 E Howard St, Quincy, MA 02169", 
                        "3075 Richmond Terrace, Staten Island, NY 10303", 
                        "1 Eastern Road, Kearny, NJ 07032", 
                        "2570 Broadway, Camden, NJ 08104", 
                        "7 Moulton St, Charlestown, MA 02129", 
                        "63 Flushing Ave, Brooklyn, NY 11205", 
                        "2100 Kitty Hawk Ave, Philadelphia, PA 19112")

addresses_mass <- c("97 E Howard St, Quincy, MA 02169", 
                    "7 Moulton St, Charlestown, MA 02129")

# Geocode the addresses using OSM
geocoded_addresses <- geocode_OSM(addresses)
geocoded_addresses_ne <- geocode_OSM(addresses_northeast)
geocoded_addresses_mass <- geocode_OSM(addresses_mass)
geocoded_addresses_cali <- geocode_OSM(addresses_cali)

all_shipyards <- data.frame(name = placelabels_all, 
                            x= geocoded_addresses$lon, 
                            y = geocoded_addresses$lat) %>% 
                            st_as_sf(coords = c("x", "y"), crs = 4269) 


mass_shipyards <- data.frame(name = placelabels_mass, 
                         x= geocoded_addresses_mass$lon, 
                         y = geocoded_addresses_mass$lat) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4269) 


ne_shipyards <- data.frame(name = placelabels_ne, 
                           x= geocoded_addresses_ne$lon, 
                           y = geocoded_addresses_ne$lat) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4269) 

cali_shipyards <- data.frame(name = placelabels_cali, 
                               x= geocoded_addresses_cali$lon, 
                               y = geocoded_addresses_cali$lat) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4269) 


#Overall Map w Shipyard labels 
cd73_district %>%
  ggplot() +
  geom_sf() +
  geom_point(data = geocoded_addresses, aes(x = lon, y = lat), color = "red") +
  geom_sf_text(data = all_shipyards, aes(label = name)) + 
  labs(title = "USN Shipyards") +
  theme_map()

#Mass Buffer
buffer_distance <- 6500 #this approximates 4 miles on the map
buffer_mass <- st_buffer(mass_shipyards, buffer_distance)
cd73_district_mass$close2yard <- ifelse(sf::st_intersects(cd73_district_mass, buffer_mass, sparse = F),"Yes","No")
cd73_district_mass$close2yard <- apply(cd73_district_mass$close2yard, 1, function(x) ifelse("Yes" %in% x, "Yes", "No"))

 #mass Map w Shipyard labels 
cd73_district_mass %>%
  ggplot() +
  geom_sf(data = cd73_district_mass, aes(fill = close2yard)) +
  geom_point(data = geocoded_addresses_mass, aes(x = lon, y = lat), color = "black") +
  geom_sf_label(data = mass_shipyards, aes(label = name), nudge_x = 0.55, nudge_y = 0.05) + 
  geom_sf(data = buffer_mass, fill = NA, color = "black") +
  geom_label_repel(data = cd73_district_mass %>% filter(close2yard == "Yes"),
                   aes(label = DISTRICT,
                       x = st_coordinates(st_centroid(geometry))[ , 1],
                       y = st_coordinates(st_centroid(geometry))[ , 2]),
                   color = "black", size = 3) +
  labs(title = "USN Contracted Shipyards in Massachussets, 1930-1939", fill = "Commuting Distance to a Shipyard?") +
  theme_map()

#Cali Buffer
buffer_distance <- 6500 #this approximates 4 miles on the map
buffer_cali <- st_buffer(cali_shipyards, buffer_distance)
cd73_district_cali$close2yard <- ifelse(sf::st_intersects(cd73_district_cali, buffer_cali, sparse = F),"Yes","No")
cd73_district_cali$close2yard <- apply(cd73_district_cali$close2yard, 1, function(x) ifelse("Yes" %in% x, "Yes", "No"))

#testing private/public shipyard buffer for Cali
#buffer_cali_private <- st_buffer(cali_shipyards[1,], buffer_distance)
#cd73_district_cali$close2yard_private <- ifelse(sf::st_intersects(cd73_district_cali, buffer_cali_private, sparse = F),"Yes","No")
#cd73_district_cali$close2yard_private <- apply(cd73_district_cali$close2yard_private, 1, function(x) ifelse("Yes" %in% x, "Yes", "No"))


## Cali Map
cd73_district_cali %>%
  ggplot() +
  geom_sf(data = cd73_district_cali) +
  geom_point(data = geocoded_addresses_cali, aes(x = lon, y = lat-.04), color = "navy", size = 7) +
  geom_text(data = cali_shipyards, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], label = emojifont::emoji("anchor")), family = "EmojiOne", size = 6, color =  "white")+
  geom_sf_label(data = cali_shipyards, aes(label = name), nudge_x = .7, nudge_y = 0.07) + 
  geom_label_repel(data = cd73_district_cali %>% filter(close2yard == "Yes"),
                   aes(label = DISTRICT,
                       x = st_coordinates(st_centroid(geometry))[ , 1],
                       y = st_coordinates(st_centroid(geometry))[ , 2]),
                   color = "black", size = 3) +
  labs(title = "USN Contracted Shipyards in California, 1930-1939", fill = "Commuting Distance to a Shipyard?") +
  theme_map()


#geom_label_repel(data = cd73_district_cali %>% filter(close2yard == "Yes"),
                 aes(label = DISTRICT,
                     x = st_coordinates(st_centroid(geometry))[ , 1],
                     y = st_coordinates(st_centroid(geometry))[ , 2]),
                 color = "black", size = 3) +

  
  
  
##running the buffer on whole NE
#NE Buffer
buffer_distance <- 6500 #this approximates 4 miles on the map
buffer_ne <- st_buffer(ne_shipyards, buffer_distance)
cd73_district_ne$close2yard <- ifelse(sf::st_intersects(cd73_district_ne, buffer_ne, sparse = F),"Yes","No")
cd73_district_ne$close2yard <- apply(cd73_district_ne$close2yard, 1, function(x) ifelse("Yes" %in% x, "Yes", "No"))
#cd73_district_ne$close2yard <- ifelse(cd73_district_ne$close2yard[,1] == "Yes" | cd73_district_ne$close2yard[,2] == "Yes", "Yes", "No")

## NE Map with Shipyard Location + Districts
cd73_district_ne %>%
  ggplot() +
  geom_sf(data = cd73_district_ne, aes(fill = close2yard)) +
  geom_point(data = geocoded_addresses_ne, aes(x = lon, y = lat), size = 2, color = "black") +
  labs(title = "Congressional Districts and Shipyards in the Northeast, 1930-1939", fill = "Commuting Distance to Shipyard?") +
  theme_map()


### MAKING A NE LIMITED SHOT


######## FINDING THE POLITICIANS ########
#List of All Shipyard-Connected Districts
cd73_district_statename<- 
  cd73 |>
  mutate(DISTRICT = as.character(DISTRICT)) %>%
  select(STATENAME, DISTRICT)

##running the buffer on whole country
buffer_distance <- 6500 #this approximates 4 miles on the map
buffer_all <- st_buffer(all_shipyards, buffer_distance)
cd73_district_statename$close2yard <- ifelse(sf::st_intersects(cd73_district_statename, buffer_all, sparse = F),"Yes","No")
cd73_district_statename$close2yard <- apply(cd73_district_statename$close2yard, 1, function(x) ifelse("Yes" %in% x, "Yes", "No"))

#map for reasonability check
cd73_district_statename %>%
  ggplot() +
  geom_sf(data = cd73_district_statename, aes(fill = close2yard)) +
  geom_point(data = geocoded_addresses, aes(x = lon, y = lat), size = 2, color = "black") +
  labs(title = "Congressional Districts Connected to Shipyards in the US, 1930-1939", fill = "Interest in Shipyard?") +
  theme_map()

vinsontrammel_senate_yeas_names <- c("Adams", "Ashurst", "Austin", "Bachman", "Bailey", "Bankhead", "Barbour", "Barkley", "Bone", "Brown", "Bulkley", "Byrd", "Byrnes", "Caraway", "Carey", "Connally", "Coolidge", "Cutting", "Kean", "Davis", "Keyes", "Dill", "Lewis", "Duffy", "Logan", "Fess", "Lonergan", "Fletcher", "McAdoo", "George", "McCarran", "Gibson", "McGill", "Goldsborough", "McKellar", "Hale", "Neely", "Harrison", "O'Mahoney", "Hastings", "Overton", "Hatch", "Patterson", "Hatfleld", "Pittman", "Hayden", "Reed", "Hebert", "Reynolds", "Johnson", "Robinson (Ark)", "Robinson (Ind)", "Russell", "Schall", "Sheppard", "Steiger", "Townsend", "Trammell", "Tydings", "Vandenberg", "Van Nuys", "Wagner", "Walcott", "Walsh", "White")

#### TRYING W SECOND VINSON VOTE ####
#Data Source for Votes: https://voteview.com/rollcall/RH0750115
library("readxl")

# Read xlsx file with data from voteview (manipulated to get district info also)
secondvinson_votes = read_excel("~/Desktop/Thesis Work/Congressional Records Research/2nd Vinson Act/secondvinsonvoteview.xlsx", sheet='Vote Matrix')
secondvinson_votes <- secondvinson_votes %>%
  mutate(DISTRICT = as.character(DISTRICT))

# Join the data frames to get geo into contact w votes
secondvinson_geometry <- left_join(secondvinson_votes, cd73_district_statename, by = c("STATENAME", "DISTRICT"))

secondvinson_geometry %>%
  ggplot() +
  geom_sf(data = secondvinson_geometry, aes(fill = VOTE)) +
  geom_point(data = geocoded_addresses, aes(x = lon, y = lat), size = 2, color = "black") +
  labs(title = "Vote on Second Vinson Act", fill = "Yea, Nea, Abstain") +
  theme_map()

glimpse(secondvinson_geometry)
glimpse(cd73_district_statename)

require("writexl")
library("writexl")
write_xlsx(secondvinson_geometry, "~/Desktop/Thesis Work/secondvinsongeometry.xlsx")
write_xlsx(cd73_district_statename, "~/Desktop/Thesis Work/cd73_district_statename.xlsx")
