# Analysis of UNHCR data


# Set up ------------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(maps)

all_data <- read.csv("data/population.csv", skip = 14)
# Simple Exploration
dim(all_data) # Rows and columns
unique(all_data$Year)
length(unique(all_data$Country.of.origin)) # How many different countries are people leaving?
unique(unique(all_data$Country.of.asylum)) # How many countries are people taking asylum in?


# Basic Questions ---------------------------------------------------------


data <- all_data %>% 
  filter(Year == 2020) %>% 
  select(contains("Country"), Asylum.seekers)
dim(data)

country_of_interest <- "ESP"
country_name <- countrycode(country_of_interest, origin = "country.name", destination = 'iso3c')

country_data <- data %>% 
  filter(Country.of.asylum..ISO. == country_of_interest)


# High Level questions ----------------------------------------------------

# From how many different countries do asylum seekers come from into country of interest
num_countries <- nrow(country_data)

# How many people sought asylum in the country in 2020?
num_people <- country_data %>% 
  summarize(total_people = sum(Asylum.seekers)) %>% 
  pull()

top_10_countries <- country_data %>% 
  top_n(10, wt = Asylum.seekers) %>% 
  arrange(-Asylum.seekers) %>% 
  select(Country.of.origin, Asylum.seekers)


# Mapping and Plotting --------------------------------------------------------

shapefile <- map_data("world")
# Get iso3 codes
shapefile <- shapefile %>% 
  mutate(iso = countrycode(region, origin = 'country.name', destination = 'iso3c')) %>% 
  left_join(country_data, by = "Country.of.origin..ISO.")
  
ggplot(data = shapefile) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group))

  
ggplot(data = shapefile) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = Asylum.seekers)
  ) + 
  labs(title = paste("Number of People Seeking Asylum in", country_name),
       x ="", y ="", fill = "Num. People") +
  theme_minimal()






