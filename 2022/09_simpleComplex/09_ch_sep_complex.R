# ----------------------------------------- #
# ---- SWD - Challenge: September 2022 ---- #
# ----------------------------------------- #


## Setup
pacman::p_load(crimedata, tidyverse, lubridate,
               osmdata, ggmap, sf, scatterpie)

## Data
data("homicides15")

## Prepare data
dt <- homicides15 %>%
    
    # Filter rows
    filter(grepl("New York", city_name)) %>%
    
    # Select columns
    select(city_name, offense_type, date_single, longitude, latitude) %>%
    
    # Round coordinates
    mutate(longitude = round(longitude, 1),
           latitude = round(latitude, 1)) %>%
    
    # Add category for daytime
    mutate(daytime = case_when(
        hour(date_single) >= 8 & hour(date_single) < 22 ~ "Day",
        TRUE ~ "Night"
    ))

## Plot number of murder and manslaughter per daytime
# Group data per daytime and count number of homicides
simple_prep <- dt %>% 
    group_by(daytime) %>%
    count(offense_type)

# Plot
simple_p <- ggplot(simple_prep, aes(x = daytime, y = n, fill = daytime)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("darkgoldenrod1", "darkblue")) +
    labs(title = "Homicides in New York City 2015",
         caption = "Source: Crime Open Database",
         x = "",
         y = "Count\n") +
    theme_bw() +
    theme(legend.position = "none")

# Save
ggsave("simple_plot.jpg",
       simple_p,
       path = file.path(here::here(), "03_figures", "09_challenge"))

## Daytime & longitude
# Group data by longitude and daytime
lon_prep <- dt %>%
    group_by(longitude, daytime) %>%
    count(offense_type)

# Plot
lon_p <- ggplot(lon_prep, aes(x = longitude, y = n, fill = daytime)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("darkgoldenrod1", "darkblue")) +
    labs(title = "Longitudinal distribution of homicides",
         caption = " ",
         x = "Longitude",
         y = "No. of homicides\n",
         fill = "Daytime") +
    theme_bw() 

## Daytime & latitude
# Group data by longitude and daytime
lat_prep <- dt %>%
    group_by(latitude, daytime) %>%
    count(offense_type)

# Plot
lat_p <- ggplot(lat_prep, aes(x = latitude, y = n, fill = daytime)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("darkgoldenrod1", "darkblue")) +
    labs(title = "Latitudinal distribution of homicides",
         caption = "Source: Crime Open Database",
         x = "Latitude",
         y = "No. of homicides\n",
         fill = "Daytime") +
    ylim(c(0, 115)) +
    theme_bw() 

## Arrange and plot
arr_p <- ggpubr::ggarrange(lon_p, lat_p, common.legend = TRUE, legend = "bottom")

ggsave("arr_p.jpg",
       arr_p,
       path = file.path(here::here(), "03_figures", "09_challenge"))

## Bubble chart with coordinates
# Group data by coordinates
bubble_prep <- dt %>%
    group_by(longitude, latitude) %>%
    count(offense_type) %>%
    rename(Count = n)

# Plot
bubble_p <- ggplot(bubble_prep, aes(x = longitude, y = latitude)) +
    geom_point(aes(size = Count)) +
    labs(title = "Spatial distribution of homicides in New York City 2015",
         caption = "Source: Crime Open Database",
         x = "Longitude",
         y = "Latitude\n") +
    theme_bw() 

# Save
ggsave("bubble_plot.jpg",
       bubble_p,
       path = file.path(here::here(), "03_figures", "09_challenge"))

## Map pie charts with background map
# 1. Prepare data
prep <- dt %>%
    
    # Round coordinates
    mutate(longitude = round(longitude, 1),
           latitude = round(latitude, 1)) %>%
    
    # Add proxy column for easier calculation
    mutate(count = 1) %>%
    
    # Add total number of homicides per coordinate pair
    group_by(longitude, latitude) %>%
    mutate(count_total = sum(count)) %>%
    
    # Add total number of homices per coordinate pair and daytime
    group_by(longitude, latitude, daytime) %>%
    mutate(count_daytime = sum(count)) %>%
    
    # Compute ratios for day and night
    mutate(ratio = count_daytime / count_total) %>%
    
    # Select relevant columns
    select(longitude, latitude, daytime, ratio, count_total) %>%
    unique() %>%
    
    # From long to wide
    pivot_wider(names_from = daytime, values_from = ratio) %>%
    
    # Change values manually
    # replace_na(list(Night = 0, Day = 0)) %>%
    
    # Add radius for plotting
    mutate(radius = sqrt(count_total) / 250) %>%
    
    # Rename column
    mutate(`Total cases` = count_total)

# Convert to spatial feature
crime_sf <- prep %>%
    
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 2. Map New York City wit OSM data
# Get tiles
nyc_map <- get_map(c(-74.25, 40.45, -73.65, 40.95), maptype = "toner-background")

# Map with pie charts
map_p <- ggmap(nyc_map) +
    
    # Add Scatterpies with individual color
    geom_scatterpie(aes(x = longitude, y = latitude, r = radius),
                    data = prep, cols = c("Night", "Day"), color = NA,
                    alpha = 0.8) +
    coord_fixed() +
    scale_fill_manual(values = c("darkblue", "darkgoldenrod1")) +
    geom_scatterpie_legend(prep$radius, n = 4, labeller = function(x) (x*250)^2, x = -73.775, y = 40.5) +
    
    # Labels, legend, ...
    labs(title = "Homicides in New York City 2015\n",
         subtitle = " Pie chart radius indicates number of homicides",
         x = "",
         y = "",
         fill = "Daytime",
         caption = "Source: Crime Open Database") +
    
    # Theme
    theme_bw() +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.direction = "horizontal")

# Map without pie charts
map_p <- ggmap(nyc_map) +
    
    # Add Scatterpies with individual color
    geom_point(data = prep, 
               aes(x = longitude, y = latitude, size = `Total cases`),
               color = "brown3", alpha = 0.7) +
    coord_fixed() +
    
    # Labels, legend, ...
    labs(title = "Homicides in New York City 2015\n",
         x = "",
         y = "",
         fill = "BLA",
         caption = "Source: Crime Open Database") +
    
    # Theme
    theme_bw()

# Save
ggsave("map_plot.jpg",
       map_p,
       path = file.path(here::here(), "03_figures", "09_challenge"),
       scale = 1.5)
