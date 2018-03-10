## ---- load-pkgs
library(lubridate)
library(tidyverse)
library(tsibble)
# devtools::install_github("heike/ggmapr")
library(ggmapr)

## ---- load-data
flights <- read_rds("data/flights.rds")

## ---- map-airlines-data
origin_dest <- flights %>% 
  distinct(origin, origin_state, dest, dest_state)
write_rds(origin_dest, path = "data/tsibble/origin_dest.rds")

## ---- map-airlines
origin_dest <- read_rds("data/tsibble/origin_dest.rds")
airports <- read_rds("data/airports.rds")
map_dat <- origin_dest %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  rename(long = lon) %>% 
  shift(origin_state == "HI", shift_by = c(52.5, 5.5)) %>%
  scale(origin_state == "AK", scale = 0.3, set_to = c(-117, 27)) %>%
  rename(origin_lat = lat, origin_lon = long)  %>% 
  left_join(select(airports, faa, lon, lat), by = c("dest" = "faa")) %>% 
  rename(long = lon) %>% 
  shift(dest_state == "HI", shift_by = c(52.5, 5.5)) %>%
  scale(dest_state == "AK", scale = 0.3, set_to = c(-117, 27)) %>%
  rename(dest_lat = lat, dest_lon = long)

states <- states %>%
  shift(NAME == "Hawaii", shift_by = c(52.5, 5.5)) %>%
  scale(NAME == "Alaska", scale = 0.3, set_to = c(-117, 27)) %>%
  filter(lat > 20)

ggplot() +
  geom_polygon(data= states, aes(x = long, y = lat, group = group), 
    fill = "white", colour = "grey60") +
  geom_curve(data = map_dat, aes(
    x = origin_lon, y = origin_lat, xend = dest_lon, yend = dest_lat
  ), curvature = 0.2, alpha = 0.2, size = 0.5, colour = "#762a83") +
  geom_point(data = map_dat, aes(x = origin_lon, y = origin_lat), 
    colour = "#f1a340", size  = 1.5) +
  ggthemes::theme_map()

## ---- tsibble
flights_ts <- flights %>% 
  as_tsibble(key = id(flight), index = sched_dep_datetime, regular = FALSE)
flights_ts
