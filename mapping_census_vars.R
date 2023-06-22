rm(list = ls())
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(maptiles)
library(tidyterra)

seattle <- places("WA", year = 2018, class = "sf") %>%
    filter(NAME == "Seattle") %>%
    st_transform(32148)

mhi_raw_sf <- get_acs(
    geography = "tract", variables = "B19013_001", state = "WA",
    county = "King", geometry = TRUE) %>%
    st_transform(32148) %>%
    # add some wiggle room around shape
    st_buffer(1e-5) %>%
    # cut the outline of Seattle
    st_intersection(seattle) %>%
    mutate(MHIQ = cut(
        estimate,
        breaks = c(20, 40, 75, 100, 125, 150, Inf)*1000,
        labels = c(
            "20-40k", "40-75k", "75-100k", "100-125k", "125-150k", ">150k")))

mhi_raw_sf %>%
    ggplot() +
    geom_sf(aes(fill = MHIQ)) +
    scale_fill_ordinal(na.value = "grey50") +
    theme_void(base_size = 14) +
    labs(fill="Median\nHousehold\nIncome") +
    ggtitle(
        "Seattle Median Houshold Income",
        "MHI by Census Tract")

mhi_sf <- mhi_raw_sf %>%
    erase_water(area_threshold = 0.9)

dc <- mhi_sf %>%
    get_tiles(
        provider = "Stamen.TonerLines",
        zoom = 11, crop = T)

ggplot() +
    geom_spatraster_rgb(data = dc)

road_map <- ggplot() +
    geom_spatraster_rgb(data = dc) +
    geom_sf(
        data = mhi_sf,
        aes(fill=MHIQ),
        alpha = .4) +
    coord_sf(crs = 32148) +
    scale_fill_ordinal(na.value = "grey50") +
    theme_void(base_size = 14) +
    labs(fill="Median\nHousehold\nIncome") +
    ggtitle(
        "Seattle Median Houshold Income",
        "MHI by Census Tract")

road_map
