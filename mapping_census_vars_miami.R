rm(list = ls())
library(tidyverse) # data manipulation
library(sf) # spatial data objects
library(tidycensus) # get census data easily
library(tigris) # get spatial data easily
library(maptiles) # tiles for adding context to plots
library(tidyterra) # easy plotting of map tiles
library(cowplot) # combine multiple plots together
library(ggspatial) # plot spatial objects like bounding boxes

# pull Miami Beach spatial object
miami_beach <- places("FL", year = 2022, class = "sf") %>%
    filter(NAME == "Miami Beach") %>%
    st_transform(32148)

# pull tract level mhi data
mhi_raw_sf <- get_acs(
    geography = "tract", variables = "B19013_001", state = "FL",
    county = "Miami-Dade", geometry = TRUE, year = 2022) %>%
    st_transform(32148) %>%
    # remove some of the larger census tracts on the outskirts of the county
    filter(GEOID != "12086011500") %>%
    filter(GEOID != "12086011408") %>%
    filter(GEOID != "12086011412") %>%
    filter(GEOID != "12086981200") %>%
    filter(GEOID != "12086980100") %>%
    mutate(MHIQ = cut(
        estimate,
        breaks = c(20, 40, 75, 100, 125, 150, Inf)*1000,
        labels = c(
            "20-40k", "40-75k", "75-100k", "100-125k", "125-150k", ">150k")))

# very basic r plot
basic_map <- mhi_raw_sf %>%
    ggplot() +
    geom_sf(aes(fill = MHIQ)) +
    scale_fill_ordinal(na.value = "grey50") +
    theme_void(base_size = 14) +
    labs(fill="Median\nHousehold\nIncome") +
    ggtitle(
        "Miami Median Household Income",
        "MHI by Census Tract")

basic_map
ggsave("images/raw.png", basic_map,  width = 9*.7, height = 10*.7, bg = "white")

# remove water
mhi_sf <- mhi_raw_sf %>%
    erase_water(area_threshold = 0.2)

# get map tiles adding road context
dc <- mhi_sf %>%
    get_tiles(
        provider = "CartoDB.Voyager",
        zoom = 10, crop = T)

road_map <- ggplot() +
    geom_spatraster_rgb(data = dc) + # plot road map tiles
    geom_sf(data = mhi_sf, aes(fill=MHIQ), alpha = .3, color=alpha("black",0.2)) +
    coord_sf(crs = 32148) +
    scale_fill_ordinal(na.translate=FALSE) + # remove "na" from legend
    theme_void(base_size = 14) +
    labs(fill="Median\nHousehold\nIncome") +
    theme(legend.position = c(1.13,.37), legend.background=element_blank())

road_map
ggsave(
    "images/updated.png", road_map,  width = 9*.7, height = 10*.7, bg = "white")


# cropped version of main spatial object focusing on miami-beach geographies
cropped_mhi_sf <- mhi_sf %>%
    st_buffer(1e-5) %>%
    st_intersection(miami_beach)

# get the map tiles for downtown this time with road names
dc2 <- cropped_mhi_sf %>%
    get_tiles(
        provider = "CartoDB.Voyager",
        zoom = 13, crop = T)

# make zoomed in plot
inlet_map <- cropped_mhi_sf %>%
    ggplot() +
    geom_spatraster_rgb(data = dc2) +
    geom_sf(data = cropped_mhi_sf, aes(fill=MHIQ), alpha = .4) +
    coord_sf(crs = 32148) +
    scale_fill_ordinal(na.value = "grey50", na.translate=FALSE) +
    theme_void(base_size = 14) +
    theme(legend.position = "bottom") +
    layer_spatial(
        data=st_bbox(st_intersection(st_buffer(mhi_sf, 1e-5), miami_beach)),
        alpha = 0,
        size=1.15,
        color = "red") +
    labs(fill="Median\nHousehold\nIncome")

# make the final plot using r package "cowplot"
final_map <- ggdraw(
    road_map +
        theme(legend.position = "none") +
        layer_spatial(
            data=st_bbox(st_intersection(st_buffer(mhi_sf, 1e-5), miami_beach)),
            alpha = 0,
            size=1.15,
            color = "red")
        , xlim = c(0,.7)) +
    draw_plot(
        inlet_map+theme(legend.position = "none"),
        width = .36,
        height = .56,
        x=-.01,
        y=.27) +
    draw_plot(
        get_legend(inlet_map),
        width = .36,
        height = .36,
        x=.01,
        y=.01) +
    draw_text(
        "Miami-Dade Residential\nIncome Segregation",
        x = .175,
        y = .9,
        size = 24)

final_map

ggsave("./images/final.png", final_map, width = 9, height = 10, bg = "white")
