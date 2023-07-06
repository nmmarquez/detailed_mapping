rm(list = ls())
library(tidyverse) # data manipulation
library(sf) # spatial data objects
library(tidycensus) # get census data easily
library(tigris) # get spatial data easily
library(maptiles) # tiles for adding context to plots
library(tidyterra) # easy plotting of map tiles
library(cowplot) # combine multiple plots together
library(ggspatial) # plot spatial objects like bounding boxes

# pull Seattle spatial object
seattle <- places("WA", year = 2018, class = "sf") %>%
    filter(NAME == "Seattle") %>%
    st_transform(32148)

# pull tract level mhi data
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

# very basic r plot
basic_map <- mhi_raw_sf %>%
    ggplot() +
    geom_sf(aes(fill = MHIQ)) +
    scale_fill_ordinal(na.value = "grey50") +
    theme_void(base_size = 14) +
    labs(fill="Median\nHousehold\nIncome") +
    ggtitle(
        "Seattle Median Household Income",
        "MHI by Census Tract")

ggsave("images/raw.png", basic_map,  width = 9*.7, height = 10*.7, bg = "white")

# remove water
mhi_sf <- mhi_raw_sf %>%
    erase_water(area_threshold = 0.9)

# get map tiles adding road context
dc <- mhi_sf %>%
    get_tiles(
        provider = "Stamen.TonerLines",
        zoom = 12, crop = T)

road_map <- ggplot() +
    geom_spatraster_rgb(data = dc) + # plot road map tiles
    geom_sf(data = mhi_sf, aes(fill=MHIQ), alpha = .4) +
    coord_sf(crs = 32148) +
    scale_fill_ordinal(na.translate=FALSE) + # remove "na" from legend
    theme_void(base_size = 14) +
    labs(fill="Median\nHousehold\nIncome") +
    theme(legend.position = c(1.13,.37), legend.background=element_blank())

ggsave(
    "images/updated.png", road_map,  width = 9*.7, height = 10*.7, bg = "white")

# what are the geographies we want to focus on
keep_geos <- str_c(
    "5303300",
    c(
        "7201", "7202", "7302", "8002", "8003", "8101", "8102",
        "8200", "8300", "8401", "8402", "8500","9200"))

# cropped version of main spatial object focusing on downtown geographies
cropped_mhi_sf <- mhi_sf %>%
    st_crop(st_bbox(filter(mhi_sf, GEOID %in% keep_geos)))

# get the map tiles for downtown this time with road names
dc2 <- cropped_mhi_sf %>%
    get_tiles(
        provider = "Stamen.TonerHybrid",
        zoom = 14, crop = T)

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
        data=st_bbox(filter(mhi_sf, GEOID %in% keep_geos)),
        alpha = 0,
        size=1.15,
        color = "red") +
    labs(fill="Median\nHousehold\nIncome")

# make the final plot using r package "cowplot"
final_map <- ggdraw(
    road_map +
        theme(legend.position = "none") +
        layer_spatial(
            data=st_bbox(filter(mhi_sf, GEOID %in% keep_geos)),
            alpha = 0,
            size=1.15,
            color = "red")
        , xlim = c(0,.7)) +
    draw_plot(
        inlet_map+theme(legend.position = "none"),
        width = .36,
        height = .36,
        x=-.01,
        y=.27) +
    draw_plot(
        get_legend(inlet_map),
        width = .36,
        height = .36,
        x=.01,
        y=.01) +
    draw_text(
        "Seattle Median Household Income",
        x = .215,
        y = .9,
        size = 24) +
    draw_text(
        "MHI by Census Tract",
        x = .10,
        y = .85,
        size = 18
    )

ggsave("./images/final.png", final_map, width = 9, height = 10, bg = "white")
