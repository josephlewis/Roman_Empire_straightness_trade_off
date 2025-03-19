RE_roads <- sf::st_read("./Data/Roman_roads/roman_roads_v2008.shp")
RE_roads <- sf::st_transform(RE_roads, crs = sf::st_crs(4326))
RE_roads <- RE_roads[RE_roads$CLASS == "Major Road",]

RE_roads_sfnetwork <- sfnetworks::as_sfnetwork(RE_roads)

RE_roads_sfnetwork <- RE_roads %>%
  sfnetworks::as_sfnetwork() %>%
  activate("edges") %>%
  convert(., to_spatial_subdivision)

RE_roads2 <- RE_roads_sfnetwork %>%
  activate("edges") %>% 
  st_as_sf() %>%
  select(-c(from, to)) %>%
  mutate(length_km = as.numeric(sf::st_length(.)/1000)) %>%
  filter(length_km > 1) %>%
  sf::st_make_valid()

RE_ext <- sf::st_read("./Data/extent/roman_empire_ad_117.gpkg") %>%
  filter(name != "Armenia Mesopotamia") %>%
  sf::st_make_valid()

RE_ext[RE_ext$name %in% c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI"),]$name <- "Italy"

RE_roads3 <- sf::st_crop(RE_roads2, sf::st_union(RE_ext))

RE_roads3 <- RE_roads3[st_within(RE_roads3, sf::st_union(RE_ext), sparse = FALSE),]
RE_roads4 <- st_join(RE_roads3, RE_ext, join = st_intersects)

dem_files <- list.files(path = "./Data/DEM/", pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
dem_list <- lapply(dem_files, terra::rast)

RE_roads5 <- sf::st_transform(RE_roads4, crs = sf::st_crs(dem_list[[1]]))
RE_roads5$RE_road_indx <- 1:nrow(RE_roads5)
sf::st_write(RE_roads5, "./Data/Roman_roads/Roman_roads2.gpkg", append = FALSE)