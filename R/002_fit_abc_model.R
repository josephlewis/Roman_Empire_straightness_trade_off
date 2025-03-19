set.seed(NULL)
set.seed(1)

dem_files <- list.files(path = "./Data/DEM/", pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
dems <- lapply(dem_files, terra::rast)
RE_roads <- sf::st_read("./Data/Roman_roads/Roman_roads2.gpkg")
sf::st_geometry(RE_roads) <- "geometry"

b_mean_re <- truncnorm::rtruncnorm(n = nsims, mean = 1, sd = 10, a = 0)
b_sd_re <- rexp(n = nsims, rate = 1)
b_mean_province <- truncnorm::rtruncnorm(n = nsims, mean = b_mean_re, sd = b_sd_re, a = 0)
b_sd_province <- truncnorm::rtruncnorm(n = nsims, mean = b_sd_re, sd = 1, a = 0)

b <- truncnorm::rtruncnorm(n = nsims, mean = b_mean_province, sd = b_sd_province, a = 0)

quantile(b_mean_re)
quantile(b_sd_re)
quantile(b_mean_province)
quantile(b_sd_province)
quantile(b)

df_vals <- cbind(b_mean_re, b_sd_re, b_mean_province, b_sd_province, b)
nrow(df_vals)

ncores <- 80
RE_road_indx <- 186

for(RE_road_indx in 173:nrow(RE_roads)) {
  
  print(RE_road_indx)
  
  r_ext <- sf::st_buffer(sf::st_sf(sf::st_as_sfc(sf::st_bbox(RE_roads[RE_road_indx,]))), dist = 10000)
  
  dem_subset <- lapply(dems, FUN = function(x) { return(tryCatch(terra::crop(x, y = r_ext), error=function(e) NULL))})
  dem_subset2 <- dem_subset[lengths(dem_subset) != 0]
  if(length(dem_subset2) > 1) { 
    dem_subset2 <- do.call(terra::merge, dem_subset2)
  } else { 
    dem_subset2 <- dem_subset2[[1]]
    }
  
  input_data <- slope_calc(r = dem_subset2, route = RE_roads[RE_road_indx,], neighbours = neigh)
  message("calculating routes...")
  
  lcps_abc <- abc(input_data = input_data, df_vals = df_vals, route = RE_roads[RE_road_indx,], cf = exp_abc_cf,  spatial = FALSE, ncores = ncores,cost = TRUE)
  lcps_abc$RE_road_indx <- RE_road_indx
  saveRDS(lcps_abc, paste0("./Output/RE_roads_simulated/RE_road_8neigh_", RE_road_indx, ".rds"))
  
}