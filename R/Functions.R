exp_abc_cf <- function(x, y) {(1 * exp(-y[5] * abs(x))) / 3.6}

slope_calc <- function(r, route, max_slope = NULL, neighbours = 8) {
  
  route_pts <- origin_destination(route)
  route_pts_cells <- terra::cellFromXY(r, sf::st_coordinates(route_pts))
  route_pts_XY <- terra::xyFromCell(r, route_pts_cells)
  route_pts_centre <- route_pts_XY %>% 
    as.data.frame %>% 
    sf::st_as_sf(coords = c(1,2))
  
  route_pts_centre_bbox <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(route_pts_centre)))
  
  r2 <- terra::crop(r, sf::st_buffer(route_pts_centre_bbox, dist = max(terra::res(r))*50))
  
  neighbours <- leastcostpath::neighbourhood(neighbours = neighbours)
  
  cells <- which(!is.na(terra::values(r2)))
  na_cells <- which(is.na(terra::values(r2)))
  
  adj <- terra::adjacent(x = r2, cells = cells, directions = neighbours, pairs = TRUE)
  adj <- adj[!adj[,2] %in% na_cells,]
  
  elev_values <- terra::values(r2)[,1]
  
  message("calculating slope...")
  
  rise <- (elev_values[adj[,2]] - elev_values[adj[,1]])
  run <- calculate_distance_geosphere(x = r2, adj = adj)
  
  mathematical_slope <- rise/run
  
  ncells <- length(cells) + length(na_cells)
  
  cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells, sparse = TRUE)
  cs_matrix[adj] <- mathematical_slope
  
  cs <- list("conductanceMatrix" = cs_matrix, 
             "costFunction" = NA,
             "maxSlope" = ifelse(!is.null(max_slope), paste0(max_slope*100, "%"), NA), 
             "exaggeration" = NA,
             "criticalSlope" = NA,
             "neighbours" = sum(neighbours, na.rm = TRUE),
             "resolution" = terra::res(r2), 
             "nrow" = terra::nrow(r2), 
             "ncol" = terra::ncol(r2), 
             "extent" = as.vector(terra::ext(r2)), 
             "crs" = terra::crs(r2, proj = TRUE))
  
  class(cs) <- "conductanceMatrix"
  
  route_list <- list(cs, run, adj)
  
  return(route_list)
  
}

calculate_distance_geosphere <- function(x, adj) { 
  
  if(sf::st_is_longlat(x)) { 
    
    xy1 <- data.frame(terra::xyFromCell(x, adj[, 1]))
    xy2 <- data.frame(terra::xyFromCell(x, adj[, 2]))
    
    dist <- as.numeric(geosphere::distHaversine(xy1, xy2))
    
  } else {
    
    xy1 <- terra::xyFromCell(x, adj[, 1])
    xy2 <- terra::xyFromCell(x,adj[, 2])
    
    xy3 <- (xy1[,1] - xy2[,1])^2
    xy4 <- (xy1[,2] - xy2[,2])^2
    
    dist <- sqrt(xy3 + xy4)  
    
  }
  
  return(dist)
  
}

origin_destination <- function(route) {
  
  route_pts <- sf::st_cast(route, "POINT")
  
  origin <- route_pts[1,]
  destination <- route_pts[nrow(route_pts),]
  
  od <- rbind(origin, destination)
  
  return(od)
  
}

abc <- function(input_data, df_vals, route, cf, ncores, spatial = TRUE, ...) {
  
  od <- origin_destination(route)
  
  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)
  
  lcps <- foreach::foreach(i = 1:nrow(df_vals), .packages= c("sf", "leastcostpath", "Matrix"), .combine = "rbind") %dopar% {
    
    input_data2 <- input_data
    
    y <- df_vals[i,, drop = FALSE]
    colnames(y) <- paste0("p.", colnames(y))
    
    input_data2[[1]]$conductanceMatrix[input_data2[[3]]] <- cf(x =  input_data2[[1]]$conductanceMatrix[input_data2[[3]]], y = y)
    
    input_data2[[1]]$conductanceMatrix[input_data2[[3]]] <-   input_data2[[1]]$conductanceMatrix[input_data2[[3]]] / input_data2[[2]]
    
    lcp <- leastcostpath::create_lcp(x = input_data2[[1]], origin = od[1,], destination = od[2,], ...)
    sf::st_crs(lcp) <- sf::st_crs(route)
    
    max_dist <- max(as.numeric(sf::st_distance(sf::st_cast(lcp, "POINT"), y = route)))
    
    lcp_index <- i
    lcp <- cbind(lcp, lcp_index, y, max_dist)
    
    if(spatial != TRUE) {
      lcp <- sf::st_drop_geometry(lcp)
    }
    
    return(lcp)
  }
  
  parallel::stopCluster(myCluster)
  
  return(lcps)
}