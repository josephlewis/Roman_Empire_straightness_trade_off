library(sf)
library(terra)

library(tidygraph)
library(sfnetworks)

library(leastcostpath)

library(foreach)

#setwd("/home/jl2094/Roman_Empire_straightness_trade_off")

source("./R/Functions.R")

neigh <- 8
nsims <- 100000
ncores <- 80
