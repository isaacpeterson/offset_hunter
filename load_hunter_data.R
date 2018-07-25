rm(list = ls())
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(abind)
library(pixmap)
library(offsetsim)

initialise_ecology_params <- function(){
  
  ecology_params = list()
  ecology_params$region_num = 1
  ecology_params$region_num_x = 1
  ecology_params$region_num_y = 1
  ecology_params$min_eco_val = 0  #minimum allowable ecological value of smallest ecological element (pixel)
  ecology_params$max_eco_val = 100 #maximum "   "     "           "
  
  return(ecology_params)
  
}

initialise_weighted_probability <- function(weight_layer, land_parcels){
  
  inds_to_use = !is.na(weight_layer)
  dev_weights = lapply(seq_along(land_parcels), function(i) mean(weight_layer[land_parcels[[i]]]))
  na_vals = is.na(unlist(dev_weights))
  dev_weights[na_vals] = 0
  scale_factor = sum(unlist(dev_weights))
  dev_weights = lapply(seq_along(dev_weights), function(i) dev_weights[[i]]/scale_factor)
  
  return(dev_weights)
}


load_mining_raster <- function(parcel_mask, current_data_folder){
  current_filenames <- list.files(path = current_data_folder, pattern = '.tif', all.files = FALSE, 
                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                  include.dirs = FALSE, no.. = FALSE)
  mining_raster = load_rasters(current_data_folder, current_filenames, layer_num = 1)
  mining_raster <- crop(mining_raster, extent(parcel_mask))
  mining_raster = as.matrix(mining_raster)
  return(mining_raster)
}

load_hunter_LGA <- function(parcel_mask, data_folder){
  
  LGA_shp <- readOGR(dsn = paste0(data_folder, 'LGA'), layer = "5_LGAs_StudyArea")
  
  LGA_raster <- shp_to_raster(shp = LGA_shp, raster_dims = dim(parcel_mask))
  
  extent(LGA_raster) = extent(parcel_mask)
  
  # define projections
  GDA.D <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs ")
  GDA94.56 <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  parcels_shp <- readOGR(dsn = paste0(data_folder, 'LH_property_2015'),  layer = "property")
  parcels_shp_transform <- spTransform(parcels_shp, GDA94.56) # project to correct CRS
  parcels_shp_cropped <- crop(parcels_shp_transform, extent(parcel_mask))
  parcels_raster = shp_to_raster(shp = parcels_shp_cropped, raster_dims = dim(parcel_mask))
  parcel_array = as.matrix(parcels_raster)
  parcel_array[is.na(parcel_array)] = 0
  
  return(parcel_array)
}


load_parcel_characteristics = TRUE
use_z_layer = TRUE
sample_decline_rate = FALSE

max_eco_val = 100
mean_decline_rate = -0.02
decline_rate_std = 0.005

ecology_params <- initialise_ecology_params()
data_folder = paste0(path.expand('~'), '/offset_data/hunter/MNES_data/')
parcel_mask = raster(paste0(data_folder, 'LH_property_2015/LH.clipping.mask_v2.tif'))

simulation_inputs_folder = paste0(path.expand('~'), '/offset_data/hunter/simulation_inputs/')

protected_areas_raster = load_rasters(paste0(data_folder),
                                      'ProtectedAreas_v4.tif', 
                                      layer_num = 'all')

protected_areas_mask = raster_to_array(protected_areas_raster)

current_filenames <- list.files(path = paste0(data_folder, 'species_layers_MNES/'), pattern = '.tif', all.files = FALSE, 
                                full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                include.dirs = FALSE, no.. = FALSE)

species_raster = load_rasters(paste0(data_folder, 'species_layers_MNES/'), current_filenames, 
                              layer_num = 'all')

if (use_z_layer == TRUE){
  current_filenames <- list.files(path = paste0(data_folder, 'z_layer/'), pattern = '.tif', all.files = FALSE, 
                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                  include.dirs = FALSE, no.. = FALSE)
  z_raster = load_rasters(paste0(data_folder, 'z_layer/'), current_filenames, layer_num = 1)
  species_raster = stack(z_raster, species_raster)
}


objects_to_save = list()

if (load_site_characteristics == TRUE){
  site_characteristics = readRDS(paste0(simulation_inputs_folder, 'site_characteristics.rds'))
  
} else { 
  LGA_array <- load_hunter_LGA(parcel_mask, data_folder)
  objects_to_save$site_characteristics = LGA_array
  site_characteristics <- LGA_to_parcel_list(LGA_array)
  objects_to_save$parcels = parcels
} 

landscape_ecology = lapply(seq(dim(species_raster)[3]), function(i) raster_to_array(subset(species_raster, i)))
objects_to_save$landscape_ecology = scale_ecology(landscape_ecology, ecology_params$max_eco_val, dim(landscape_ecology[[1]]))
objects_to_save$parcel_ecology <- split_ecology(objects_to_save$landscape_ecology, parcels$land_parcels)

mining_raster = load_mining_raster(parcel_mask, current_data_folder = paste0(data_folder, 'mining_scenarios/'))
objects_to_save$mining_mask = raster_to_array(mining_raster) & (raster_to_array(parcel_mask) > 0)

objects_to_save$dev_weights <- initialise_weighted_probability(objects_to_save$mining_mask, 
                                               land_parcels = parcels$land_parcels)

objects_to_save$offset_mask <- (protected_areas_mask == 3) & !(objects_to_save$mining_mask)
objects_to_save$offset_weights <- initialise_weighted_probability(objects_to_save$offset_mask, 
                                                  parcels$land_parcels)

objects_to_save$decline_rates_initial = simulate_decline_rates(parcel_num = length(parcels$land_parcels), 
                                                               sample_decline_rate, 
                                                               rep(list(mean_decline_rate), dim(species_raster)[3]), 
                                                               rep(list(decline_rate_std), dim(species_raster)[3]))

save_simulation_inputs(objects_to_save, simulation_inputs_folder)
