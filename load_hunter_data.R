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

build_probability_list <- function(weight_layer, land_parcels, site_indexes_to_exclude){
  
  intervention_weights = rep(list(0), length(land_parcels))
  sites_to_use = setdiff(seq_along(land_parcels), site_indexes_to_exclude)
  intervention_weights[sites_to_use] = lapply(sites_to_use, function(i) mean(weight_layer[land_parcels[[i]]]))
  scale_factor = sum(unlist(intervention_weights))
  intervention_weights = lapply(seq_along(intervention_weights), function(i) intervention_weights[[i]]/scale_factor)
  
  return(intervention_weights)
}




load_site_characteristics = TRUE
use_z_layer = TRUE
sample_decline_rate = FALSE

max_eco_val = 100
mean_decline_rate = -0.02
decline_rate_std = 0.005

ecology_params <- initialise_ecology_params()
data_folder = paste0(path.expand('~'), '/offset_data/hunter/MNES_data/')
cadastre_msk = raster(paste0(data_folder, 'LH_property_2015/LH.clipping.mask_v2.tif'))

simulation_inputs_folder = paste0(path.expand('~'), '/offset_data/hunter/simulation_inputs/')

protected_areas_raster = load_rasters(paste0(data_folder,'ProtectedAreas_v4.tif'), 'all')

protected_areas_msk = raster_to_array(protected_areas_raster)

current_filenames <- list.files(path = paste0(data_folder, 'species_layers_MNES/'), pattern = '.tif', all.files = FALSE, 
                                full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                include.dirs = FALSE, no.. = FALSE)

species_raster = load_rasters(paste0(data_folder, 'species_layers_MNES/', current_filenames), 'all')

if (use_z_layer == TRUE){
  current_filenames <- list.files(path = paste0(data_folder, 'z_layer/'), pattern = '.tif', all.files = FALSE, 
                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                  include.dirs = FALSE, no.. = FALSE)
  z_raster = load_rasters(paste0(data_folder, 'z_layer/', current_filenames), 'all')
  species_raster = stack(z_raster, species_raster)
}




if (load_site_characteristics == TRUE){
  site_characteristics = readRDS(paste0(simulation_inputs_folder, 'site_characteristics.rds'))
  
} else { 
  LGA_shp <- readOGR(dsn = paste0(data_folder, 'LGA'), layer = "5_LGAs_StudyArea")
  LGA_raster <- shp_to_raster(shp = LGA_shp, raster_dims = dim(cadastre_msk))
  extent(LGA_raster) = extent(cadastre_msk)
  
  # define projections
  GDA.D <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs ")
  GDA94.56 <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  site_ID_shp <- readOGR(dsn = paste0(data_folder, 'LH_property_2015'),  layer = "property")
  site_ID_shp_transform <- spTransform(parcels_shp, GDA94.56) # project to correct CRS
  site_ID_shp_cropped <- crop(parcels_shp_transform, extent(cadastre_msk))
  site_ID_raster = shp_to_raster(shp = parcels_shp_cropped, raster_dims = dim(cadastre_msk))
  extent(site_ID_raster) = extent(cadastre_msk)
  writeRaster(site_ID_raster, paste0(simulation_inputs_folder, 'hunter_site_IDs.tif'), overwrite = TRUE)
  site_characteristics = build_site_characteristics(raster_to_array(site_ID_raster))
} 


######## NOTE: GET HEINI TO CHECK ON MINING LAYER ORIENTATION AS IT LOOKS ROTATED BY 90 DEG.

mining_layer_filenames <- list.files(path = paste0(data_folder, 'mining_scenarios/'), pattern = '.tif', all.files = FALSE, 
                                full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                include.dirs = FALSE, no.. = FALSE)
mining_layer = load_rasters(paste0(data_folder, 'mining_scenarios/', mining_layer_filenames), 'all')
mining_layer <- crop(mining_layer, extent(cadastre_msk))
mining_layer = raster_to_array(mining_layer)

mining_msk = mining_layer & (raster_to_array(cadastre_msk) > 0)

objects_to_save = list()
objects_to_save$dev_probability_list <- build_probability_list(mining_msk, site_characteristics$land_parcels, site_indexes_to_exclude = 1)

offset_msk <- (protected_areas_msk == 3) & !(mining_layer > 0)
objects_to_save$offset_probability_list <- build_probability_list(offset_msk, site_characteristics$land_parcels, site_indexes_to_exclude = 1)

objects_to_save$unregulated_probability_list <- build_probability_list(raster_to_array(cadastre_msk), site_characteristics$land_parcels, site_indexes_to_exclude = 1)

save_simulation_inputs(objects_to_save, simulation_inputs_folder)
