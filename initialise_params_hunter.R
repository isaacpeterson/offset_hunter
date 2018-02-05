initialise_user_global_params <- function(){
  
  global_params = list()
  global_params$simulation_folder = paste0(path.expand('~'), '/offset_data/hunter/')
  
  global_params$overwrite_default_params = TRUE
  
  global_params$use_simulated_data = FALSE
  
  global_params$number_of_cores = 1
  
  # The number of realizations to run
  global_params$realisation_num = 1
  
  # Makes a single pdf at the end of the simulation showing the locatons of all offsets
  global_params$write_offset_layer = TRUE
  
  # Create an animation of the outputs
  global_params$write_movie = TRUE
  
  return(global_params)
}


initialise_user_combination_params <- function(){ #list of variations in policy
  combination_params = list()
  combination_params$allow_developments_from_credit = TRUE
  combination_params$development_selection_type = 'weighted'  #how the development parcels are selected - 'random' or 'weighted'

  combination_params$site_for_site = c(FALSE) # TRUE - one-to-one selection of offset parcels for one development, FALSE = many-to-one selection of offset parcels for one development
  combination_params$offset_time_horizon = c(30)
  combination_params$offset_action_params = list(c('avoided_loss', 'maintain')) 
  
  combination_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 
  
  combination_params$include_potential_developments_in_offset_calc = c(TRUE)
  combination_params$include_illegal_clearing_in_offset_calc = c(TRUE)
  
  combination_params$dev_counterfactual_adjustment = 'as_offset'  
  combination_params$offset_multiplier = 1
  
  return(combination_params)
}



initialise_user_simulation_params <- function(){ 
  
  simulation_params = list()
  
  # what subset of features to use in the simulation
  simulation_params$features_to_use_in_simulation = 1
  
  # The total number of layers to use in the offset calcuation (iterating from the start)
  simulation_params$features_to_use_in_offset_calc = 1
  
  simulation_params$features_to_use_in_offset_intervention = 1
  
  # The total number of parcels that will be developed
  simulation_params$total_dev_num = 40
  
  # The time step at which development starts
  simulation_params$dev_start = 1
  
  # The time at which development ends
  simulation_params$dev_end = 50
  
  # How long to run the simulaton in years
  simulation_params$time_steps = 50
  
  # The maxoimum number of parcels can be selected to offset a single development
  
  simulation_params$max_offset_parcel_num = 20
  
  # Stops the offset from delivering any further gains once it has acheived the gains required
  simulation_params$limit_offset_restoration = TRUE
  
  # The probability per parcel of it being illegally cleared, every parcel gets set to this number - set to zero to turn off
  simulation_params$illegal_clearing_prob = 0
  
  # Exclude parcels with less than this number of pixels.
  simulation_params$site_screen_size = 50
  
  # The mean and the standard deviation of a normal distribution from which to sample the restoration parameters from
  simulation_params$restoration_rate = 0.02
  
  simulation_params$restoration_rate_std = 0.005
  #   c('net_gains', 'restoration_gains', 'avoided_condition_decline', 'avoided_loss',
  #     'protected_condition', 'current_condition', 'restored_condition')
  
  simulation_params$offset_action_params = list(c('avoided_loss', 'maintain'))
  
  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
  simulation_params$dev_calc_type = 'future_condition'    #'future_condition', 'current_condition' 
  
  # Track accumulated credit from previous exchanges (eithger in current or
  # previous time step) and use them to allow developments to proceed if the
  # credit is large enough. FALSE means ignore any exces credit from offset exchanges
  simulation_params$allow_developments_from_credit = TRUE
  
  # How the development parcels are selected options are 'random' or
  # 'weighted'. Note tha weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # intialise_routines.R  (or put the files in simulation_inputs)
  simulation_params$development_selection_type = 'random'  
  
  # Whether to use banking. FALSE - means perform offsets simultaneously with development, TRUE -
  # means perform offset banking prior to development according to offset bank
  # parameters
  simulation_params$use_offset_bank = FALSE

  # The time horizon in which the offset gains need to equal the devlopment impact
  simulation_params$offset_time_horizon = 20
  
  # Include illegal clearing in the calculating the contribution of avoided
  # losses to the impact of the development. 
  
  # The probability per parcel of it being illegally cleared, every parcel gets set to this number - set to zero to turn off
  simulation_params$illegal_clearing_prob = 0.001
  
  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  simulation_params$include_potential_developments_in_offset_calc = TRUE
  
  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  simulation_params$include_illegal_clearing_in_offset_calc = TRUE
  
  simulation_params$dev_counterfactual_adjustment = 'as_offset'
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  simulation_params$offset_multiplier = 1
  
  
  return(simulation_params)
  
}





initialise_user_plot_params <- function(){
  plot_params = list()
  plot_params$output_plot_folder = vector()
  plot_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
  plot_params$output_type = 'scenarios' # set to plot through 'features', 'scenarios' or 'site_sets'
  plot_params$realisation_num = 1 # 'all' or number to plot
  plot_params$features_to_plot = 1:3
  plot_params$write_pdf = FALSE
  plot_params$sets_to_plot = 5 # example site to plot
  plot_params$scenario_vec = 'all' #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  plot_params$site_impact_col_vec = c('darkgreen', 'red', 'black')
  plot_params$program_col_vec = c('darkgreen', 'red', 'black') 
  plot_params$cfac_col = 'blue' 
  plot_params$landscape_col = 'black'
  plot_params$lwd_vec = c(3, 0.5)
  
  plot_params$plot_subset_type = c('dev_calc_type') # 'offset_calc', 'time_horizon'
  plot_params$plot_subset_param = c('future_condition')
  
  plot_params$site_impact_lwd = 0.5
  plot_params$site_outcome_lwd_vec = c(0.5)
  plot_params$program_lwd_vec = c(3, 0.5)
  plot_params$program_outcome_lwd_vec = c(3, 0.5)
  plot_params$landscape_lwd_vec  = c(3)
  plot_params$landscape_outcome_lwd_vec = c(3)
  
  plot_params$string_width = 3 # how many digits are used to store scenario index and realisation index
  plot_params$nx = 3 
  plot_params$ny = 4
  
  plot_params$site_outcome_plot_lims_set = rep(list(c(0, 3e4)), max(plot_params$features_to_plot))
  plot_params$program_outcome_plot_lims_set = rep(list(c(0e6, 1e7)), max(plot_params$features_to_plot))
  plot_params$landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), max(plot_params$features_to_plot))
  
  plot_params$site_impact_plot_lims_set = rep(list(c(-5e3, 5e3)), max(plot_params$features_to_plot))
  plot_params$program_impact_plot_lims_set = rep(list(c(-1e6, 1e6)), max(plot_params$features_to_plot))
  plot_params$landscape_impact_plot_lims_set = rep(list(c(-1e6, 0)), max(plot_params$features_to_plot))
  
  return(plot_params)
}


