rm(list = ls())

library(offsetsim)

source('initialise_params_hunter.R')

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
osim.run(user_global_params, user_simulation_params, loglevel = 'TRACE')

plot_params <- initialise_user_plot_params()
osim.plot(plot_params, user_global_params$simulation_folder, loglevel = 'TRACE')