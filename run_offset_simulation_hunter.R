library(offsetsim)
source('logistic_hunter_params.R' )

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
user_feature_params = initialise_user_feature_params(user_simulation_params$features_to_use_in_simulation)
user_output_params <- initialise_user_output_params()

osim.run(user_global_params, user_simulation_params, user_feature_params, user_transform_function, loglevel = 'TRACE')

simulation_folder = find_current_run_folder(base_folder = '~/offset_data/hunter/')
osim.output(user_output_params, simulation_folder, loglevel = 'TRACE')