##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

# Purpose of script: Simulation study of habitat associations using gamma test

source("1_Functions/setup.R")

source("1_Functions/create_simulation_pattern.R")
source("1_Functions/create_simulation_species.R")

# set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

#### Create simulation experiment parameters ####

df_experiment <- expand.grid(association_strength = association_strength, fract_dim = fract_dim,
                             n_random = n_random) %>% 
  dplyr::mutate(row_counter = 1:dplyr::n()) %>% 
  dplyr::slice(rep(x = 1:dplyr::n(), each = iterations)) %>% 
  tibble::tibble()

simulation_habitat_list <- vector(mode = "list", length = nrow(df_experiment) / iterations)

simulation_pattern_list <- vector(mode = "list", length = nrow(df_experiment) / iterations)

j <- 1

for (i in seq(from = 1, to = nrow(df_experiment), by = iterations)) {
  
  # create simulation landscape with 5 discrete classes
  simulation_habitat <- NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows, resolution = resolution,
                                      fract_dim = df_experiment[i, "fract_dim", drop = TRUE],
                                      verbose = FALSE, cPrintlevel = 0) %>%
    terra::rast() %>%
    shar::classify_habitats(n = n)
  
  # create simulation pattern with 4 species
  simulation_pattern <- create_simulation_pattern(raster = simulation_habitat,
                                                  number_points = number_points,
                                                  association_strength = df_experiment[i, "association_strength", drop = TRUE])
  
  simulation_habitat_list[[j]] <- terra::wrap(simulation_habitat)
  
  simulation_pattern_list[[j]] <- simulation_pattern
  
  j <- j + 1
  
}

#### Save data ####

suppoRt::save_rds(object = df_experiment, filename = "df_experiment.rds", path = "3_Data/", 
                  overwrite = FALSE)

suppoRt::save_rds(object = simulation_habitat_list, filename = "simulation_habitat_list.rds", path = "3_Data/", 
                  overwrite = FALSE)

suppoRt::save_rds(object = simulation_pattern_list, filename = "simulation_pattern_list.rds", path = "3_Data/", 
                  overwrite = FALSE)
