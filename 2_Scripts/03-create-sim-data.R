##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1_Functions/setup.R")

source("1_Functions/create_simulation_pattern.R")
source("1_Functions/create_simulation_species.R")

RandomFields::RFoptions(install = "no")

#### Setup lists to store results ####

# calculate number of total jobs based on treatment levels
n_jobs <- length(association_strength) * length(fract_dim) * length(n_random) * iterations

# create lists to store results
simulation_experiment_list <- vector(mode = "list", length = n_jobs)

#### Create data using loop ####

# create total counter to store results in list for nested loops
counter <- 1

for (i in seq_along(association_strength)) {
  
  for (j in seq_along(fract_dim)) {
    
    for (k in seq_along(n_random)) {
      
      # create simulation landscape with n discrete classes
      simulation_habitat <- NLMR::nlm_fbm(ncol = number_cols, nrow = number_rows, resolution = resolution,
                                          fract_dim = fract_dim[[j]],
                                          verbose = FALSE, cPrintlevel = 0) |>
        terra::rast() |>
        shar::classify_habitats(n = n, style = "fisher")

      # create simulation pattern
      simulation_pattern <- create_simulation_pattern(raster = simulation_habitat,
                                                      number_points = number_points,
                                                      association_strength = association_strength[[i]])
      
      # save results for each iteration/repetition
      for (l in seq_len(iterations)) {
        
        # in x times of repetitions, create new simulation data
        if (runif(n = 1) < 1/4) {
          
          # create simulation landscape with 5 discrete classes
          simulation_habitat <- NLMR::nlm_fbm(ncol = number_cols, nrow = number_rows, resolution = resolution,
                                              fract_dim = fract_dim[[j]],
                                              verbose = FALSE, cPrintlevel = 0) |> 
            terra::rast() |>
            shar::classify_habitats(n = n, style = "fisher")
          
          # create simulation pattern with 4 species  
          simulation_pattern <- create_simulation_pattern(raster = simulation_habitat,
                                                          number_points = number_points,
                                                          association_strength = association_strength[[i]])
          
        }
        
        # combine to list and save in final list
        simulation_experiment_list[[counter]] <- list(parameter = c(fract_dim = fract_dim[[j]], n_random = n_random[[k]], 
                                                                    association_strength = association_strength[[i]]), 
                                                      habitat = terra::wrap(simulation_habitat), 
                                                      pattern = simulation_pattern)
        
        message("\r> Progres: ", counter, " / ", n_jobs, "\t\t", appendLF = FALSE)
        
        counter <- counter + 1
        
      }
    }
  } 
}

#### Save data ####

suppoRt::save_rds(object = simulation_experiment_list, filename = "simulation_experiment_list.rds", 
                  path = "3_Data/", overwrite = FALSE)
