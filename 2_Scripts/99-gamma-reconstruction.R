##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

#### Import packages & functions ####

source("1_Functions/setup.R")
source("1_Functions/create_simulation_pattern.R")
source("1_Functions/create_simulation_species.R")

# set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

RandomFields::RFoptions(install = "no")

#### Create example data ####
# create landscape
simulation_landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, 
                                      resolution = 20, fract_dim = 1.5, 
                                      verbose = FALSE, 
                                      cPrintlevel = 0) |> 
  terra::rast() |> 
  shar::classify_habitats(classes = 5, style = "fisher")

# create pattern with 4 species
simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 50, 
                                                association_strength = 0.35)

# pick species 2 as example
example_species <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2)

#### Randomize data ####

# fit clustered pattern to data
gamma_test <- shar::fit_point_process(spatstat.geom::unmark(example_species), 
                                      n_random = 99, process = "cluster")

pattern_recon <- shar::reconstruct_pattern(spatstat.geom::unmark(example_species), 
                                           n_random = 99, max_runs = max_runs,
                                           method = "cluster", no_change = no_change)

#### Save results ####

suppoRt::save_rds(object = gamma_test, filename = "appendix_gamma_test.rds",
                  path = "3_Data/", overwrite = FALSE)

suppoRt::save_rds(object = pattern_recon, filename = "appendix_pattern_recon.rds",
                  path = "3_Data/", overwrite = FALSE)

#### Create figures ####

# read data
gamma <- readr::read_rds("3_Data/appendix_gamma_test.rds")
recon <- readr::read_rds("3_Data/appendix_pattern_recon.rds")

# calculate pcf of inital pattern
pcf_observed <- spatstat.explore::pcf.ppp(gamma$observed, divisor = "d", correction = "Ripley", 
                                          r = seq(from = 0, to = 250, length.out = 515)) |> 
  tibble::as_tibble()

# calculate pcf of all gamma model fits
result_gamma <- purrr::map_dfr(gamma$randomized, function(current_pattern) { 
  tibble::as_tibble(
    spatstat.explore::pcf.ppp(current_pattern, divisor = "d", correction = "Ripley", 
                              r = seq(from = 0, to = 250, length.out = 515))
  )}, .id = "pattern")

# calculate pcf of all reconstructions
result_recon <- purrr::map_dfr(recon$randomized, function(current_pattern) { 
  tibble::as_tibble(
    spatstat.explore::pcf.ppp(current_pattern, divisor = "d", correction = "Ripley", 
                              r = seq(from = 0, to = 250, length.out = 515))
  )}, .id = "pattern")

# combine both methods 
result_combn <- dplyr::bind_rows(gamma = result_gamma, recon = result_recon, .id = "method") |> 
  dplyr::group_by(method, r) |> 
  dplyr::summarise(lo = quantile(iso, probs = 0.025),
                   hi = quantile(iso, probs = 0.975), .groups = "drop") |> 
  dplyr::mutate(method = factor(method, levels = c("gamma", "recon"),
                                labels = c("gamma" = "gamma-test", "recon" = "Pattern reconstruction")))

# create ggplot
gg_comparison <- ggplot(data = result_combn) + 
  geom_hline(yintercept = 1, color = "darkgrey", linetype = 2) +
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi, fill = method), alpha = 0.5) +
  geom_line(data = pcf_observed, aes(x = r, y = iso), col = "black", linewidth = 0.75) +
  scale_fill_manual(name = "", values = c("gamma-test" = "#AE0400", "Pattern reconstruction" = "#0085A9")) + 
  labs(x = expression(paste("Distance ", italic("r"), " in meters [m]")), 
       y = expression(paste("Pair-correlation function ", italic("g(r)")))) +
  theme_classic(base_size = 12) +  
  theme(legend.position = c(0.8, 0.8))

suppoRt::save_ggplot(plot = gg_comparison, path = "4_Figures/", filename = "Fig-S1.png",
                     dpi = dpi, width = width, height = height * 1/2, units = units, 
                     overwrite = FALSE)
