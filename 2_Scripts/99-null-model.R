##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

#### Import packages & functions ####

source("1_Functions/setup.R")

# source all functions in R_functions folder
list.files(path = "1_Functions/", full.names = TRUE) |> 
  purrr::walk(function(x) source(x))

# set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

#### Create example data ####
# create landscape
simulation_landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, 
                                      resolution = 20, fract_dim = 1.5, 
                                      verbose = FALSE, 
                                      cPrintlevel = 0) |> 
  terra::rast() |> 
  shar::classify_habitats(classes = 5)

# create pattern with 4 species
simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 50, 
                                                association_strength = 0.35)

# pick species 2 as example
example_species <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4)

#### Randomize data ####

# fit clustered pattern to data
gamma_test_39 <- shar::fit_point_process(spatstat.geom::unmark(example_species), 
                                         n_random = 39, process = "cluster")

gamma_test_199 <- shar::fit_point_process(spatstat.geom::unmark(example_species), 
                                          n_random = 199, process = "cluster")

pattern_recon_39 <- shar::reconstruct_pattern(spatstat.geom::unmark(example_species), 
                                              n_random = 39, max_runs = max_runs, 
                                              method = "cluster", comp_fast = comp_fast, 
                                              no_change = no_change)

pattern_recon_199 <- shar::reconstruct_pattern(spatstat.geom::unmark(example_species), 
                                               n_random = 199, max_runs = max_runs, 
                                               method = "cluster", comp_fast = comp_fast, 
                                               no_change = no_change)

random_list <- list(gamma_39 = gamma_test_39, gamma_199 = gamma_test_199, 
                    recon_39 = pattern_recon_39, recon_199 = pattern_recon_199)

suppoRt::save_rds(object = random_list, filename = "example_pattern_random.rds", 
                  path = "3_Data/", overwrite = FALSE)

#### Create ggplot #### 

random_list <- readRDS("3_Data/example_pattern_random.rds")

pcf_obs_df <- spatstat.explore::pcf.ppp(random_list$gamma_39$observed, divisor = "d", correction = "Ripley", 
                                     r = seq(from = 0, to = 250, length.out = 515)) |> 
  tibble::as_tibble()

# calculate pcf function 
pcf_rand_df <- purrr::map_dfr(random_list, function(i) {
  
  purrr::map_dfr(i$randomized, function(j) {
    spatstat.explore::pcf.ppp(j, divisor = "d", correction = "Ripley", 
                              r = seq(from = 0, to = 250, length.out = 515)) |> 
      tibble::as_tibble()}, .id = "itr")}, .id = "method") |> 
  tidyr::separate(method, sep = "_", into = c("method", "n_null")) |> 
  dplyr::group_by(method, n_null, r) |> 
  dplyr::summarise(lo = quantile(iso, probs = 0.025),
                   hi = quantile(iso, probs = 0.975), .groups = "drop") |> 
  dplyr::mutate(method = factor(method, levels = c("gamma", "recon"),
                                labels = c("gamma" = "gamma-test", "recon" = "Pattern reconstruction")), 
                n_null = factor(n_null, levels = c("39", "199"), 
                                labels = c("39" = "n random: 39", "199" = "n random: 199")))

gg_plot_null <- ggplot(data = pcf_rand_df) + 
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi, fill = n_null), alpha = 0.5) + 
  geom_line(data = pcf_obs_df, aes(x = r, y = theo), col = "grey", linetype = 2, linewidth = 0.5) +
  geom_line(data = pcf_obs_df, aes(x = r, y = iso)) +
  facet_wrap(. ~ method, nrow = 2) + 
  scale_fill_manual(name = "", values = c("n random: 39" = "#AE0400", "n random: 199" = "#0085A9")) +
  labs(x = expression(paste("Distance ", italic("r"), " in meters [m]")), 
       y = expression(paste("Pair-correlation function ", italic("g(r)")))) +
  theme_classic(base_size = 12) +
  theme(legend.position = c(0.9, 0.9), 
        strip.background = element_blank(), strip.text = element_text(hjust = 0))

suppoRt::save_ggplot(plot = gg_plot_null, path = "4_Figures/", filename = "Fig-A3.png",
                     dpi = dpi, width = width, height = height * 1/2, units = units, 
                     overwrite = FALSE)
