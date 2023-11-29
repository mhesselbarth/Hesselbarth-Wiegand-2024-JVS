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
example_species <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4)

#### Randomize data ####

# fit clustered pattern to data
gamma_test_99 <- shar::fit_point_process(spatstat.geom::unmark(example_species), 
                                         n_random = 99, process = "cluster")

gamma_test_499 <- shar::fit_point_process(spatstat.geom::unmark(example_species), 
                                          n_random = 499, process = "cluster")

# Pattern reconstruction

pattern_recon_99 <- shar::reconstruct_pattern(spatstat.geom::unmark(example_species), 
                                              n_random = 99, max_runs = max_runs, 
                                              method = "cluster", no_change = no_change)

pattern_recon_499 <- shar::reconstruct_pattern(spatstat.geom::unmark(example_species), 
                                               n_random = 499, max_runs = max_runs, 
                                               method = "cluster", no_change = no_change)

random_list <- list(gamma_99 = gamma_test_99, gamma_499 = gamma_test_499, 
                    recon_99 = pattern_recon_99, recon_499 = pattern_recon_499)

suppoRt::save_rds(object = random_list, filename = "example_pattern_random.rds", 
                  path = "3_Data/", overwrite = FALSE)

#### Create ggplot #### 

random_list <- readRDS("3_Data/example_pattern_random.rds")

pcf_obs_df <- spatstat.explore::pcf.ppp(random_list$gamma_99$observed, divisor = "d", correction = "Ripley", 
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
                n_null = factor(n_null, levels = c("99", "499"), 
                                labels = c("99" = "n random: 99", "499" = "n random: 499")))

gg_plot_null <- ggplot(data = pcf_rand_df) + 
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi, fill = n_null), alpha = 0.25) +
  geom_line(aes(x = r, y = lo, color = n_null), alpha = 0.5) + 
  geom_line(aes(x = r, y = hi, color = n_null), alpha = 0.5) + 
  geom_line(data = pcf_obs_df, aes(x = r, y = theo), col = "grey", linetype = 2, linewidth = 0.5) +
  geom_line(data = pcf_obs_df, aes(x = r, y = iso)) +
  facet_wrap(. ~ method, nrow = 2) + 
  scale_color_manual(name = "", values = c("n random: 99" = "#AE0400", "n random: 499" = "#0085A9")) +
  scale_fill_manual(name = "", values = c("n random: 99" = "#AE0400", "n random: 499" = "#0085A9")) +
  labs(x = expression(paste("Distance ", italic("r"), " in meters [m]")), 
       y = expression(paste("Pair-correlation function ", italic("g(r)")))) +
  theme_bw(base_size = 12) +
  theme(legend.position = c(0.85, 0.9), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(), strip.text = element_text(hjust = 0))

suppoRt::save_ggplot(plot = gg_plot_null, path = "4_Figures/", filename = "Fig-S5.png",
                     dpi = dpi, width = width, height = height * 1/2, units = units, 
                     overwrite = FALSE)
