##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

# Purpose of script: 

source("1_Functions/setup.R")

source("1_Functions/create_simulation_pattern.R")
source("1_Functions/create_simulation_species.R")

# set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

#### Create example data ####

# a = gamma; b = torus; c = walk; d = reconstruction

# create landscape
simulation_landscape <- NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows, 
                                      resolution = resolution, fract_dim = 1.5, 
                                      verbose = FALSE, cPrintlevel = 0, user_seed = 42) %>%
  terra::rast() %>% 
  shar::classify_habitats(n = 5, style = "fisher")

# create pattern with 4 species
simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 100, 
                                                association_strength = 0.35)

# pick species 2 as example
example_species <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2)

#### Randomize data ####

# fit clustered pattern to data
gamma_test <- shar::fit_point_process(spatstat.geom::unmark(example_species), n_random = 1,
                                      process = "cluster")

# translate habitats
torus_trans <- shar::translate_raster(raster = simulation_landscape, steps_x = 10, steps_y = 25)

# randomize habitats
random_walk <- shar::randomize_raster(raster = simulation_landscape, n_random  = 1)

# reconstruct pattern
pattern_recon <- shar::reconstruct_pattern(pattern = spatstat.geom::unmark(example_species), 
                                           method = "cluster", n_random = 1, max_runs = 2500)

#### Convert all to data.frames ####

# convert to tibble
gamma_test_df <- tibble::as_tibble(spatstat.geom::as.data.frame.ppp(gamma_test$randomized[[1]]))

torus_trans_df <- tibble::as_tibble(terra::as.data.frame(torus_trans$randomized[[1]], xy = TRUE))

random_walk_df <- tibble::as_tibble(raster::as.data.frame(random_walk$randomized[[1]], xy = TRUE))

pattern_recon_df <- tibble::as_tibble(spatstat.geom::as.data.frame.ppp(pattern_recon$randomized[[1]]))

simulation_landscape_df <- terra::as.data.frame(simulation_landscape, xy = TRUE)

example_species_df <- tibble::as_tibble(spatstat.geom::as.data.frame.ppp(example_species))

#### Setup ggplot globals ####

# set point size
size_point <- 0.5

size_base <- 10.0

color_point <- "grey"

#### Create single ggplots ####

ggplot_observed <- ggplot() +
  geom_raster(data = simulation_landscape_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = example_species_df, aes(x = x, y = y), size = size_point, color = color_point) +
  scale_fill_viridis_d() +
  theme_classic(base_size = size_base) + 
  labs(title = "Observed data") + 
  theme(aspect.ratio = 1, legend.position = "none", plot.title = element_text(vjust = -5, size = size_base),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        plot.margin = margin(5, 0, 0, 2.5, "mm"))

ggplot_gamma <- ggplot() +
  geom_raster(data = simulation_landscape_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = gamma_test_df, aes(x = x, y = y), size = size_point, color = color_point) +
  scale_fill_viridis_d() +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none",
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "mm"))

ggplot_torus <- ggplot() +
  geom_raster(data = torus_trans_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = example_species_df, aes(x = x, y = y), size = size_point, color = color_point) +
  scale_fill_viridis_d() +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none", 
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "mm"))

ggplot_walk <- ggplot() +
  geom_raster(data = random_walk_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = example_species_df, aes(x = x, y = y), size = size_point, color = color_point) +
  scale_fill_viridis_d() +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none", 
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "mm"))

ggplot_recon <- ggplot() +
  geom_raster(data = simulation_landscape_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = pattern_recon_df, aes(x = x, y = y), size = size_point, color = color_point) +
  scale_fill_viridis_d() +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none", 
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "mm"))

#### Create total plot ####

ggplot_random <- cowplot::plot_grid(ggplot_gamma, ggplot_torus, ggplot_walk, ggplot_recon,
                                    nrow = 2, ncol = 2, labels = c("a)", "b)", "c)", "d)"), 
                                    label_fontface = "plain", label_size = size_base, 
                                    label_y = 0.9, label_x = 0.05)

ggplot_total <- cowplot::plot_grid(ggplot_observed, ggplot_random, ncol = 2)

### Save ggplot ####

suppoRt::save_ggplot(plot = ggplot_total, filename = "4_Figures/Fig-1.pdf", overwrite = T, 
                     dpi = dpi, height = height * 0.5, width = width, units = units)
