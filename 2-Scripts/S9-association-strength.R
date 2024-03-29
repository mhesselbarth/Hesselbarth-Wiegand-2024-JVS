##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1-Functions/setup.R")

#### Load data ####

gamma_df <- readRDS("3-Data/S9-gamma-test.rds")
reconstruction_df <- readRDS("3-Data/S9-pattern-recon.rds")
torus_df <- readRDS("3-Data/S9-torus-trans.rds")
walk_df <- readRDS("3-Data/S9-habitat-random.rds")

simulation_experiment_list <- readRDS("3-Data/S9-sim-experiment.rds")

combined_df <- dplyr::bind_rows(gamma = gamma_df, reconstruction = reconstruction_df, 
                                torus = torus_df, walk = walk_df, .id = "method") |> 
  tibble::as_tibble()

#### Summarize data ####

summarized_df <- dplyr::group_by(combined_df, method, species, fract_dim,
                                 association_strength, n_random) |> 
  dplyr::summarise(correct_mn = mean(correct) * 100, correct_sd = sd(correct) * 100,
                   false_mn = mean(false) * 100, false_sd = sd(false) * 100, .groups = "drop") |> 
  dplyr::mutate(correct_lo = correct_mn - correct_sd, correct_hi = correct_mn + correct_sd, 
                false_lo = false_mn - false_sd, false_hi = false_mn + false_sd) |> 
  dplyr::mutate(correct_lo = dplyr::case_when(correct_lo < 0 ~ 0, TRUE ~ correct_lo), 
                correct_hi = dplyr::case_when(correct_hi > 100 ~ 100, TRUE ~ correct_lo), 
                false_lo = dplyr::case_when(false_lo < 0 ~ 0, TRUE ~ false_lo), 
                false_hi = dplyr::case_when(false_hi > 100 ~ 100, TRUE ~ false_lo)) |>
  dplyr::mutate(method = factor(method, levels = c("gamma", "torus", "walk", "reconstruction")), 
                species = factor(species, levels = c(2, 4), 
                                 labels = c("2" = "Cluster process (positive association)", 
                                            "4" = "Cluster process (negative association)")),
                fract_dim = factor(fract_dim, levels = c(0.5, 1.65), 
                                   labels = c("0.5" = "Low fragmentation", "1.65" = "High fragmentation")), 
                n_random = factor(n_random, levels = c(99, 499), 
                                  labels = c("99" = "n rand.: 99", "499" = "n rand.: 499"))) |> 
  dplyr::select(method, species, fract_dim, n_random, association_strength, 
                correct_mn, correct_lo, correct_hi, 
                false_mn, false_lo, false_hi)

dplyr::group_by(summarized_df, fract_dim) |> 
  dplyr::summarise(correct_mn = mean(correct_mn), false_mn = mean(false_mn))

dplyr::group_by(summarized_df, n_random) |> 
  dplyr::summarise(correct_mn = mean(correct_mn), false_mn = mean(false_mn))

#### Setup plot globals ####

color_scale <- c("gamma" = "#df4e25", "torus" = "#007aa1",
                 "walk" = "#41b282", "reconstruction" = "#fcb252")

color_alpha <- 0.5

size_base <- 12
size_point <- 1.75

#### Create dummy plot for legeend ####

ggplot_dummy <- data.frame(method = levels(summarized_df$method),
                           association_strength = c(0, 0, 0, 0), correct_mn = c(1, 1, 1, 1)) |> 
  ggplot(aes(x = association_strength,  y = correct_mn, color = method)) + 
  geom_point(size = size_point) + 
  scale_color_manual(name = "", values = color_scale, 
                     labels = c("gamma" = "gamma-test", "torus" = "Torus-translation test", 
                                "walk" = "Randomised-habitats procedure", "reconstruction" = "Pattern reconstruction")) +
  guides(color = guide_legend(nrow = 1, byrow = FALSE)) + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom")

#### Create ggplot correct detections ####

ggplot_correct_list <- purrr::map(levels(summarized_df$species), function(i) {
  
  if (i == "Cluster process (positive association)") {
    strip_text_x <- element_text(hjust = 0.5)
    axis_text_x <- element_blank()
  } else {
    strip_text_x <- element_blank()
    axis_text_x <- NULL
  }
  
  dplyr::filter(summarized_df, species == i) |> 
    ggplot(aes(x = association_strength, y = correct_mn, color = method)) + 
    
    # adding guidelines
    geom_hline(yintercept = 0.0, color = "grey", linetype = 3) +  
    geom_hline(yintercept = 50, color = "grey", linetype = 3) +  
    geom_hline(yintercept = 100, color = "grey", linetype = 3) +  
    geom_vline(xintercept = 0.5, color = "grey", linetype = 3) +  
    
    # adding rates
    geom_line(alpha = color_alpha, linewidth = 0.75) +
    geom_point(size = size_point) +
    
    # # add errorbars
    # geom_errorbar(aes(ymin = correct_lo, ymax = correct_hi), width = 0.0, alpha = 0.5) +
    
    # facet wrap by species and null model
    facet_grid(rows = dplyr::vars(n_random), cols  = dplyr::vars(fract_dim)) +
    
    # set scales
    scale_x_continuous(limits = c(0, 1), breaks = seq(0.1, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    scale_color_manual(values = color_scale) +
    
    # adapt theme
    labs(x = "", y = "") +
    theme_bw(base_size = size_base) + 
    theme(legend.position = "none", strip.background = element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.text.x = strip_text_x, axis.text.x = axis_text_x)
  
})

# a: "Cluster process (positive association)"
# b: "Cluster process (negative association)"
ggplot_correct_total <- cowplot::plot_grid(plotlist = ggplot_correct_list, labels = c("(a)", "(b)"),
                                           nrow = 2, hjust = -0.1, label_fontface = "plain")

ggplot_correct_total <- cowplot::ggdraw(ggplot_correct_total, xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05)) + 
  cowplot::draw_label("Habitat association strength", x = 0.5, y = 0, vjust = -0.5, angle = 0, size = size_base) + 
  cowplot::draw_label("Correct association detected", x = -0.025, y = 0.5, angle = 90, size = size_base) +
  cowplot::draw_label("Positive association [%]", x = 0.0, y = 0.75, angle = 90, size = size_base) + 
  cowplot::draw_label("Negative association [%]", x = 0.0, y = 0.25, angle = 90, size = size_base)

ggplot_correct_total <- cowplot::plot_grid(ggplot_correct_total, cowplot::get_legend(ggplot_dummy),
                                           nrow = 2, ncol = 1, rel_heights = c(0.95, 0.05))

#### Plot pattern ####

habitat_df <- simulation_experiment_list[[50]]$habitat |> 
  terra::unwrap() |> 
  terra::as.data.frame(xy = TRUE)

gg_pattern <- as.data.frame(simulation_experiment_list[[50]]$pattern) |> 
  ggplot() + 
  geom_raster(data = habitat_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(aes(x = x, y = y, shape = factor(species_code)), size = 2.0) +
  geom_polygon(data = as.data.frame(simulation_experiment_list[[1]]$pattern$window), 
               aes(x = x, y = y), fill = NA, color = "black") +
  # facet_wrap(. ~ species_code) +
  coord_equal() +
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Demuth", n = 5, type = "discrete")) +
  scale_shape_manual(name = "", values = c("2" = 2, "4" = 4),
                     labels = c("2" = "Postive association (cluster)", "4" = "Negative association (cluster)")) +
  guides(fill = "none") +
  theme_void(base_size = size_base) + 
  theme(legend.position = "bottom", strip.text = element_blank())

# #### Create final plot ####
# 
# ggplot_final <- cowplot::plot_grid(ggplot_correct_total, gg_pattern, labels = c("", "(c)"), 
#                                    ncol = 2, hjust = -0.1, vjust = 20, label_fontface = "plain", 
#                                    rel_widths = c(0.8, 0.2))

#### Summarize results ####

diff_frag <- summarized_df |> 
  dplyr::group_by(fract_dim) |>
  dplyr::group_split()

# x: Low fragmentation; y: High fragmentation
t.test(x= diff_frag[[1]]$correct_mn, y= diff_frag[[2]]$correct_mn)

diff_null <- summarized_df |> 
  dplyr::group_by(n_random) |>
  dplyr::group_split()

# x: 99; y: 499
t.test(x = diff_null[[1]]$correct_mn, y = diff_null[[2]]$correct_mn)

aov(correct_mn ~ species + fract_dim + n_random + method, data = summarized_df) |> summary()

#### Save ggplots

suppoRt::save_ggplot(plot = gg_pattern, filename = "4-Figures/Fig-S9-1.png", 
                     dpi = dpi, height = height * 1/2, width = width, units = units, 
                     overwrite = FALSE)

suppoRt::save_ggplot(plot = ggplot_correct_total, filename = "4-Figures/Fig-S9-2.png", 
                     dpi = dpi, height = height * 2/3, width = width, units = units, 
                     overwrite = FALSE)

