##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1_Functions/setup.R")

#### Load data ####

gamma_df <- readRDS("3_Data/gamma_test.rds")

reconstruction_df <- readRDS("3_Data/pattern_recon.rds")

torus_df <- readRDS("3_Data/torus_trans.rds")

walk_df <- readRDS("3_Data/habitat_random.rds")

combined_df <- dplyr::bind_rows(gamma = gamma_df, reconstruction = reconstruction_df, 
                                torus = torus_df, walk = walk_df, .id = "method") |> 
  tibble::as_tibble()

#### Summarize data ####

summarized_df <- dplyr::group_by(combined_df, method, species, fract_dim,
                                 association_strength, n_random) |> 
  dplyr::summarise(correct_mn = mean(correct) * 100, correct_sd = sd(correct) * 100,
                   false_mn = mean(false) * 100, false_sd = sd(false) * 100, .groups = "drop") |> 
  dplyr::mutate(method = factor(method, levels = c("gamma", "torus", "walk", "reconstruction")), 
                species = factor(species, levels = c(1, 2, 3, 4), 
                                 labels = c("CSR (positive association)", "Cluster process (positive association)", 
                                            "CSR (negative association)", "Cluster process (negative association)")),
                fract_dim = factor(fract_dim, levels = c(0.5, 1.65), 
                                   labels = c("Low fragmentation", "High fragmentation")), 
                n_random = factor(n_random, levels = c(99, 499), 
                                  labels = c("n rand.: 99", "n rand.: 499")))

dplyr::group_by(summarized_df, fract_dim) |> 
  dplyr::summarise(correct_mn = mean(correct_mn), false_mn = mean(false_mn))

dplyr::group_by(summarized_df, n_random) |> 
  dplyr::summarise(correct_mn = mean(correct_mn), false_mn = mean(false_mn))

#### Setup plot globals ####

color_scale <- c("gamma" = "#df4e25", "torus" = "#007aa1",
                 "walk" = "#41b282", "reconstruction" = "#fcb252")

color_alpha <- 0.15

size_base <- 12
size_point <- 1.75

#### Create dummy plot for legeend ####

ggplot_dummy <- data.frame(method = levels(summarized_df$method),
                           association_strength = c(0, 0, 0, 0), correct_mn = c(1, 1, 1, 1)) |> 
  ggplot(aes(x = association_strength,  y = correct_mn, color = method)) + 
  geom_line(alpha = color_alpha) + 
  geom_point(size = size_point) + 
  scale_color_manual(name = "", values = color_scale, 
                     labels = c("gamma" = "gamma-test", "torus" = "Torus-translation test", 
                                "walk" = "Randomised-habitats procedure", "reconstruction" = "Pattern reconstruction")) +
  guides(color = guide_legend(nrow = 1, byrow = FALSE)) + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom")

#### Create ggplot correct detections ####

ggplot_correct_list <- purrr::map(levels(summarized_df$species), function(i) {
  
  if (i %in% c("CSR (positive association)", "Cluster process (positive association)")) {
    strip_text_x <- element_text(hjust = 0.5)
    axis_text_x <- element_blank()
  } else {
    strip_text_x <- element_blank()
    axis_text_x <- NULL
  }
  
  if (i %in% c("Cluster process (positive association)", "Cluster process (negative association)")) {
    strip_text_y <- element_text(hjust = 0.5)
    axis_text_y <- element_blank()
  } else {
    strip_text_y <- element_blank()
    axis_text_y <- NULL
  }
  
  dplyr::filter(summarized_df, species == i) |> 
    ggplot(aes(x = association_strength, y = correct_mn, color = method)) + 
    geom_hline(yintercept = 0.0, color = "grey", linetype = 3) +  
    geom_hline(yintercept = 50, color = "grey", linetype = 3) +  
    geom_hline(yintercept = 100, color = "grey", linetype = 3) +  
    geom_vline(xintercept = 0.5, color = "grey", linetype = 3) +  
    geom_line(alpha = color_alpha, linewidth = 0.75) +
    geom_point(size = size_point) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0.1, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    scale_color_manual(values = color_scale) +
    facet_grid(rows = dplyr::vars(n_random), cols  = dplyr::vars(fract_dim)) +
    labs(x = "", y = "") +
    theme_bw(base_size = size_base) + 
    theme(legend.position = "none", strip.background = element_blank(), 
          strip.text.x = strip_text_x, strip.text.y = strip_text_y, 
          axis.text.x = axis_text_x, axis.text.y = axis_text_y)
  
})

# a: "CSR (positive association)", b: "Cluster process (positive association)", 
# c: "CSR (negative association)", d: "Cluster process (negative association)"
ggplot_correct_total <- cowplot::plot_grid(plotlist = ggplot_correct_list, 
                                           labels = c("a)", "b)", "c)", "d)"),
                                           label_fontface = "plain")

ggplot_correct_total <- cowplot::ggdraw(ggplot_correct_total, xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05)) + 
  cowplot::draw_label("Habitat association strength", x = 0.5, y = 0, vjust = -0.5, angle = 0, size = size_base) + 
  cowplot::draw_label("CSR", x = 0.275, y = 1, size = size_base) + cowplot::draw_label("Clustered", x = 0.75, y = 1, size = size_base) + 
  cowplot::draw_label("Correct association detected", x = -0.025, y = 0.5, angle = 90, size = size_base) +
  cowplot::draw_label("Positive association [%]", x = 0.0, y = 0.75, angle = 90, size = size_base) + 
  cowplot::draw_label("Negative association [%]", x = 0.0, y = 0.25, angle = 90, size = size_base)

ggplot_correct_total <- cowplot::plot_grid(ggplot_correct_total, cowplot::get_legend(ggplot_dummy),
                                           nrow = 2, ncol = 1, rel_heights = c(0.95, 0.05))

#### Create ggplot false detections ####

ggplot_false_list <- purrr::map(levels(summarized_df$species), function(i) {
  
  if (i %in% c("CSR (positive association)", "Cluster process (positive association)")) {
    strip_text_x <- element_text(hjust = 0)
    axis_text_x <- element_blank()
  } else {
    strip_text_x <- element_blank()
    axis_text_x <- NULL
  }
  
  if (i %in% c("Cluster process (positive association)", "Cluster process (negative association)")) {
    strip_text_y <- element_text(hjust = 0)
    axis_text_y <- element_blank()
  } else {
    strip_text_y <- element_blank()
    axis_text_y <- NULL
  }
  
  dplyr::filter(summarized_df, species == i) |> 
    ggplot(aes(x = association_strength, y = false_mn, color = method, group = method)) + 
    geom_hline(yintercept = 0.0, color = "grey", linetype = 3) +  
    geom_hline(yintercept = 50, color = "grey", linetype = 3) +  
    geom_hline(yintercept = 100, color = "grey", linetype = 3) + 
    geom_vline(xintercept = 0.5, color = "grey", linetype = 3) +  
    geom_line(alpha = color_alpha, linewidth = 0.75) + 
    geom_point(size = size_point) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0.1, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    scale_color_manual(values = color_scale) +
    facet_grid(rows = dplyr::vars(n_random), cols  = dplyr::vars(fract_dim)) + 
    labs(x = "", y = "") +
    theme_bw(base_size = size_base) + 
    theme(legend.position = "none", strip.background = element_blank(), 
          strip.text.x = strip_text_x, strip.text.y = strip_text_y, 
          axis.text.x = axis_text_x, axis.text.y = axis_text_y)
  
})

# a = gamma; b = reconstruction; c = torus; d = walk
ggplot_false_total <- cowplot::plot_grid(plotlist = ggplot_false_list, labels = c("a)", "b)", "c)", "d)"),
                                         label_fontface = "plain")

ggplot_false_total <- cowplot::ggdraw(ggplot_false_total, xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05)) + 
  cowplot::draw_label("Habitat association strength", x = 0.5, y = 0, vjust = -0.5, angle = 0, size = size_base) + 
  cowplot::draw_label("CSR", x = 0.25, y = 1, size = size_base) + cowplot::draw_label("Clustered", x = 0.75, y = 1, size = size_base) + 
  cowplot::draw_label("False association detected", x = -0.025, y = 0.5, angle = 90, size = size_base) +
  cowplot::draw_label("Positive association [%]", x = 0.0, y = 0.75, angle = 90, size = size_base) + 
  cowplot::draw_label("Negative association [%]", x = 0.0, y = 0.25, angle = 90, size = size_base)

ggplot_false_total <- cowplot::plot_grid(ggplot_false_total, cowplot::get_legend(ggplot_dummy),
                                         nrow = 2, ncol = 1, rel_heights = c(0.95, 0.05))

#### Summarize results ####

diff_frag <- summarized_df |> 
  dplyr::group_by(fract_dim) |>
  dplyr::group_split()

# x: Low fragmentation; y: High fragmentation
t.test(x= diff_frag[[1]]$correct_mn, y= diff_frag[[2]]$correct_mn)
t.test(x = diff_frag[[1]]$false_mn, y = diff_frag[[2]]$false_mn)

diff_null <- summarized_df |> 
  dplyr::group_by(n_random) |>
  dplyr::group_split()

# x: 99; y: 499
t.test(x = diff_null[[1]]$correct_mn, y = diff_null[[2]]$correct_mn)
t.test(x = diff_null[[1]]$false_mn, y = diff_null[[2]]$false_mn)

aov(correct_mn ~ species + fract_dim + n_random + method, data = summarized_df) |> summary()
aov(false_mn ~ species + fract_dim + n_random + method, data = summarized_df) |> summary()

#### Save ggplots

suppoRt::save_ggplot(plot = ggplot_correct_total, filename = "4_Figures/Fig-4.png", overwrite = T, 
                     dpi = dpi, height = width * 0.75, width = height, units = units)

suppoRt::save_ggplot(plot = ggplot_false_total, filename = "4_Figures/Fig-S4.png", overwrite = T, 
                     dpi = dpi, height = width * 0.75, width = height, units = units)

