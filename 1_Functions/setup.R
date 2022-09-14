###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    mhk.hesselbarth@gmail.com   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Load libraries ####

# Packages
library(cowplot)
library(maptools)
library(NLMR)
library(rslurm)
library(shar)
library(sf)
library(spatstat.geom)
library(spatstat.random)
library(suppoRt)
library(terra)
library(tidyverse)

#### Init simulation environment ####

# run `file.path(R.home("bin"), "Rscript")`
rscript_path <- "/opt/sw/rev/21.12/haswell/gcc-9.3.0/r-4.1.1-kwp4zk/rlib/R/bin/Rscript"

# number of columns and rows for neutral landscape
number_coloumns <- 50 

number_rows <- 50

# resolution of neutral landscape
resolution <- 20

# fragmentation levels
fract_dim <- c(0.5, 1.0, 1.5)

# approximate number of points for each species
number_points <- 100

# number of randomized habitat maps / point patterns
n_random <- c(19, 39, 199)

# number of habitats
n <- 5 # c(3, 5, 10)

# association strength sequence
association_strength <- seq(from = 0.05, to = 1, by = 0.15)

# number of simulation runs
iterations <- 25

# number of iterations pattern reconstruction
max_runs <- 10000

# threshold for fast computation of summary functions
comp_fast <- 0

# threshold to stop reconstruction if no change occurred
no_change <- 5000

# ggplot settings
dpi <- 900

height <- 297

width <- 210

units <- "mm"
