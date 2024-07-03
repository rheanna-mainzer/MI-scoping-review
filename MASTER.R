# MASTER
# Run this script to conduct the analysis for the scoping review
# Written by R Mainzer, Dec 2023
# ------------------------------------------------------------------------------

# Load packages
library(dplyr)
library(flextable)
library(forcats)
library(ggplot2)
library(gridExtra)
library(gt)
library(gtsummary)
library(haven)
library(here)
library(labelled)
library(readxl)
library(stringr)
library(tidyr)
library(writexl)
library(xtable)

# Clean data
source(here("clean_dat.R"))

# Create tables
source(here("tables_for_paper.R"))

# Create Figure 2
source(here("figures_for_paper.R"))

