# MASTER
# Run this script to conduct the analysis for the scoping reivew
# Written by R Mainzer, Dec 2023
# ------------------------------------------------------------------------------

# Load packages
library(dplyr)
library(flextable)
library(forcats)
library(gt)
library(gtsummary)
library(haven)
library(labelled)
library(readxl)
library(stringr)
library(tidyr)
library(writexl)
library(xtable)

# Set working directory
setwd("C:/Users/rheanna.mainzer/OneDrive - Murdoch Children's Research Institute/MCRI/MI research/Scoping review/Analysis")

# Clean data
source("clean_dat.R")

# Create tables
source("tables_for_paper.R")
