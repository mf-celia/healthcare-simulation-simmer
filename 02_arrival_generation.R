# 02_arrival_generation.R
# This script contains functions and elements to generate synthetic patient attributes.

## Loading required libraries ----
library(dplyr)
library(stringr)
library(purrr)


# 1. Random age generation ---------------------------------------------------

# Given an age group label (e.g., "15-19", "65+"), return a randomly sampled age
# within the group bounds. For example, "65+" samples uniformly from 65 to 95.
random_age <- function(group_label) {
  if (is.na(group_label) || group_label == "") return(NA_real_)

  # If group has explicit range (e.g., "5-9")
  if (grepl("-", group_label)) {
    bounds <- as.numeric(strsplit(group_label, "-")[[1]])
    return(runif(1, bounds[1], bounds[2]))
  }

  # If group is open-ended (e.g., "95+")
  if (grepl("\\+$", group_label)) {
    lower <- as.numeric(sub("\\+$", "", group_label))
    return(runif(1, lower, 95))
  }

  # If not a group, try converting directly
  as.numeric(group_label)
}

