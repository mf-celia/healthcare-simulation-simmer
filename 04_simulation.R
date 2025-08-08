# 04_simulation.R

# This script runs the core discrete event simulation (DES) model for primary care in Murcia
# using the simmer package. 

# Key features of this simulation:
# - Each professional has a limited number of daily consultation slots
# - Patients are scheduled based on availability and urgency
# - Urgent patients bypass the slot logic and are always attended
# - Regular patients retry the next day if no slot is available
# - Global variables track slot usage per center and professional type
# - Simulation runs for 5 full working days (7 hours activity per day)

# This script sources '03_simulation_prep.R', which prepares the input data 
# and generates patient arrivals. Alternatively, input files can be loaded 
# directly from './data/processed/' if preprocessing was done previously.

## Loading required libraries ----
library(tidyverse)
library(stringr)
library(readxl)
library(purrr)
library(simmer)
library(simmer.plot)

# Setting processed data path
processed_data_path <- "./data/processed"

# 1. Loading pre-processing ------------------------------------------------

source("./03_simulation_prep.R")

# 2. Set up simulation environment and parameters -------------------------

env <- simmer("Murcia")
daily_minutes <- 420
total_time <- daily_minutes * 5  # simulate 5 full working days = 2100 minutes


# 3. Defining resources and global slot counters ----------------------------

unique_resources <- professionals_center_weights %>%
  mutate(resource_name = paste(center_id, professional_type, sep = "_")) %>%
  pull(resource_name) %>%
  unique()

for (res in unique_resources) {
  env <- add_global(env, paste0("slots_", res), 0)  # e.g., slots_01_GP
}


# 4. Define patient trajectory -----------------------------------------------

make_traj_factory <- function(year, center_id, prof_type, age, modality, priority, total_hours, sigma = 0.3) {
  mu <- 12
  if (prof_type == "GP" && age >= 65) mu <- mu + 5
  if (prof_type == "Pediatrician" && age < 1) mu <- mu + 5
  if (modality == "Remote") mu <- pmax(mu - 5, 2)
  if (modality == "Home") mu <- mu * 2
  
  meanlog <- log(mu) - 0.5 * sigma^2
  sdlog <- sigma
  
  resource_name <- paste(center_id, prof_type, sep = "_")
  slot_var <- paste0("slots_", resource_name)
  daily_slots <- round((total_hours / 5) * 60 / mu)
  
  # Main trajectory block ---
  trajectory(paste("traj", resource_name, sep = "_")) %>%
    
    # Set attributes ONCE at the beginning ---
    set_attribute("year", year) %>%
    set_attribute("center_id", as.numeric(center_id)) %>%
    set_attribute("modality", match(modality, c("In-person", "Remote", "Home"))) %>%
    set_attribute("age", age) %>%
    set_attribute("professional_type", match(prof_type, c("GP", "Nurse", "Pediatrician"))) |> 
    set_attribute("priority", priority) %>%
    
    # 4. Branch by priority (urgent vs regular) ---
    branch(
      option = function() ifelse(priority == 0, 1, 2),
      continue = c(TRUE, TRUE),
      
      # Path 1: URGENT patient (bypasses slot logic)
      trajectory("urgent_path") %>%
        seize(resource_name, 1, priority = priority) %>%
        simmer::timeout(function() rlnorm(1, meanlog, sdlog)) %>%
        release(resource_name, 1),
      
      # Path 2: REGULAR patient (checks slots, retries if needed)
      trajectory("regular_path") %>%
        set_attribute("retries", 0) %>%
        branch(
          option = function() {
            slots_used <- get_global(env, slot_var)
            if (slots_used >= daily_slots) return(1) else return(2)
          },
          continue = c(TRUE, TRUE),
          
          # Sub-Path 2.1: No slots → wait and retry next day
          trajectory("wait_retry") %>%
            set_attribute("retries", 1, mod = "+") %>%  # increment retries
            simmer::timeout(daily_minutes) %>%
            rollback(2),  # go back to slot check branch
          
          # Sub-Path 2.2: Slot available → attend
          trajectory("attend") %>%
            set_global(slot_var, 1, mod = "+") %>%
            seize(resource_name, 1, priority = priority) %>%
            simmer::timeout(function() rlnorm(1, meanlog, sdlog)) %>%
            release(resource_name, 1)
        )
    )
}


# 5. Add resources with capacity based on total weekly hours --------------

pwalk(
  professionals_center_weights %>%
    mutate(capacity = pmax(1, round(total_hours / 37.5))),
  function(center_id, professional_type, capacity, ...) {
    resource_name <- paste(center_id, professional_type, sep = "_")
    env <<- add_resource(env, resource_name, capacity)
  }
)


# 6. Add patient generators using arrival data ----------------------------

pwalk(
  arrivals %>%
    dplyr::select(id, year, center_id, priority, professional_type, age, modality, arrival_time, total_hours),
  function(id, year, center_id, priority, professional_type, age, modality, arrival_time, total_hours, ...) {
    gen_name <- paste0("p_", id)
    env <<- add_generator(
      env,
      name_prefix = gen_name,
      trajectory = make_traj_factory(year, center_id, professional_type, age, modality, priority, total_hours),
      distribution = at(arrival_time),
      mon = 2
    )
  }
)

# 7. Reset slot counters at the start of each simulated day ---------------

reset_times <- seq(0, total_time, by = daily_minutes)

for (res in unique_resources) {
  slot_var <- paste0("slots_", res)
  env <- add_generator(
    env,  
    name_prefix = paste0("reset_", res),
    trajectory = trajectory() %>%
      set_global(slot_var, 0),  # reset slot usage for the resource
    distribution = at(reset_times),
    mon = 2
  )
}


# Let's run! --------------------------------------------------------------

invisible(env %>% run(until = total_time))

## Simulation results ----

sim_resources <- get_mon_resources(env) |> 
  write_csv(file.path(processed_data_path, "sim_resources.csv"))

sim_attributes <- get_mon_attributes(env) |> 
  write_csv(file.path(processed_data_path, "sim_attributes.csv"))

sim_arrivals <- get_mon_arrivals(env) |> 
  write_csv(file.path(processed_data_path, "sim_arrivals.csv"))

