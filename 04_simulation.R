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

<<<<<<< HEAD

# 2. Set up simulation environment and parameters -------------------------

daily_minutes <- 420
days_per_year <- 5
year_horizon  <- daily_minutes * days_per_year
drain_days    <- 10L


## 3. Resources & global slot counters -------------------------------------
resources_spec <- professionals_center_weights %>%
  dplyr::select(center_id, professional_type, total_hours) %>% 
  mutate(
    capacity      = pmax(1, round(total_hours / 37)),  # capacity in FTE (1 FTE = 37h/week)
    resource_name = paste(center_id, professional_type, sep = "_")
  )

# Unique resource names for later use
unique_resources <- resources_spec$resource_name

# 4. Defining patient trajectory -----------------------------------------------

make_traj_factory <- function(env, pid, year, center_id, prof_type, age, modality, priority, total_hours, sigma = 0.3) {
  
  # Average consultation time (minutes) adjustments based on conditions (patient-level service time)
  mu <- 12
  if (prof_type == "GP"           && age >= 65) mu <- mu + 5
  if (prof_type == "Pediatrician" && age < 1)   mu <- mu + 5
  if (modality == "Remote")       mu <- pmax(mu - 5, 2)
  if (modality == "Home")         mu <- mu * 2
  
  meanlog <- log(mu) - 0.5 * sigma^2
  sdlog   <- sigma
  
  resource_name  <- paste(center_id, prof_type, sep = "_")
  slot_var       <- paste0("slots_", resource_name)       # daily usage counter
  slot_limit_var <- paste0("limit_", resource_name)       # daily limit per resource
  
  trajectory(paste("traj", resource_name, sep = "_")) %>%
    # Store patient attributes for monitoring
    set_attribute("pid",               as.numeric(pid)) %>%
    set_attribute("year",              as.numeric(year)) %>%
    set_attribute("center_id",         as.numeric(center_id)) %>%
    set_attribute("modality",          as.numeric(match(modality, c("In-person","Remote","Home")))) %>%
    set_attribute("age",               as.numeric(age)) %>%
    set_attribute("professional_type", as.numeric(match(prof_type, c("GP","Nurse","Pediatrician")))) %>%
    set_attribute("priority",          as.numeric(priority)) %>%
    
    # Split: urgent patients (priority 0) vs regular patients
    branch(
      option   = function() ifelse(priority == 0, 1, 2),
      continue = c(TRUE, TRUE),
      
      # Path 1: URGENT → always attended immediately
=======
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
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978
      trajectory("urgent_path") %>%
        seize(resource_name, 1, priority = priority) %>%
        simmer::timeout(function() rlnorm(1, meanlog, sdlog)) %>%
        release(resource_name, 1),
      
<<<<<<< HEAD
      # Path 2: REGULAR patients → check daily slot availability and retry if full
      trajectory("regular_path") %>%
        set_attribute("sim_day",       function() floor(now(env) / daily_minutes)) %>%
        set_attribute("slots_used_in", function() get_global(env, slot_var)) %>%
        set_attribute("daily_slots",   function() get_global(env, slot_limit_var)) %>%  # for monitoring %>%
        
        branch(
          option = function() {
            slots_used  <- get_global(env, slot_var)
            slots_limit <- get_global(env, slot_limit_var)
            if (slots_used >= slots_limit) 1 else 2
          },
          continue = c(TRUE, TRUE),
          
          # 2.1 No slot available → wait 1 day and retry (with maximum retry limit)
          trajectory("wait_retry") %>%
            set_attribute("retries", 1, mod = "+") %>%
            
            # Check if maximum retries reached
            branch(
              option = function() {
                current_retries <- get_attribute(env, "retries")
                if (current_retries >= 5) return(1) else return(2)  # máximo 5 reintentos
              },
              continue = c(TRUE, TRUE),
              
              # 2.1.1 Maximum retries reached → patient abandons
              trajectory("abandoned") %>%
                set_attribute("abandoned", 1),  # mark as abandoned
              
              # 2.1.2 Continue retrying
              trajectory("continue_retry") %>%
                simmer::timeout(daily_minutes + 0.01) %>%
                rollback(5, times = Inf)  # rollback to the slot availability check
            ),
          
          # 2.2 Slot available → attend
          trajectory("attend") %>%
            set_global(slot_var, 1, mod = "+") %>%  # reserve slot
            set_attribute("slots_used_out", function() get_global(env, slot_var)) %>%
            set_attribute("final_retries", function() get_attribute(env, "retries")) %>%
=======
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
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978
            seize(resource_name, 1, priority = priority) %>%
            simmer::timeout(function() rlnorm(1, meanlog, sdlog)) %>%
            release(resource_name, 1)
        )
    )
}


<<<<<<< HEAD
### Running a single year and return monitors with 'year' column
run_one_year <- function(yr, arrivals_year) {
  
  yr <- as.integer(yr)
  
  env <- simmer(paste0("Murcia_", yr))
  
  # Initialising global counters for all resources (daily usage)

  for (res in unique_resources) {
    env <- add_global(env, paste0("slots_", res), 0)
  }
  
  # Deriving avg_mu per resource from that year's arrivals, then a fixed daily limit per resource
  mu_by_res <- arrivals_year %>%
    mutate(
      mu_patient = {
        base <- 12
        base <- base + if_else(professional_type == "GP" & age >= 65, 5, 0)
        base <- base + if_else(professional_type == "Pediatrician" & age < 1, 5, 0)
        base <- base - if_else(modality == "Remote", 5, 0)
        base <- pmax(base, 2)
        base <- if_else(modality == "Home", base * 2, base)
        base
      }
    ) %>%
    group_by(center_id, professional_type) %>%
    summarise(avg_mu = mean(mu_patient, na.rm = TRUE), .groups = "drop")
  
  slots_limits <- resources_spec %>%
    dplyr::select(center_id, professional_type, total_hours, resource_name) %>%
    left_join(mu_by_res, by = c("center_id", "professional_type")) %>%
    mutate(
      avg_mu = coalesce(avg_mu, 12),  # fallback if no arrivals for that resource
      slots_per_day = pmax(1, round((total_hours / 37) * daily_minutes / avg_mu))
    )
  
  # Registering global daily limit for each resource
  for (i in seq_len(nrow(slots_limits))) {
    res_name <- slots_limits$resource_name[i]
    lim_var  <- paste0("limit_", res_name)
    env <- add_global(env, lim_var, as.numeric(slots_limits$slots_per_day[i]))
  }
 
# 5. Add resources (once per resource) ------------------------------------
  pwalk(
    resources_spec %>% dplyr::select(center_id, professional_type, capacity),
    function(center_id, professional_type, capacity, ...) {
      resource_name <- paste(center_id, professional_type, sep = "_")
      env <<- add_resource(env, resource_name, capacity)
    }
  )
  
  
# 6. Adding patient generators using arrival data ----------------------------
  pwalk(
    arrivals_year %>%
      dplyr::select(pid, year, center_id, priority, professional_type, age, modality, arrival_time, total_hours),
    function(pid, year, center_id, priority, professional_type, age, modality, arrival_time, total_hours, ...) {
      gen_name <- sprintf("p_%d_", pid)
      env <<- add_generator(
        env,
        name_prefix = gen_name,
        trajectory = make_traj_factory(
          env = env,
          pid = pid,
          year = year,
          center_id = center_id,
          prof_type = professional_type,
          age = age,
          modality = modality,
          priority = priority,
          total_hours = total_hours
        ),
        distribution = at(arrival_time),
        mon = 2
      )
    }
  )
  
# 7. Reset slot counters at the start of each simulated day ---------------

  sim_end <- year_horizon + drain_days * daily_minutes   
  reset_times <- seq(0, sim_end, by = daily_minutes)     
  
  for (res in unique_resources) {
    slot_var <- paste0("slots_", res)
    env <- add_generator(
      env,
      name_prefix = paste0("reset_", res),
      trajectory = trajectory() %>% set_global(slot_var, 0),
      distribution = at(reset_times),
      mon = 0
    )
  }
  
  
# 8. Running this year's environment ---------------

  # Run simulation for the year
  env %>% run(until = sim_end)
  
  # Return collected monitors tagged with the year
  list(
    attributes = get_mon_attributes(env) %>% mutate(year = yr),
    arrivals   = get_mon_arrivals(env)   %>% mutate(year = yr),
    resources  = get_mon_resources(env)  %>% mutate(year = yr)
  )
}

## Splitting arrivals by year and run each year independently ----
arrivals_list <- split(arrivals, arrivals$year)
results_list <- imap(arrivals_list, ~ run_one_year(yr = .y, arrivals_year = .x))


# Combine monitors into single data frames
sim_attributes <- map_dfr(results_list, "attributes")
sim_arrivals   <- map_dfr(results_list, "arrivals")
sim_resources  <- map_dfr(results_list, "resources")
=======
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
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978

