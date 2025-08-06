# ===============================================================
# PRIMARY CARE SIMULATION – FIXED DAILY SLOTS PER PROFESSIONAL
# No agenda resets, no re-attempts. Patients are rejected if no slot available.
# ===============================================================

# 1. Set up simulation environment and parameters
env <- simmer("Murcia")
daily_minutes <- 420
total_time <- daily_minutes * 5  # simulate 5 full working days = 2100 minutes

# 2. Define resources and global slot counters
unique_resources <- professionals_center_weights %>%
  mutate(resource_name = paste(center_id, professional_type, sep = "_")) %>%
  pull(resource_name) %>%
  unique()

for (res in unique_resources) {
  env <- add_global(env, paste0("slots_", res), 0)  # e.g., slots_01_GP
}

# 3. Define patient trajectory with waiting and retry if no slot
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

  if (priority == 0) {
    # Urgent patients are always attended
    trajectory(paste("traj_urg", resource_name, sep = "_")) %>%
      # Set patient attributes
      set_attribute("year", year) %>% 
      set_attribute("center_id", center_id) %>%
      set_attribute("modality", match(modality, c("In-person", "Remote", "Home"))) %>%
      set_attribute("age", age) %>%
      set_attribute("professional_type", match(prof_type, c("GP", "Nurse", "Pediatrician"))) %>%
      set_attribute("priority", priority) %>%
      set_attribute("total_hours", total_hours) %>%
      # Seize resource and process
      seize(resource_name, 1, priority = priority) %>%
      simmer::timeout(function() rlnorm(1, meanlog, sdlog)) %>%
      release(resource_name, 1)
  } else {
    # Regular patients retry every day until attended
    trajectory(paste("traj", resource_name, sep = "_")) %>%
      # Set patient attributes at the beginning
      set_attribute("year", year) %>% 
      set_attribute("center_id", center_id) %>%
      set_attribute("modality", match(modality, c("In-person", "Remote", "Home"))) %>%
      set_attribute("age", age) %>%
      set_attribute("professional_type", match(prof_type, c("GP", "Nurse", "Pediatrician"))) %>%
      set_attribute("priority", priority) %>%
      set_attribute("total_hours", total_hours) %>%
      # Branch logic for slot availability
      branch(
        option = function() {
          slots_used <- get_global(env, slot_var)
          if (slots_used >= daily_slots) return(1) else return(2)
        },
        continue = c(TRUE, TRUE),

        # Path 1: no slots → wait 1 day, then retry
        trajectory("wait_retry") %>%
          simmer::timeout(daily_minutes) %>%
          rollback(2),

        # Path 2: slot available → attend
        trajectory("attend") %>%
          set_global(slot_var, 1, mod = "+") %>%
          seize(resource_name, 1, priority = priority) %>%
          simmer::timeout(function() rlnorm(1, meanlog, sdlog)) %>%
          release(resource_name, 1)
      )
  }
}

# 4. Add resources with capacity based on total weekly hours
pwalk(
  professionals_center_weights %>%
    mutate(capacity = pmax(1, round(total_hours / 37.5))),
  function(center_id, professional_type, capacity, ...) {
    resource_name <- paste(center_id, professional_type, sep = "_")
    env <<- add_resource(env, resource_name, capacity)
  }
)

# 5. Add patient generators using arrival data
pwalk(
  llegadas %>%
    dplyr::select(id, year, center_id, priority, professional_type, age, modality, arrival_time, total_hours),
  function(id, year, center_id, priority, professional_type, age, modality, arrival_time, total_hours, ...) {
    gen_name <- paste0("p_", id)
    env <<- add_generator(
      env,
      name_prefix = gen_name,
      trajectory = make_traj_factory(year, center_id, prof_type = professional_type, age, modality, priority, total_hours),
      distribution = at(arrival_time),
      mon = 2
    )
  }
)

# 6. Reset slot counters at the start of each simulated day
reset_times <- seq(0, total_time, by = daily_minutes)

for (res in unique_resources) {
  slot_var <- paste0("slots_", res)
  env <- add_generator(
    env,  
    name_prefix = paste0("reset_", res),
    trajectory = trajectory() %>%
      set_global(slot_var, 0),  # reset slot usage for the resource
    distribution = at(reset_times),    # Use at() instead of schedule()
    mon = 0
  )
}

# 7. Run the simulation
invisible(env %>% run(until = total_time))