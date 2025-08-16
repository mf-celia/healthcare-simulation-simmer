# 03_simulation_prep.R

# This script prepares all necessary data and parameters 
# for running the discrete-event simulation (DES) model 
# of primary care in the Region of Murcia using the simmer package.
#
# It includes:
# - Loading and cleaning population and consultation datasets
# - Web scraping and processing of professional schedules
# - Estimating center-level consultation capacities
# - Distributing ordinary and emergency visits across centers
# - Generating patient-level arrivals for the simulation
#
# The final output is a clean dataset of individual arrivals 
# saved to './data/processed/arrivals.csv' to be used by the DES model.


## Loading required libraries ----

library(tidyverse)
library(stringr)
library(readxl)
library(purrr)


# 1. Load custom functions ------------------------------------------------

source("./01_data_cleaning.R")
source("./02_arrival_generation.R")


# 2. Load raw data and apply data cleaning --------------------------------

# Setting raw and processed data paths
raw_data_path <- "./data/raw"
processed_data_path <- "./data/processed"

# Reading original Excel files from SIAP
list_data <- obtain_data(raw_data_path)

## Population ----

# Cleaning and harmonising population data
population <- list_data[["pob-asignada-siap"]] %>% 
  rename(
    year = AÒo,                         # Year of observation
    area = "¡rea de salud",             # Health area name
    age_group = "Grupos Quinquenales",  # Age group (quinquennial)
    sex = Sexo,                         # Sex (coded)
    population = "N˙mero"               # Population count
  ) %>%
  mutate(
    # Cleaning area names by removing prefix like "Area I: ..."
    area = str_sub(area, str_locate(area, ":")[1] + 2, str_length(area)),
    
    # Fixing some known encoding issues and recode specific age ranges
    age_group = case_when(
      age_group == "45905" ~ "5-9",
      age_group == "41913" ~ "10-14",
      age_group == "95 y m·s" ~ "95+",
      TRUE ~ age_group
    ),
    
    # Recoding sex to numeric: 1 = Male, 2 = Female
    sex = if_else(sex == "VarÛn", 0, 1)
  ) %>%
  dplyr::select(year, area, age_group, sex, population)

# Saving processed population data
write_csv(population, file.path(processed_data_path, "population.csv"))

## Consultations ----

# Loading and cleaning primary care consultation records from SIAP
consultations <- list_data[["visitas-siap"]] %>%
  rename(
    year = AÒo,                         # Year of observation
    area = "¡rea de salud",             # Health area name
    age_group = "Grupos Quinquenales",  # Age group (quinquennial)
    professional_type = Profesional,    # Professional type (GP, pediatrician, nurse)
    modality = Lugar,                   # Place/type of consultation (center, home, teleconsultation)
    n_consultations = Consultas         # Number of consultations
  ) %>%
  mutate(
    # Cleaning area names (remove prefix "Área X: ")
    area = str_remove(area, "^[^:]+: "),
    
    # Fixing codification errors in age groups
    age_group = case_when(
      age_group == "45905" ~ "5-9",
      age_group == "41913" ~ "10-14",
      age_group == "95 y m·s" ~ "95+",
      age_group == "Sin informar" ~ NA,
      TRUE ~ age_group
    ),
    
    n_consultations = as.numeric(n_consultations),
    
    # Recoding professional type
    professional_type = recode(professional_type,
                               "Medicina de familia" = "GP",
                               "PediatrÌa" = "Pediatrician",
                               "EnfermerÌa" = "Nurse"),
    
    # Recoding consultation modality
    modality = recode(modality,
                      "Centro" = "In-person",        
                      "Teleconsulta" = "Remote",  
                      "Domicilio" = "Home")     
  ) %>%
  filter(!is.na(professional_type),
         !is.na(age_group)) %>%
  dplyr::select(year, area, age_group, professional_type, modality, n_consultations) %>%
  
  # Fill in all missing combinations with 0 consultations
  complete(year, area, age_group, professional_type, modality,
           fill = list(n_consultations = 0))

# Saving processed consultations data
write_csv(consultations, file.path(processed_data_path, "consultations.csv"))

## Emergencies ----

# Loading and cleaning primary care emergency consultations
emergencies <- list_data[["urgencias-siap"]] %>%
  rename(
    year = AÒo,                         # Year of observation
    area = "¡rea de salud",             # Health area name
    age_group = "Grupos Quinquenales",  # Age group (quinquennial)
    n_emerg = "Atendidas por Medicina"  # Number of emergencies
  ) %>%
  mutate(
    # Cleaning area names (remove prefix "Área X: ")
    area = str_remove(area, "^[^:]+: "),
    
    # Fixing codification errors in age groups
    age_group = case_when(
      age_group == "45905" ~ "5-9",
      age_group == "41913" ~ "10-14",
      age_group == "95 y m·s" ~ "95+",
      age_group == "Sin informar" ~ NA,
      TRUE ~ age_group
    ),
    
    n_emerg = as.numeric(n_emerg)
  ) %>%
  filter(!is.na(age_group)) %>%
  dplyr::select(year, area, age_group, n_emerg) %>%
  
  # Fill in all missing combinations with 0 consultations
  complete(year, area, age_group,
           fill = list(n_emerg = 0))

# Saving processed emergencies data
write_csv(emergencies, file.path(processed_data_path, "emergencies.csv"))


## Primary care centers ----

# Loading and cleaning data on the number of primary care centers by area and type

primary_care_centers <- list_data[["centros-siap"]] %>%
  rename(
    year = AÒo,                           # Year of observation
    area = "¡rea de salud",               # Health area name
    center_type = "Tipo de centro",       # Type of center (health center or local clinic)
    n_centers = "N∫ de centros"           # Number of centers
  ) %>%
  mutate(
    # Cleaning area names (remove prefix like "Área X: ")
    area = str_remove(area, "^[^:]+: "),
    
    # Ensuring number of centers is numeric
    n_centers = as.numeric(n_centers),
    
    # Recoing center types
    center_type = recode(center_type,
                         "Centros de salud" = 1,      # Health center
                         "Consultorios locales" = 2) # Local clinic
  ) %>%
  dplyr::select(year, area, center_type, n_centers)

primary_care_centers |> 
  mutate(sum(n_centers))

# Saving processed consultations data
write_csv(primary_care_centers, file.path(processed_data_path, "primary_care_centres.csv"))

## Profesionals ----

### Weekly working hours by individual ----
professionals_week_hours <- professionals_scrap |> 
  # Normalising professional types and area names
  mutate(
    professional_type = case_when(
      str_detect(professional_type, "Médico") ~ "GP",
      str_detect(professional_type, "Pediatra") ~ "Pediatrician",
      str_detect(professional_type, "Enfermería") ~ "Nurse",
      TRUE ~ NA_character_
    ),
    area = case_when(
      area_id == 1 ~ "Murcia Oeste",
      area_id == 2 ~ "Cartagena",
      area_id == 3 ~ "Lorca",
      area_id == 4 ~ "Noroeste",
      area_id == 5 ~ "Altiplano",
      area_id == 6 ~ "Vega media del Segura",
      area_id == 7 ~ "Murcia Este",
      area_id == 8 ~ "Mar Menor",
      area_id == 9 ~ "Vega Alta del Segura"
    )
  ) |> 
<<<<<<< HEAD
  filter(!is.na(professional_type)) |>  # Quitar el filtro de cupo
  
  # Creating cupo variable
  mutate(
    has_cupo = case_when(
      str_detect(extra_info, "Cupo") ~ TRUE,  # 
      is.na(extra_info) | extra_info == "" ~ runif(n()) <= 0.8,  # Considering 80% professionals attending patients
      TRUE ~ FALSE  
    )
  ) |> 
  
  # Filtrar solo profesionales con cupo
  filter(has_cupo) |> 
=======
  filter(!is.na(professional_type)) |> 
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978
  
  # Estimating weekly hours from schedule strings
  mutate(week_hours = week_hours(schedule)) |> 
  
<<<<<<< HEAD
  # Imputing missing or invalid hours with center-level mean
=======
  # Imputeing missing or invalid hours with center-level mean
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978
  group_by(center_id) |> 
  mutate(
    mean_center = mean(week_hours[week_hours > 0], na.rm = TRUE),
    week_hours = ifelse(week_hours <= 0 | is.na(week_hours), mean_center, week_hours)
  ) |> 
  ungroup() |> 
  
  # Further impute with professional-type mean if still missing
  group_by(professional_type) |> 
  mutate(
    mean_type = mean(week_hours, na.rm = TRUE),
    week_hours = ifelse(is.na(week_hours), mean_type, week_hours)
  ) |> 
  ungroup()

<<<<<<< HEAD

=======
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978
### Professionals – Aggregated hours and weights by center ----

professionals_center_weights <- professionals_week_hours |> 
  # Aggregating total weekly hours by area, center and professional type
<<<<<<< HEAD
  group_by(area, zbs, center_id, professional_type) |> 
  summarise(total_hours = sum(week_hours), .groups = "drop") |> 
  
  # Computing total hours per area and professional type
  group_by(area, zbs, professional_type) |> 
=======
  group_by(area, center_id, professional_type) |> 
  summarise(total_hours = sum(week_hours), .groups = "drop") |> 
  
  # Computing total hours per area and professional type
  group_by(area, professional_type) |> 
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978
  mutate(total_area_hours = sum(total_hours)) |> 
  ungroup() |> 
  
  # Calculating center-level weight within each area
  mutate(weight = total_hours / total_area_hours)

# Saving processed professionals data
write_csv(professionals_week_hours, file.path(processed_data_path, "professionals_week_hours.csv"))
write_csv(professionals_center_weights, file.path(processed_data_path, "professionals_center_weights.csv"))



# 3. Defining simulation parameters and components ------------------------

## Daily consultations per center and professional type ----

# This section calculates the expected number of daily consultations 
# for each healthcare center and professional type, using available 
# data on total annual consultations and professional distribution weights.
# 
# Two types of consultations are handled:
# - Ordinary consultations (regular primary care demand)
# - Emergency consultations (urgent visits, GP only)
#
# The number of daily consultations is distributed across centers 
# proportionally to their share of weekly working hours.

### Ordinary consultations ----
ord_consultations_center <- consultations |> 
  # Estimating average daily consultations (assuming 250 working days/year)
  mutate(
    day_consultations = round(n_consultations / 250),
    priority = 1  # Regular priority
  ) %>%
  
  # Merging with professional center weights
  left_join(professionals_center_weights, by = c("area", "professional_type")) %>%
  
  # Distributing consultations to each center by weight
  mutate(
    day_consultations_center = round(day_consultations * weight)
  ) %>%
  
  # Removing zero or missing values
  filter(day_consultations_center > 0 & !is.na(day_consultations_center))

## Emergency consultations (only for GPs) ----
emerg_consultations_center <- emergencies |> 
  # Estimating daily emergencies and set fixed attributes
  mutate(
    day_consultations = round(n_emerg / 250),
    modality = "In-person",
    priority = 0  # Urgent
  ) |> 
  
  # Merging with GP weights only
  left_join(
    professionals_center_weights |> filter(professional_type == "GP"),
    by = "area"
  ) |> 
  
  # Distributing emergency consultations to centers
  mutate(
    day_consultations_center = round(day_consultations * weight)
  ) |> 
  
  # Removing zero or missing values
  filter(day_consultations_center > 0 & !is.na(day_consultations_center)) |> 
  
  # Dropping unused column
  dplyr::select(-n_emerg)

# Saving processed daily activity data
write_csv(ord_consultations_center, file.path(processed_data_path, "ord_consultations_center.csv"))
write_csv(emerg_consultations_center, file.path(processed_data_path, "emerg_consultations_center.csv"))



## Synthetic patient arrivals for simulation ----

# This section creates a synthetic patient-level arrival data.frame
# based on the expected daily consultation volume per center
# and professional type, including both ordinary and emergency visits.
# It assigns each patient an arrival time, age (sampled from age group),
# modality (in-person, remote, home), and priority (urgent vs regular).
# Used as input for the discrete-event simulation model.
# The resulting dataset is saved to './data/processed/arrivals.csv'

set.seed(123)  # for reproducibility

arrivals <- bind_rows(
  ord_consultations_center,
  emerg_consultations_center
) |> 
<<<<<<< HEAD
=======
  filter(year == 2023) |> 
  
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978
  # For each consultation row, generate individual patients
  rowwise() %>%
  mutate(
    arrival_time = list(sort(runif(day_consultations_center, 0, 420))),  # 7h = 420 min
    age = list(floor(map_dbl(seq_len(day_consultations_center), ~ random_age(age_group)))),
    year = list(rep(year, day_consultations_center)),
    area = list(rep(area, day_consultations_center)),
    professional_type = list(rep(professional_type, day_consultations_center)),
    age_group = list(rep(age_group, day_consultations_center)),
    center_id = list(rep(as.numeric(center_id), day_consultations_center)),
    modality = list(rep(modality, day_consultations_center)),
    priority = list(rep(priority, day_consultations_center))
  ) %>%
  # Flattening the lists into individual rows
  unnest(c(year, area, center_id, arrival_time, priority, modality, professional_type, age, age_group)) %>%
  ungroup() |> 
  
  # Ordering and assign unique ID
  arrange(year, area, center_id, arrival_time) |> 
<<<<<<< HEAD
  mutate(pid = row_number())
=======
  mutate(id = row_number())
>>>>>>> 44b9ba7cadf5d19685db2d2dd1c55ec22c5ba978

# Saving to processed folder
write_csv(arrivals, file.path(processed_data_path, "arrivals.csv"))


