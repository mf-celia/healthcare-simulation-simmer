## Loading required libraries ----
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(sf)

# Setting processed data path
processed_data_path <- "./data/processed"

# 1. Loading simulation results ------------------------------------------------

#source("04_simulation.R")


# 2. Processing simulation results ----------------------------------------

## Processing resource log ----
resources <- sim_resources %>%
  mutate(
    center_id = as.numeric(str_extract(resource, "^\\d+")),
    professional_type = str_extract(resource, "(?<=_).*")
  ) %>%
  left_join(
    professionals_center_weights %>%
      dplyr::select(area, zbs, center_id) %>%
      distinct(),
    by = "center_id"
  ) 

write_csv(resources, file.path(processed_data_path, "resources.csv"))


## Processing attributes per patient ----
patient_attributes <- sim_attributes %>%
  filter(!str_starts(key, "slots_"),
         !str_starts(key, "limit_")) %>%
  group_by(name, key) %>%
  summarise(
    value = last(na.omit(value)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = key, 
    values_from = value,
    names_repair = "unique"
  )

## Processing arrivals ----
patient_arrivals <- sim_arrivals %>%
  filter(!str_starts(name, "reset_")) %>%
  mutate(
    finished = !is.na(end_time),
    waiting_time = end_time - start_time - activity_time,
    patient_id = str_extract(name, "p_(\\d+)_", group = 1)
  )


## Combining attributes and arrivals ----
combined_patients <- sim_arrivals %>%
  filter(!str_starts(name, "reset_")) %>%
  mutate(
    finished = !is.na(end_time),
    waiting_time = end_time - start_time - activity_time,
    patient_id = str_extract(name, "p_(\\d+)_", group = 1)
  ) %>%
  left_join(
    patient_attributes %>% 
      dplyr::select(-year),
    by = "name"
  ) %>%
  # Adding area informatin
  left_join(professionals_center_weights |> 
              distinct(area, zbs, center_id), 
            by = "center_id") %>%
  # Cleaning duplicated columns
  dplyr::select(
    name, start_time, end_time, activity_time, finished, waiting_time, area, zbs,
    pid, year, center_id, modality, age, professional_type, priority,
    sim_day, daily_slots, final_retries, retries, abandoned
  ) %>%
  mutate(
    # Attributes from patients that abandoned 
    abandoned = coalesce(abandoned, 0),  
    
    # Attributes form patients that have to retry 
    retries = coalesce(retries, 0), 
    
    # Attributes from patients that completed the simulation
    final_retries = coalesce(final_retries, retries),  
    
    # Attributes from non urgent patients (regular patients)
    sim_day = case_when(
      priority == 0 ~ NA_real_,  # priority patients do not have sim days
      TRUE ~ coalesce(sim_day, 0)  # regular patients take 0 as sim_day if no sim_day
    ),
    daily_slots = case_when(
      priority == 0 ~ NA_real_,  # urgen patients do not have daily slots
      TRUE ~ coalesce(daily_slots, 0)  # regular patients 
    ),
    
    # Setting characteristics: modality, professional, age group, etc.
    modality_label = case_when(
      modality == 1 ~ "In-person",
      modality == 2 ~ "Remote", 
      modality == 3 ~ "Home",
      TRUE ~ "Unknown"
    ),
    age_group = case_when(
      age < 15 ~ "0-14",
      age >= 15 & age < 65 ~ "15-64",
      age >= 65 ~ "65+",
      TRUE ~ "Unknown"
    ),
    professional_type_label = case_when(
      professional_type == 1 ~ "GP",
      professional_type == 2 ~ "Nurse",
      professional_type == 3 ~ "Pediatrician",
      TRUE ~ "Unknown"
    ),
    
    # Estado real del paciente (lógica mejorada)
    patient_status = case_when(
      abandoned == 1 ~ "Abandoned",
      finished == TRUE ~ "Completed",
      TRUE ~ "In Progress"
    ),
    
    # Tipo de paciente según prioridad
    patient_type = case_when(
      priority == 0 ~ "Urgent",
      TRUE ~ "Regular"
    )
  )

write_csv(combined_patients, file.path(processed_data_path, "combined_patients.csv"))

# 3. Global results -------------------------------

## Core patient outcomes ---

### Summary ----

# Distribution of patients by type (urgent vs. regular) and their final status 
# (completed, abandoned, or in progress) across different healthcare areas. 
# It provides insights into how different patient categories are managed 
# and their outcomes within the system.

patient_type_analysis <- combined_patients %>%
  group_by(area, patient_type, patient_status) %>%
  summarise(
    n = n(),
    pct = n / nrow(combined_patients) * 100,
    mean_retries = mean(retries, na.rm = TRUE),
    mean_waiting_time = mean(waiting_time, na.rm = TRUE),
    .groups = "drop"
  )


p_patient_type_analysis <- patient_type_analysis %>%
  ggplot(aes(x = patient_type, y = area, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), 
            color = "white", size = 3, fontface = "bold") +
  facet_wrap(~patient_status, scales = "free_x") +
  scale_fill_viridis_c(name = "Percentage", option = "plasma") +
  labs(
    title = "Patient distribution by type and status across areas",
    subtitle = "Percentage of total patients in each category",
    x = "Patient type", y = "Healthcare area"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  ) 

ggsave("./figs/p_patient_type_analysis.png", p_patient_type_analysis)

### Modality ----

# This explores how different care modalities (in-person, remote, home) perform 
# across healthcare areas and patient statuses. It helps identify which modalities 
# are most effective.

modality_analysis <- combined_patients %>%
  group_by(area, modality_label, patient_status) %>%
  summarise(
    n = n(),
    pct = n / nrow(combined_patients) * 100,
    mean_retries = mean(retries, na.rm = TRUE),
    mean_waiting_time = mean(waiting_time, na.rm = TRUE),
    .groups = "drop"
  )


p_modality_analysis <- modality_analysis %>%
  ggplot(aes(x = area, y = n, fill = modality_label)) +
  geom_col(position = "stack") +
  facet_wrap(~patient_status, scales = "free_y") +
  scale_fill_manual(
    name = "Care Modality",
    values = c("In-person" = "#1f77b4", "Remote" = "#ff7f0e", "Home" = "#2ca02c")
  ) +
  labs(
    title = "Patient distribution by modality and status",
    subtitle = "Number of patients across healthcare areas",
    x = "Healthcare area", y = "Number of patients"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_modality_analysis.png", p_modality_analysis)


### Capacity vs. demand ----

# Evaluation on the balance between available daily consultation slots and actual patient demand. 
# It identifies centers and professional types that are operating beyond capacity, 
# leading to high abandonment rates.

capacity_demand_analysis <- combined_patients %>%
  filter(!is.na(daily_slots)) %>%  # Just regular patients
  group_by(area, professional_type_label) %>%
  summarise(
    total_patients = n(),
    completed = sum(patient_status == "Completed"),
    abandoned = sum(patient_status == "Abandoned"),
    avg_daily_slots = mean(daily_slots, na.rm = TRUE),
    avg_retries = mean(retries, na.rm = TRUE),
    completion_rate = completed / total_patients,
    abandonment_rate = abandoned / total_patients,
    .groups = "drop"
  ) %>%
  arrange(desc(completion_rate))


t_capacity_demand_analysis <- capacity_demand_analysis %>%
  group_by(area) %>%
  summarise(
    avg_completion_rate = mean(completion_rate, na.rm = TRUE),
    avg_abandonment_rate = mean(abandonment_rate, na.rm = TRUE),
    total_patients = sum(total_patients),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_completion_rate)) %>%
  mutate(
    across(c(avg_completion_rate, avg_abandonment_rate), 
           ~sprintf("%.1f%%", .x * 100))
  ) %>%
  knitr::kable(
    col.names = c("Area", "Avg Completion Rate", "Avg Abandonment Rate", "Total Patients"),
    caption = "Area Performance Ranking by Completion Rate"
  )


p_capacity_demand_analysis <- capacity_demand_analysis %>%
  ggplot(aes(x = area, y = completion_rate, fill = professional_type_label)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    name = "Professional Type",
    values = c("GP" = "#1f77b4", "Nurse" = "#ff7f0e", "Pediatrician" = "#2ca02c")
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Completion Rates by area and professional type",
    subtitle = "Percentage of patients who successfully completed their consultation",
    x = "Healthcare area", y = "Completion rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_capacity_demand_analysis.png", p_capacity_demand_analysis)


### Equity in access ----

# Measures the fairness of healthcare access across different demographic groups (age groups) 
# and professional types within each area. The equity score indicates how evenly 
# healthcare outcomes are distributed, identifying potential disparities in service delivery.

# equity_score = 1 - abs(pct_abandoned - mean(pct_abandoned)) / mean(pct_abandoned)
# Calculates abandonment rate for each group, compares each group's rate to the area average 
# and normalises differences 

access_equity <- combined_patients %>%
  group_by(area, age_group, professional_type_label) %>%
  summarise(
    n = n(),
    pct_abandoned = mean(patient_status == "Abandoned"),
    pct_completed = mean(patient_status == "Completed"),
    mean_retries = mean(retries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(area) %>%
  mutate(
    equity_score = 1 - abs(pct_abandoned - mean(pct_abandoned)) / mean(pct_abandoned)
  )

p_access_equity <- access_equity %>%
  group_by(area) %>%
  summarise(
    mean_equity = mean(equity_score, na.rm = TRUE),
    sd_equity = sd(equity_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(area, mean_equity), y = mean_equity)) +
  geom_col(fill = "#1f77b4", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_equity - sd_equity, ymax = mean_equity + sd_equity), 
                width = 0.2, color = "#333333") +
  geom_text(aes(label = sprintf("%.2f", mean_equity)), 
            vjust = -0.2, 
            size = 3) +
  labs(
    title = "Healthcare access equity by area",
    subtitle = "Higher scores indicate more equitable access across demographic groups",
    x = "Healthcare area", y = "Mean equity score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_access_equity.png", p_access_equity)


# 4. Performance KPIs --------------------------------------------------------

## Service level and backlog ----

# A comprehensive overview of system performance by measuring the percentage of patients 
# who successfully complete their consultations vs. those who abandon the system 
# or remain in waiting. Primary indicator of overall system effectiveness.

kpi_summary <- combined_patients %>%
  summarise(
    total_patients = n(),
    completed = sum(patient_status == "Completed"),
    abandoned = sum(patient_status == "Abandoned"),
    in_progress = sum(patient_status == "In Progress"),
    pct_completed = sum(patient_status == "Completed") / n(),
    pct_abandoned = sum(patient_status == "Abandoned") / n(),
    pct_in_progress = sum(patient_status == "In Progress") / n()
  )


t_kpi_summary <- kpi_summary %>%
  mutate(
    across(c(pct_completed, pct_abandoned, pct_in_progress), 
           ~sprintf("%.1f%%", .x * 100))
  ) %>%
  knitr::kable(
    col.names = c("Total patients", "Completed", "Abandoned", "In progress", 
                  "% Completed", "% Abandoned", "% In progress"),
    caption = "Overall system performance summary"
  )

p_kpi_summary <- kpi_summary %>%
  pivot_longer(cols = c(pct_completed, pct_abandoned, pct_in_progress),
               names_to = "metric", values_to = "percentage") %>%
  mutate(metric = case_when(
    metric == "pct_completed" ~ "Completed",
    metric == "pct_abandoned" ~ "Abandoned", 
    metric == "pct_in_progress" ~ "In Progress"
  )) %>%
  ggplot(aes(x = metric, y = percentage, fill = metric)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage * 100)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(
    name = "Patient status",
    values = c("Completed" = "#2ca02c", "Abandoned" = "#d62728", "In Progress" = "#ff7f0e")
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Overall system performance",
    subtitle = "Distribution of patient outcomes across the entire system",
    x = "Patient status", y = "Percentage of total patients"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_kpi_summary.png", p_kpi_summary)


kpi_summary_temporal <- combined_patients %>%
  group_by(year) %>%
  summarise(
    total_patients = n(),
    completed = sum(patient_status == "Completed"),
    abandoned = sum(patient_status == "Abandoned"),
    in_progress = sum(patient_status == "In Progress"),
    pct_completed = sum(patient_status == "Completed") / n(),
    pct_abandoned = sum(patient_status == "Abandoned") / n(),
    pct_in_progress = sum(patient_status == "In Progress") / n(),
    .groups = "drop"
  )

# Temporal trends analysis
temporal_trends <- combined_patients %>%
  group_by(year, area) %>%
  summarise(
    total_patients = n(),
    completion_rate = mean(patient_status == "Completed"),
    abandonment_rate = mean(patient_status == "Abandoned"),
    mean_waiting_time = mean(waiting_time, na.rm = TRUE),
    mean_retries = mean(retries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(area) %>%
  mutate(
    completion_trend = (completion_rate - lag(completion_rate)) / lag(completion_rate) * 100,
    abandonment_trend = (abandonment_rate - lag(abandonment_rate)) / lag(abandonment_rate) * 100
  )


p_temporal_trends <- temporal_trends %>%
  ggplot(aes(x = year, y = completion_rate, color = area, group = area)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(name = "Healthcare area", option = "plasma") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Completion rate trends by year and area",
    subtitle = "Evolution of system performance from 2021 to 2023",
    x = "Year", y = "Completion rate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_temporal_trends.png", p_temporal_trends)


# Year-over-year comparison
year_comparison <- combined_patients %>%
  group_by(year) %>%
  summarise(
    total_patients = n(),
    completion_rate = mean(patient_status == "Completed"),
    abandonment_rate = mean(patient_status == "Abandoned"),
    mean_waiting_time = mean(waiting_time, na.rm = TRUE),
    mean_retries = mean(retries, na.rm = TRUE),
    urgent_ratio = mean(patient_type == "Urgent"),
    remote_ratio = mean(modality_label == "Remote"),
    .groups = "drop"
  ) %>%
  mutate(
    completion_change = (completion_rate - lag(completion_rate)) / lag(completion_rate) * 100,
    abandonment_change = (abandonment_rate - lag(abandonment_rate)) / lag(abandonment_rate) * 100
  )



## Retry metrics ----

### Retry distribution (how many attempts until successful appointment) ----

# How many attempts patients make before either successfully obtaining an appointment 
# or abandoning the system. 

retry_distribution <- combined_patients %>%
  count(retries) %>%
  mutate(
    pct = n / sum(n),
    cumulative_pct = cumsum(pct)
  )


p_retry_distribution <- retry_distribution %>%
  ggplot(aes(x = retries, y = pct)) +
  geom_col(fill = "#1f77b4", alpha = 0.8) +
  geom_line(aes(y = cumulative_pct), color = "#ff7f0e", linewidth = 1.5) +
  geom_point(aes(y = cumulative_pct), color = "#ff7f0e", size = 3) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)), 
            vjust = -0.5, 
            size = 3) +
  scale_y_continuous(
    name = "Percentage",
    labels = scales::percent,
    sec.axis = sec_axis(~., name = "Cumulative percentage", labels = scales::percent)
  ) +
  labs(
    title = "Patient retry distribution",
    subtitle = "Bars: Percentage of patients with X retries | Line: Cumulative percentage",
    x = "Number of retry attempts"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.y.right = element_text(color = "#ff7f0e"),
    axis.text.y.right = element_text(color = "#ff7f0e")
  )

ggsave("./figs/p_retry_distribution.png", p_retry_distribution)


### Retries by age ----

# How different age groups behave in terms of retry attempts and their likelihood 
# of completing or abandoning care. It helps identify age-specific patterns 
# that might require targeted interventions or adjustments.

retries_age <- combined_patients %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mean_retries = mean(retries, na.rm = TRUE),
    median_retries = median(retries, na.rm = TRUE),
    max_retries = max(retries, na.rm = TRUE),
    pct_abandoned = mean(patient_status == "Abandoned"),
    pct_completed = mean(patient_status == "Completed"),
    .groups = "drop"
  )

### Centers with highest retry burden ----

# This analysis identifies healthcare centers and professional types that experience 
# the highest average retry attempts, indicating potential capacity constraints or 
# inefficiencies in service delivery.

retry_burden <- combined_patients %>%
  group_by(area, center_id, professional_type_label) %>%
  summarise(
    mean_retries = mean(retries, na.rm = TRUE),
    pct_abandoned = mean(patient_status == "Abandoned"),
    pct_completed = mean(patient_status == "Completed"),
    n_patients = n(),
    max_retries = max(retries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_retries))


## Resource utilisation ----

# Measures how effectively healthcare resources (professionals) are being utilised 
# across different areas and professional types. It helps identify underutilised resources 
# and areas where additional capacity might be needed.

resource_utilisation <- resources %>%
  mutate(utilisation = server / capacity) %>%
  group_by(area, professional_type) %>%
  summarise(
    avg_utilisation = mean(utilisation, na.rm = TRUE),
    max_utilisation = max(utilisation, na.rm = TRUE),
    p95_utilisation = quantile(utilisation, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_utilisation))


p_resource_utilisation <- resource_utilisation %>%
  mutate(area = factor(area, levels = unique(area))) %>%
  ggplot(aes(x = professional_type, y = area, fill = avg_utilisation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", avg_utilisation * 100)), 
            color = "white", size = 3) +
  scale_fill_viridis_c(name = "Average utilization", option = "plasma") +
  labs(
    title = "Resource utilisation across areas and professional types",
    subtitle = "Percentage of time professionals are actively serving patients",
    x = "Professional type", y = "Healthcare area"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_resource_utilisation.png", p_resource_utilisation)


## System performance metrics ----

### Temporal congestion ----

# This analysis tracks how patient load accumulates over simulation days and 
# identifies periods of peak congestion.

temporal_congestion <- combined_patients %>%
  filter(!is.na(sim_day)) %>%
  group_by(sim_day, area) %>%
  summarise(
    total_patients = n(),
    new_arrivals = sum(sim_day == 0),  
    cumulative_patients = cumsum(total_patients),
    abandoned_today = sum(patient_status == "Abandoned" & sim_day == 0),
    .groups = "drop"
  ) %>%
  mutate(
    congestion_index = cumulative_patients / total_patients,
    daily_abandonment_rate = abandoned_today / new_arrivals
  )

p_temporal_congestion <- temporal_congestion %>%
  ggplot(aes(x = sim_day, y = congestion_index, color = area)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_viridis_d(name = "Healthcare Area", option = "plasma") +
  labs(
    title = "Temporal Congestion Patterns",
    subtitle = "How patient load accumulates over simulation days",
    x = "Simulation Day", y = "Congestion Index"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )


ggsave("./figs/p_temporal_congestion.png", p_temporal_congestion)


### Demand patterns ----

# How demand varies across different modalities, age groups, and areas. 
# It helps identify high-risk combinations that lead to increased abandonment rates

demand_patterns <- combined_patients %>%
  group_by(area, modality_label, age_group) %>%
  summarise(
    n = n(),
    pct_abandoned = mean(patient_status == "Abandoned"),
    mean_retries = mean(retries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(area) %>%
  mutate(
    demand_intensity = n / sum(n),
    risk_score = pct_abandoned * mean_retries
  )

### Operational efficiency ----

# It calculates efficiency scores that balance completion rates with resource utilisation 
# and retry costs. It provides a comprehensive view of how well each center and professional 
# is performing relative to their resource allocation.

# efficiency_score = completed / (total_patients + total_retries)
# Successfully completed patients in the numerator and 
# total system effort (patients + retry attempts) in the denominator
# It Identifies centers/professionals that need process optimisation

operational_efficiency <- combined_patients %>%
  group_by(area, professional_type_label) %>%
  summarise(
    total_patients = n(),
    urgent_patients = sum(patient_type == "Urgent"),
    regular_patients = sum(patient_type == "Regular"),
    completed = sum(patient_status == "Completed"),
    abandoned = sum(patient_status == "Abandoned"),
    total_retries = sum(retries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    efficiency_score = completed / (total_patients + total_retries),
    urgent_ratio = urgent_patients / total_patients,
    retry_efficiency = completed / (regular_patients + total_retries)
  )


p_operational_efficiency <- operational_efficiency %>%
  ggplot(aes(x = area, y = efficiency_score, fill = professional_type_label)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", efficiency_score)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(
    name = "Professional type",
    values = c("GP" = "#1f77b4", "Nurse" = "#ff7f0e", "Pediatrician" = "#2ca02c")
  ) +
  labs(
    title = "Operational efficiency by area and professional type",
    subtitle = "Higher scores indicate better performance relative to resource allocation",
    x = "Healthcare area", y = "Efficiency score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_operational_efficiency.png", p_operational_efficiency)



### Bottlenecks ----

# This analysis identifies specific centers and professional types that create 
# the most significant bottlenecks in the system. 

bottleneck_analysis <- combined_patients %>%
  filter(patient_status == "Abandoned") %>%
  group_by(area, center_id, professional_type_label) %>%
  summarise(
    n_abandoned = n(),
    avg_retries_before_abandonment = mean(retries, na.rm = TRUE),
    max_retries_before_abandonment = max(retries, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    bottleneck_severity = n_abandoned * avg_retries_before_abandonment
  ) %>%
  arrange(desc(bottleneck_severity))


p_bottleneck_analysis <- bottleneck_analysis %>%
  slice_max(bottleneck_severity, n = 10) %>%
  ggplot(aes(x = reorder(paste(area, center_id, professional_type_label), bottleneck_severity), 
             y = bottleneck_severity, fill = area)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(name = "Healthcare area", option = "plasma") +
  labs(
    title = "Top 15 system bottlenecks",
    subtitle = "Centers and professional types with highest bottleneck severity",
    x = "Center and professional type", y = "Bottleneck severity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("./figs/p_bottleneck_analysis.png", p_bottleneck_analysis)

