# **healthcare-simulation-simmer**
This repository contains the code, data, and documentation for my Master's Thesis project. The objective is to simulate the functioning of primary healthcare centers in the Region of Murcia using discrete-event simulation with the `simmer` package in R.

## **Title**
**Simulation of patient flow and resource allocation in Primary Care: A discrete event simulation**

## **Description**
This project develops a discrete-event simulation to analyse how primary care services are organised and delivered in a regional healthcare system. Using real-world administrative and clinical data from 2019 to 2023, the simulation models patient arrivals, staffing levels, consultation durations, and the impact of urgent visits on system performance.

The goals are to:
- Evaluate capacity and demand balance across healthcare areas.
- Simulate patient flow at the level of healthcare centers.
- Estimate system congestion, queueing times, and resource utilization.
- Compare dynamics over time and under different demand scenarios.

The simulation is built using the `simmer` package in R, incorporating realistic schedules, arrival rates by age group, professional availability by center, and empirical consultation durations.

```{r}
## **Project Structure**
├── data/
│   ├── Raw and processed datasets: population, consultations, professionals, etc.
│   └── Derived input files for simulation (arrival tables, capacities, durations)
├── scripts/
│   ├── 01_data_cleaning.R      # ETL and harmonization of health data
│   ├── 02_arrival_generation.R # Generates synthetic arrival schedules per center
│   ├── 03_simulation.R         # Core simulation logic using simmer
│   └── 04_visualizations.R     # Plotting resource usage, waiting times, and patient flow
├── results/
│   ├── Plots by area and center (ggplot2 / simmer.plot)
│   └── Summary metrics
├── docs/
│   └── Report
└── README.md

```

## **Requirements**
- R version ≥ 4.3.0
- Required packages:
  - `tidyverse` (includes `dplyr`, `stringr`, `purrr`, `readr`, `ggplot2`, etc.)
  - `simmer` and `simmer.plot`
  - `readxl`
  - `rvest`
  - `httr`

## **Author**
Celia Muñoz Fernández

Master in Computational Social Science

UC3M
