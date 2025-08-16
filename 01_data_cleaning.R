# 01_data_cleaning.R

# This script performs the cleaning and harmonisation of input data.
# It includes web scraping of healthcare centers and professionals from murciasalud.es,
# reading local Excel files from SIAP, and interpreting working hour descriptions.

## Loading required libraries ----
library(httr)           # HTTP requests
library(rvest)          # Web scraping
library(dplyr)          # Data manipulation
library(purrr)          # Functional programming
library(stringr)        # String operations
library(readr)          # Reading text files
library(readxl)         # Reading Excel files
library(progress)       # Progress bar (optional)


# 1. Web scraping from murciasalud.es  ------------------------------------

# Defining the base URL and user agent for web scraping
BASE_URL <- "https://www.murciasalud.es/"
UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/124.0 Safari/537.36"
)

# Getting list of primary care centers for a given health area
get_centers <- function(area_id) {
  area_url <- sprintf(
    "%scaps.php?op=mostrar_area&id_area=%d&idsec=6",
    BASE_URL, area_id
  )
  
  res <- GET(area_url, add_headers(`User-Agent` = UA), httr::timeout(10))
  stop_for_status(res)
  pg <- read_html(res)
  
  # Finding the heading associated with the center list
  h2 <- html_element(
    pg,
    xpath = "//h2[contains(translate(.,'ÁÉÍÓÚ','AEIOU'),'Centros de salud')]"
  )
  if (is.na(h2)) {
    message("Area ", area_id, ": <h2> heading not found.")
    return(tibble())
  }
  
  # Extracting the <ul> or <div> element immediately after the heading
  list_block <- html_element(h2, xpath = "following-sibling::*[1]")
  
  # Extracting links to each center
  links <- html_elements(list_block, "a[href*='op=mostrar_centro']")
  
  # Returning a tibble with center name and URL
  tibble(
    area_id     = area_id,
    center_name = html_text(links, trim = TRUE),
    center_url  = url_absolute(html_attr(links, "href"), BASE_URL)
  ) |> distinct()
}

# Scraping the professionals table from a specific center
scrape_professionals <- function(area_id, center_url) {
  page <- GET(
    center_url,
    add_headers(`User-Agent` = UA),
    httr::timeout(10)
  ) |> read_html()
  
  # Extract ZBS information
  zbs <- extract_zbs(page)
  
  # Trying to locate the professionals table
  tbl <- html_element(page, "table#lista_profesionales")
  if (is.na(tbl)) return(tibble())  # no table present at this center
  
  # Extracting the table and clean column names and content
  df <- html_table(tbl, fill = TRUE) |>
    as_tibble(.name_repair = "minimal") |>
    rename(
      full_name         = 1,
      professional_type = 2,
      schedule          = 3,
      extra_info        = 4
    ) |>
    mutate(across(everything(), ~ str_squish(.x))) |>
    mutate(
      area_id    = area_id,
      center_id  = as.numeric(str_extract(center_url, "(?<=id_centro=)\\d+")),
      center_url = center_url,
      zbs        = zbs  
    ) |>
    dplyr::select(area_id, center_id, zbs, center_url, everything())  # reorder columns
  
  return(df)
}

# Helper function to extract ZBS from the page
extract_zbs <- function(page) {
  # Getting all text content from the page
  all_text <- html_text(page, trim = TRUE)
  
  # Extracting text following "Zona básica de salud:"
  zbs <- str_extract(all_text, "(?<=Zona básica de salud:)\\s*(.+?)(?=\\s*Área de Salud|$)")
  
  # Clean up the extracted ZBS
  if (!is.na(zbs)) {
    zbs <- str_squish(zbs)
  }
  
  return(zbs)
}


# Loop over all areas to retrieve professionals from each center
areas <- 1:9
professionals_scrap <- tibble()

for (area in areas) {
  centres <- get_centers(area)
  cat("Area", area, "...", nrow(centres), "centers found\n")
  
  # For each center URL, scrape the professionals table
  data_area <- map_dfr(
    centres$center_url,
    ~ scrape_professionals(area, .x)
  )
  
  # Append to the full dataset
  professionals_scrap <- bind_rows(professionals_scrap, data_area) 
}

professionals_scrap |> write_csv("./data/processed/professionals_scrap.csv")

# 2. Reading Excel files from SIAP -------------------------------------------

# This function reads all .xlsx files in a directory and returns a named list of data frames.
# File names (without extension) are used as list names.
obtain_data <- function(path) {
  files <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE)
  data_list <- map(files, readxl::read_excel)
  names(data_list) <- str_remove(basename(files), "\\.xlsx$")
  return(data_list)
}

# 3. Interpreting weekly working hours from text schedules -------------------

# This function estimates the total number of hours worked per week
# based on free-text schedules such as "8:00 - 15:00 Monday to Friday"
week_hours <- function(x) {

  # Patterns to recognize weekdays in Spanish
  week_days <- list(
    monday    = c("lunes", "lu", "\\bL\\b"),
    tuesday   = c("martes", "ma", "\\bM\\b"),
    wednesday = c("miercoles", "miércoles", "mi", "\\bX\\b"),
    thursday  = c("jueves", "ju", "\\bJ\\b"),
    friday    = c("viernes", "vi", "\\bV\\b"),
    saturday  = c("sabado", "sábado", "sa", "\\bS\\b"),
    sunday    = c("domingo", "do", "\\bD\\b")
  )

  # Counting number of active weekdays based on the text
  count_days <- function(text) {
    lower_txt <- str_to_lower(text)

    if (str_detect(lower_txt, "lunes a viernes|lu a vi|l a v")) return(5)
    if (str_detect(lower_txt, "lunes a sabado|lu a sa|l a s")) return(6)
    if (str_detect(lower_txt, "todos los dias|diariamente")) return(5)

    n <- sum(map_lgl(week_days, ~ any(str_detect(lower_txt, .x))))
    if (n == 0) return(5)  # Conservative default
    return(n)
  }

  # Converting hour string (e.g. "8:30") to decimal hour (e.g. 8.5)
  parse_hour <- function(h) {
    parts <- str_split_fixed(h, ":", 2)
    as.numeric(parts[, 1]) + ifelse(parts[, 2] == "", 0, as.numeric(parts[, 2]) / 60)
  }

  # Applying transformation for each schedule string
  map_dbl(x, function(txt) {
    if (is.na(txt) || str_trim(txt) == "") return(NA_real_)

    # Look for hour ranges like "8:00 - 15:00"
    ranges <- str_match_all(txt, "(\\d{1,2}(?::\\d{2})?)\\s*(?:-|a|hasta)\\s*(\\d{1,2}(?::\\d{2})?)")[[1]]
    if (nrow(ranges) == 0) return(NA_real_)

    ranges_df <- unique(data.frame(start = ranges[,2], end = ranges[,3], stringsAsFactors = FALSE))

    # Calculating daily total
    start <- parse_hour(ranges_df$start)
    end   <- parse_hour(ranges_df$end)
    daily_hours <- sum(pmax(end - start, 0), na.rm = TRUE)

    # Estimating total weekly hours
    detected_days <- count_days(txt)
    daily_hours * detected_days
  })
}
