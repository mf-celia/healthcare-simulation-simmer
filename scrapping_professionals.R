
library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(progress)

## Parámetros generales ----
BASE_URL <- "https://www.murciasalud.es/"
UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/124.0 Safari/537.36"
)

## Lista de centros de un área ----
get_centers <- function(area_id) {
  area_url <- sprintf(
    "%scaps.php?op=mostrar_area&id_area=%d&idsec=6",
    BASE_URL, area_id
  )
  res <- GET(area_url, add_headers(`User-Agent` = UA), httr::timeout(10))
  stop_for_status(res)
  pg <- read_html(res)

  h2 <- html_element(
    pg,
    xpath = "//h2[contains(translate(.,'ÁÉÍÓÚ','AEIOU'),'Centros de salud')]"
  )
  if (is.na(h2)) {
    message("Área ", area_id, ": h2 no encontrado.")
    return(tibble())
  }

  lista <- html_element(h2, xpath = "following-sibling::*[1]")
  links <- html_elements(lista, "a[href*='op=mostrar_centro']")

  tibble(
    area_id     = area_id,
    center_name = html_text(links, trim = TRUE),
    center_url  = url_absolute(html_attr(links, "href"), BASE_URL)
  ) |> distinct()
}

## Professionals df for one center ----
scrape_professionals <- function(area_id, center_url) {

  page <- GET(
            center_url,
            add_headers(`User-Agent` = UA),
            httr::timeout(10)
          ) |>
          read_html()

  tbl <- html_element(page, "table#lista_profesionales")
  if (is.na(tbl)) return(tibble())    # centre without table

  df <- html_table(tbl, fill = TRUE) |>
        as_tibble(.name_repair = "minimal") |>
        rename(
          full_name  = 1,
          professional_type = 2,
          schedule   = 3,
          extra_info = 4
        ) |>
        mutate(across(everything(), ~ str_squish(.x))) |>
        mutate(
          area_id    = area_id,
          center_id  = str_extract(center_url, "(?<=id_centro=)\\d+"),
          center_url = center_url
        ) |>
        dplyr::select(area_id, center_id, center_url, everything())   # reorder

  df
}

## Loop over areas ----
areas <- 1:9
professionals_scrap <- tibble()

for (area in areas) {
  centres <- get_centers(area)
  cat("Area", area, "...", nrow(centres), "centres found\n")

  data_area <- map_dfr(
    centres$center_url,
    ~ scrape_professionals(area, .x)   # two arguments
  )

  professionals_scrap <- bind_rows(professionals_scrap, data_area)
}