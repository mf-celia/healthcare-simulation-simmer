library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(progress)


# functions
obtain_data <- function(path) {
  # Obtener todos los archivos .xlsx en el directorio
  files <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE)
  
  # Leer cada archivo y guardarlo en una lista nombrada
  data_list <- map(files, ~ read_excel(.x))
  names <- str_remove(basename(files), "\\.xlsx$")
  names(data_list) <- names
  
  return(data_list)
}


# Returns a random age between the bounds of age_group
random_age <- function(x) {
  if (is.na(x) || x == "") return(NA_real_)
  
  if (grepl("-", x)) {
    bounds <- as.numeric(strsplit(x, "-")[[1]])
    return(runif(1, bounds[1], bounds[2]))
  }
  
  if (grepl("\\+$", x)) {
    lower <- as.numeric(sub("\\+$", "", x))
    return(runif(1, lower, 95))
  }
  
  as.numeric(x)
}


week_hours <- function(x) {
  
  # Lista de patrones para detectar días
  dias_semana <- list(
    lunes     = c("lunes", "lu", "\\bL\\b"),
    martes    = c("martes", "ma", "\\bM\\b"),
    miercoles = c("miercoles", "miércoles", "mi", "\\bX\\b"),
    jueves    = c("jueves", "ju", "\\bJ\\b"),
    viernes   = c("viernes", "vi", "\\bV\\b"),
    sabado    = c("sabado", "sábado", "sa", "\\bS\\b"),
    domingo   = c("domingo", "do", "\\bD\\b")
  )
  
  contar_dias <- function(txt) {
    txt_l <- str_to_lower(txt)
    
    # Expresiones comunes
    if (str_detect(txt_l, "lunes a viernes|lu a vi|l a v")) return(5)
    if (str_detect(txt_l, "lunes a sabado|lu a sa|l a s")) return(6)
    if (str_detect(txt_l, "todos los dias|diariamente")) return(5)
    
    # Detectar individualmente
    n <- sum(map_lgl(dias_semana, ~ any(str_detect(txt_l, .x))))
    if (n == 0) return(5)
    return(n)
  }
  
  # Convertir texto horario a número decimal
  parse_hour <- function(h) {
    partes <- str_split_fixed(h, ":", 2)
    as.numeric(partes[, 1]) + ifelse(partes[, 2] == "", 0, as.numeric(partes[, 2]) / 60)
  }
  
  map_dbl(x, function(txt) {
    if (is.na(txt) || str_trim(txt) == "") return(NA_real_)
    
    # Detectar rangos horarios, como "8:00 - 15:00"
    rangos <- str_match_all(txt, "(\\d{1,2}(?::\\d{2})?)\\s*(?:-|a|hasta)\\s*(\\d{1,2}(?::\\d{2})?)")[[1]]
    if (nrow(rangos) == 0) return(NA_real_)
    
    # Crear combinación única de tramos horarios
    rangos_df <- unique(data.frame(inicio = rangos[,2], fin = rangos[,3], stringsAsFactors = FALSE))
    
    # Calcular duración de cada tramo
    inicio <- parse_hour(rangos_df$inicio)
    fin    <- parse_hour(rangos_df$fin)
    horas_dia <- sum(pmax(fin - inicio, 0), na.rm = TRUE)
    
    # Días detectados
    dias_detectados <- contar_dias(txt)
    horas_dia * dias_detectados
  })
}
