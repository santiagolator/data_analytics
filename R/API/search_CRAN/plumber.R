library(plumber)
library(tidyverse)

db_min <- readr::read_rds("./data/db_cran.rds")
db_status <- readr::read_rds("./data/db_status.rds")


#* @apiTitle Busqueda de paquetes en CRAN por keyword

#* @apiDescription  Esta API utiliza una palabra clave o "keyword" para buscar en la base de paquetes de CRAN y devuelve las coincidencias como resultado


#* Buscar paquete por descripcion
#* @param keyword Palabra clave para realizar la busqueda en la base por la descripcion del paquete (devuelve primeros 10 resultados)
#* @get /search-pkg-top10
#* @response 200 OK
function(keyword) {
  result <- db_min %>%
    filter(
      str_detect(Description, as.character(keyword))
    ) %>%
    mutate(
      Description = ifelse(nchar(Description) > 13, paste0(strtrim(Description, 100), "..."), Description)
    ) %>% 
    slice_head(n = 10)
  
  return(result)
}



#* Buscar paquete por descripcion (lista completa)
#* @param keyword Palabra clave para realizar la busqueda en la base por la descripcion del paquete (devuelve lista completa)
#* @get /search-pkg-all
function(keyword) {
  result <- db_min %>%
    filter(
      str_detect(Description, as.character(keyword))
    ) %>%
    mutate(
      Description = ifelse(nchar(Description) > 13, paste0(strtrim(Description, 100), "..."), Description)
    )
  
  return(result)
}


#* Buscar paquete por nombre
#* @param keyword Palabra clave para realizar la busqueda en la base por el nombre del paquete.
#* @get /search-pkg-name
function(keyword) {
  result <- db_min %>%
    filter(
      str_detect(Package, as.character(keyword))
    ) %>%
    mutate(
      Description = ifelse(nchar(Description) > 13, paste0(strtrim(Description, 100), "..."), Description)
    )
  
  return(result)
}

#* Obtener ultima actualizacion y cantidad de paquetes en base de datos
#* @get /cran-db-status
function() {
  return(db_status)
}

