library(tidyverse)
library(tools)
library(tm)


### Cargo el dataframe con los paquetes en CRAN
db <- tools::CRAN_package_db()



### Selecciono las columnas de mi interes
db_min <- db %>% 
  select(
    Package, Version, Description
  ) %>% 
  mutate(
    url_pkg = paste0("https://cran.r-project.org/web/packages/",Package,"/index.html"),
    tokens = removeWords(Description, stopwords(kind = "en"))
  )



### Guardo el dataframe en un archivo para usar mas adelante
write_rds(db_min, "./data/db_cran.rds")



### Pruebo filtrar por algun string
db_min %>% 
  filter(
    str_detect(tokens, "model")
  )

## Armo la funcion para realizar la busqueda y acortar la descripcion
function(keyword) {
  result <- db_min %>%
    filter(
      str_detect(tokens, as.character(keyword))
    ) %>%
    mutate(
      Description = ifelse(nchar(Description) > 13, paste0(strtrim(Description, 100), "..."), Description)
    ) %>% 
    slice_head(n = 10)
  
  return(result)
}


### Busco por nombre EXACTO
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

### Estado db_cran.rds
total_pkg <- nrow(db_min)
last_update_db <- file.info("./data/db_cran.rds")$mtime


## Pruebo la funcion
search_package("machine learning")

search_package_name("ggplot")





