library(tidyverse)
library(tools)

### Cargo el dataframe con los paquetes en CRAN
db <- tools::CRAN_package_db()

### Genero dataset con columnas de interes
db_min <- db %>% 
  select(
    Package, Version, Description
  ) %>% 
  mutate(
    url_pkg = paste0("https://cran.r-project.org/web/packages/",Package,"/index.html")
  )

### Genero dataframe con los detalles de la db creada
total_pkg <- nrow(db_min)
last_update_db <- file.info("./data/db_cran.rds")$mtime
db_status <- data.frame(total_pkg, last_update_db)


### Borro objetos innecesarios
rm(list = c("total_pkg","last_update_db", "db"))


### Guardo el dataframe en un archivo para usar en la API
#save.image(file = "./data/db_cran.RData")
write_rds(db_min, "./data/db_cran.rds")
write_rds(db_status, "./data/db_status.rds")
