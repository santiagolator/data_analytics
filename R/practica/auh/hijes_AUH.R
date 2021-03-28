library(tidyverse)
library(lubridate)
library(janitor)



df_hijos <-  read_csv("C:/GitHub/data_analytics/R/practica/auh/dataset/H.2.3.Total Pais. Titulares de la AUH. Hijo e Hijo Discapacitado, segun hijos a cargo.csv") %>% 
  clean_names() %>% 
  mutate(
    periodo = paste0(20, as.numeric(parse_number(periodo)) * (-1))
  )

df_hijos_anio <- df_hijos %>% 
  group_by(periodo) %>% 
  summarise(x1 = sum(x1),
            x2 = sum(x2),
            x3 = sum(x3),
            x4 = sum(x4),
            x5 = sum(x5)
            ) %>% 
  mutate(
    media = rowMeans(df_hijos_anio[ , c(2,6)])
  )


