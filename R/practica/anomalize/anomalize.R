# devtools::install_github("business-science/anomalize")
install.packages("anomalize")

library(tidyverse)
library(anomalize)

view(tidyverse_cran_downloads)

tidyverse_cran_downloads %>%
  filter(
    package == "broom",
    lubridate::year(date) %in% c("2018")) %>% 
  #ungroup() %>% 
  # Data Manipulation / Anomaly Detection
  time_decompose(count, method = "stl") %>%
  anomalize(remainder, method = "iqr") %>%
  clean_anomalies() %>% 
  #select(date, anomaly, observed, observed_cleaned) %>%
  #filter(anomaly == "Yes")
  time_recompose() %>%
  # Plot decomposition
  #plot_anomaly_decomposition() %>% 
  # Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
  labs(title = "Tidyverse Anomalies", subtitle = "STL + IQR Methods")


sube_diario = read_csv(file = "https://archivos-datos.transporte.gob.ar/upload/Sube/total-usuarios-por-dia-AMBA.csv")


sube_diario %>%
  filter(
    lubridate::year(indice_tiempo) == 2021) %>% 
  time_decompose(total_amba, method = "stl") %>% 
  anomalize(remainder, method = "iqr") %>% 
  clean_anomalies() %>% 
  time_recompose() %>% 
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
  labs(title = "Cantidad de usuarios SUBE por dia en AMBA", subtitle = "STL + IQR Methods")
