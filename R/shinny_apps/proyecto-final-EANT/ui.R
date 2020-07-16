#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("tidyverse")
library("ggplot2")
library("viridis")
library("hrbrthemes")
library("shinythemes")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Theme
    
    theme = shinytheme("flatly"),

    # Application title
    titlePanel("Trabajo no remunerado y brecha salarial"),
    
    # Subtitulo
    
    p("Este proyecto tiene como objetivo principal visibilizar las diferencias existentes entre la distribución de trabajo del hogar y en los salarios percibidos en el mercado laboral según el género."),
    
    # Salto de linea
    br(),

    # Sidebar with a slider input for number of bins
    tabsetPanel(
        tabPanel("Problema", ## TAB 1 - Problema
                 fluidRow(
                     br(),
                     column(1),
                     column(10,
                         h4("Problema"),
                         h5("Los roles de género son construcciones sociales que, si bien se han ido flexibilizando, continúan conservando cierta vigencia dentro de nuestra cultura. Los históricos papeles de hombre proveedor y mujer cuidadora, a pesar de haber perdido fuerza, siguen siendo parte de las representaciones y el imaginario colectivo. En las últimas décadas la mujer se introdujo en el mercado laboral buscando un equilibrio entre éste y el trabajo doméstico. 
En el presente proyecto pretendemos analizar la desigualdad de género en el trabajo remunerado y no remunerado, a través del análisis de la brecha salarial y la participación de hombres y mujeres en el trabajo doméstico. Acotamos el estudio a la Ciudad Autónoma de Buenos Aires.
"),
                         br(),
                         h4("Objetivos del proyecto"),
                         h5("Este proyecto tiene como objetivo principal visibilizar las diferencias existentes en la distribución de trabajo del hogar y tareas de cuidado, y en los salarios percibidos en el mercado laboral según género. Se intenta contribuir a la discusión actual sobre la desigualdad de género y los roles establecidos."),
                         br(),
                         h4("Fuentes de datos"),
                         tags$div(tags$ul(
                             tags$li(
                                 tags$a(href="https://data.buenosaires.gob.ar", "Buenos Aires data (GCBA)")
                                 ),
                             tags$li(
                                 tags$a(href="https://www.estadisticaciudad.gob.ar","Sistema de indicadores de Género (GCBA)")
                                 ),
                             tags$li(
                                 tags$a(href="https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos", "Encuesta Permanente de Hogares (EPH - INDEC")
                                 )
                         ))                         
                            ),
                     column(1)
                 )), 
        tabPanel("Trabajo no remunerado", ## TAB 2 - Trabajo no remunerado
                 br(),
                 tabsetPanel(
                     tabPanel("Grupo de edad", ## TAB 2.1 - Grupo Edad
                              br(),
                              sidebarLayout(
                                  sidebarPanel(
                                      checkboxGroupInput( ## Checkbox SEXO
                                          inputId = "sexo",
                                          label = "Seleccione el sexo",
                                          choiceValues = unique(df_sin_total$sexo),
                                          choiceNames = c("Mujeres", "Varones"),
                                          selected = unique(df_sin_total$sexo)[1:2]
                                      ),
                                      checkboxGroupInput(  ## Checkbox GRUPO EDAD
                                          inputId = "grupo_edad",
                                          label = "Seleccione el grupo de edad",
                                          choices = unique(df_sin_total$grupo_edad),
                                          selected = unique(df_sin_total$grupo_edad)[1:5]
                                      )                                      
                                      
                                  ),
                                  mainPanel(plotOutput(outputId = "graph_grupo_edad")) ## Plot output - GRUPO EDAD
                              )),
                     
                     tabPanel("Nivel de instruccion", ## TAB 2.2 - Nivel instruccion
                              br(),
                              sidebarLayout(
                                  sidebarPanel(
                                      checkboxGroupInput( ## Checkbox SEXO
                                          inputId = "sexo2",
                                          label = "Seleccione el sexo",
                                          choiceValues = unique(df_2_sin_total$sexo),
                                          choiceNames = c("Mujeres", "Varones"),
                                          selected = unique(df_2_sin_total$sexo)[1:2]                                      
                                  ),
                                  checkboxGroupInput( ## Checkbox NIVEL INSTRUCCION
                                      inputId = "nivel_instruccion",
                                      label = "Seleccione el grado de educacion",
                                      choiceValues = unique(df_2_sin_total$nivel_instruccion),
                                      choiceNames = c("Hasta secundario incompleto", "Secundario completo y superior incompleto", "Superior completo y mas"),
                                      selected = unique(df_2_sin_total$nivel_instruccion)[1:3]                                      
                                  ),
                                  sliderInput( ## Slider promedio horas
                                      inputId = "prom_horas",
                                      label = "Seleccione el promedio de horas",
                                      min = min(df_2_sin_total$promedio_hs_diarias),
                                      max = max(df_2_sin_total$promedio_hs_diarias),
                                      value = c(min(df_2_sin_total$promedio_hs_diarias),max(df_2_sin_total$promedio_hs_diarias))
                                      )                                  
                                  ),
                                  mainPanel(plotOutput(outputId = "graph_nivel_instruccion"))  ## Plot output - Nivel instruccion
                                  
                              )),
                     
                     
                     
                     tabPanel("Quintil de ingreso", ## TAB 2.3 - Quintil ingreso
                              br(),
                              sidebarLayout(
                                  sidebarPanel(
                                      checkboxGroupInput( ## Checkbox SEXO
                                          inputId = "sexo3",
                                          label = "Seleccione el sexo",
                                          choiceValues = unique(df_3_sin_total$sexo),
                                          choiceNames = c("Mujeres", "Varones"),
                                          selected = unique(df_3_sin_total$sexo)[1:2]                                      
                                      ),
                                      checkboxGroupInput( ## Checkbox QUINTIL
                                          inputId = "quintil",
                                          label = "Seleccione el quintil de ingreso",
                                          choiceValues = unique(df_3_sin_total$quintil_ing_familiar),
                                          choiceNames = c("1°", "2°","3°","4°","5°"),
                                          selected = unique(df_3_sin_total$quintil_ing_familiar)[1:5]                                      
                                      ),
                                      sliderInput( ## Slider promedio horas
                                          inputId = "prom_horas2",
                                          label = "Seleccione el promedio de horas",
                                          min = min(df_3_sin_total$promedio_hs_diarias),
                                          max = max(df_3_sin_total$promedio_hs_diarias),
                                          value = c(min(df_3_sin_total$promedio_hs_diarias),max(df_3_sin_total$promedio_hs_diarias))
                                      )                                       
                                  ),
                                  mainPanel(plotOutput(outputId = "graph_quintil"))  ## Plot output - Nivel instruccion
                                  
                              )
                              ) 
                     
                 )),
        
        tabPanel("Brecha salarial"), ## TAB 3 - Brecha salarial
        
        tabPanel("Creditos") ## TAB 4 - Creditos y bibliografia
        )
))
