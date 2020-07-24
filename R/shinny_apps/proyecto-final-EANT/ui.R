#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Librerias
# library("tidyverse")
# library("shinythemes")
# library("shinyWidgets")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Theme
  theme = shinytheme("flatly"),

  # Application title
  titlePanel("Trabajo no remunerado y brecha salarial"),

  # Subtitulo

  p("Trabajo final curso Data Analytics - EANT 2020."),

  # Salto de linea
  br(),

  # Sidebar with a slider input for number of bins
  tabsetPanel(
    tabPanel(
      "Abstract", ## TAB 1 - Problema
      fluidRow(
        br(),
        column(1),
        column(
          10,
          h4("Problema"),
          h5("Los roles de género son construcciones sociales que, si bien se han ido flexibilizando, continúan conservando cierta vigencia dentro de nuestra cultura."),
          h5("Los históricos papeles de hombre proveedor y mujer cuidadora, a pesar de haber perdido fuerza, siguen siendo parte de las representaciones y el imaginario colectivo."),
          h5("En las últimas décadas la mujer se introdujo en el mercado laboral buscando un equilibrio entre éste y el trabajo doméstico"),
          br(),
          h4("Objetivos del proyecto"),
          h5("En el presente proyecto pretendemos analizar la desigualdad de género en el trabajo remunerado y no remunerado, a través del análisis de la brecha salarial y la participación de hombres y mujeres en el trabajo doméstico."),
          h5("Nuestro objetivo principal visibilizar las diferencias existentes tanto en la distribución de trabajo del hogar y tareas de cuidado, como en los salarios percibidos en el mercado laboral según género. Se intenta contribuir a la discusión actual sobre la desigualdad de género y los roles establecidos."),
          h5("Acotamos el estudio a la Ciudad Autónoma de Buenos Aires."),
          br(),
          h4("Fuentes de datos"),
          tags$div( ## tag div
            tags$ul( ## tags lista
              tags$li(
                tags$a(href = "https://data.buenosaires.gob.ar", "Buenos Aires data (GCBA)")
              ),
              tags$li(
                tags$a(href = "https://www.estadisticaciudad.gob.ar", "Sistema de indicadores de Género (GCBA)")
              ),
              tags$li(
                tags$a(href = "https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos", "Encuesta Permanente de Hogares (EPH - INDEC)")
              )
            ) ## cierre tags lista
          ), ## cierre tag div
          br(),
          br()
        ), ## cierre column
        column(1)
        
      ) ## cierre fluidRow
      
    ), ## Cierre tabPanel
    
    # tabPanel(
    #   "Situacion nacional",
    #   br(),
    #   tabsetPanel(
    #     tabPanel("Tasa participacion",
    #              br(),
    #              navlistPanel(
    #                           tabPanel("Mujeres",
    #                                    leafletOutput(outputId = "map_tasa1")),
    #                           
    #                           # absolutePanel(top = 10, right = 10,
    #                           #               selectInput(
    #                           #                 inputId = "tasa_mapa", 
    #                           #                 label = "Tasa participacion",
    #                           #                 choices = c("Mujeres" = "tasa_participacion_m", 
    #                           #                             "Varones" = "tasa_participacion_v")
    #                           #               )
    #                           # )
    #                           
    #                           
    #                            tabPanel("Varones",
    #                                     leafletOutput(outputId = "map_tasa2"))
    #                           
    # 
    #              )
    # 
    #   ),
    #   tabPanel("Promedio horas")
    # 
    #   )
    # ),
    
    
    tabPanel(
      "Trabajo no remunerado", ## TAB 2 - Trabajo no remunerado
      br(),
      tabsetPanel( ## SUB TABSET
        tabPanel(
          "Grupo de edad", ## TAB 2.1 - Grupo Edad
          br(),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupButtons( ## Checkbox SEXO
                inputId = "sexo",
                label = "Seleccione el sexo",
                choiceValues = unique(df_sin_total$sexo),
                choiceNames = c("Mujeres", "Varones"),
                selected = unique(df_sin_total$sexo)[1:2],
                justified = TRUE
              ), ## cierre checkbox

              pickerInput( ## Dropdown GRUPO EDAD
                inputId = "grupo_edad",
                label = "Seleccione el grupo de edad",
                choices = unique(df_sin_total$grupo_edad),
                selected = unique(df_sin_total$grupo_edad)[1:5],
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ) ## cierre dropdown
            ), ## Cierre sidebarPanel
            
            mainPanel( ## inicio mainPanel
              br(),
              h4("Trabajo no remunerado"),
              h5("Promedio horas diarias por grupo de edad"),
              plotOutput(outputId = "graph_grupo_edad"), ## Plot output - GRUPO EDAD
              h6("Año: 2016 - Ciudad Autonoma de Buenos Aires"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()
              
              ) ## Cierre mainPanel
            
          ) ## cierre sidebarLayout
        ), ## cierre TAB 2.1

        tabPanel(
          "Nivel de instruccion", ## TAB 2.2 - Nivel instruccion
          br(),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupButtons( ## Checkbox SEXO
                inputId = "sexo2",
                label = "Seleccione el sexo",
                choiceValues = unique(df_2_sin_total$sexo),
                choiceNames = c("Mujeres", "Varones"),
                selected = unique(df_2_sin_total$sexo)[1:2],
                justified = TRUE
              ), ## cierre checkbox

              pickerInput( ## dropdown NIVEL INSTRUCCION
                inputId = "nivel_instruccion",
                label = "Seleccione el grado de educacion",
                choices = unique(df_2_sin_total$nivel_instruccion),
                # choiceValues = unique(df_2_sin_total$nivel_instruccion),
                # choiceNames = c("Hasta secundario incompleto", "Secundario completo y superior incompleto", "Superior completo y mas"),
                selected = unique(df_2_sin_total$nivel_instruccion)[1:3],
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ), ## cierre dropdown

              sliderTextInput( ## Slider promedio horas
                inputId = "prom_horas",
                label = "Seleccione el promedio de horas",
                # min = min(df_2_sin_total$promedio_hs_diarias),
                # max = max(df_2_sin_total$promedio_hs_diarias),
                # value = c(min(df_2_sin_total$promedio_hs_diarias),max(df_2_sin_total$promedio_hs_diarias))
                choices = sort(df_2_sin_total$promedio_hs_diarias),
                selected = c(min(df_2_sin_total$promedio_hs_diarias), max(df_2_sin_total$promedio_hs_diarias)),
                from_min = min(df_2_sin_total$promedio_hs_diarias),
                from_max = max(df_2_sin_total$promedio_hs_diarias) - 1,
                to_min = min(df_2_sin_total$promedio_hs_diarias) + 1,
                to_max = max(df_2_sin_total$promedio_hs_diarias),
                dragRange = TRUE,
                hide_min_max = TRUE
              ) ## cierre slider
            ), ## cierre sidebar

            mainPanel(
              br(),
              h4("Trabajo no remunerado"),
              h5("Promedio horas por nivel de instruccion"),
              plotOutput(outputId = "graph_nivel_instruccion"), ## Plot output - Nivel instruccion
              h6("Año: 2016 - Ciudad Autonoma de Buenos Aires"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()
              
              
              
              ) ## Cierre mainPanel
            
            
            
          ) ## cierre sidebarLayout
        ), ## Cierre TAB 2.2



        tabPanel(
          "Quintil de ingreso", ## TAB 2.3 - Quintil ingreso
          br(),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupButtons( ## Checkbox SEXO
                inputId = "sexo3",
                label = "Seleccione el sexo",
                choiceValues = unique(df_3_sin_total$sexo),
                choiceNames = c("Mujeres", "Varones"),
                selected = unique(df_3_sin_total$sexo)[1:2],
                justified = TRUE
              ), ## cierre checkbox

              pickerInput( ## dropdown QUINTIL
                inputId = "quintil",
                label = "Seleccione el quintil de ingreso",
                # choiceValues = unique(df_3_sin_total$quintil_ing_familiar),
                # choiceNames = c("1°", "2°","3°","4°","5°"),
                choices = unique(df_3_sin_total$quintil_ing_familiar),
                selected = unique(df_3_sin_total$quintil_ing_familiar)[1:length(unique(df_3_sin_total$quintil_ing_familiar))],
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ), ## cierre dropdown

              sliderTextInput( ## Slider promedio horas
                inputId = "prom_horas2",
                label = "Seleccione el promedio de horas",
                choices = sort(df_3_sin_total$promedio_hs_diarias),
                selected = c(min(df_3_sin_total$promedio_hs_diarias), max(df_3_sin_total$promedio_hs_diarias)),
                from_min = min(df_3_sin_total$promedio_hs_diarias),
                from_max = max(df_3_sin_total$promedio_hs_diarias) - 1,
                to_min = min(df_3_sin_total$promedio_hs_diarias) + 1,
                to_max = max(df_3_sin_total$promedio_hs_diarias),
                dragRange = TRUE,
                hide_min_max = TRUE
              ) ## cierre slider
            ), ## cierre sidebarPanel

            mainPanel(
              br(),
              h4("Trabajo no remunerado"),
              h5("Promedio horas diarias segun quintinl de ingreso familiar"),
              plotOutput(outputId = "graph_quintil"), ## Plot output - quintil ingreso
              h6("Año: 2016 - Ciudad Autonoma de Buenos Aires"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()
                      
            ) ## cierre MainPanel
            
          ) ## cierre sidebarLayout
        ) ## Cierre TAB 2.3
      ) ## Cierre SUB TABSETPANEL
    ), ## Cierre TAB 2


    tabPanel(
      "Brecha salarial", ## TAB 3 - Brecha salarial
      br(),
      tabsetPanel( ## SUB TABSETPANEL
        tabPanel(
          "Normalizado", ## TAB 3.1 Normalizado
          br(),
          sidebarLayout(
            sidebarPanel(
              sliderTextInput( ## Slider año
                inputId = "anio_1",
                label = "Seleccione año",
                choices = sort(df_5$anio),
                selected = c(min(df_5$anio), max(df_5$anio)),
                from_min = min(df_5$anio),
                from_max = max(df_5$anio) - 1,
                to_min = min(df_5$anio) + 1,
                to_max = max(df_5$anio),
                animate = TRUE,
                dragRange = TRUE,
                hide_min_max = TRUE
              ), ## Cierre slider

              h6("Se muestra adicionalmente linea de tendencia")
            ), ## cierre sidebarPanel

            
            mainPanel(
                br(),
                h4("Brecha salarial"),
                h5("Relación entre los ingresos laborales mensuales de la ocupación principal, normalizados por la cantidad de horas, de mujeres y varones"),
                plotOutput(outputId = "graph_brecha_norm"), ## Plot output - brecha normalizado
                h6("Ciudad Autonoma de Buenos Aires"),
                tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
                br()
                ) ## cierre mainPanel
            
            
          ) ## Cierre sidebarLayout
        ), ## Cierre TAB 3.1

        tabPanel(
          "Fuente de ingresos", ## TAB 3.2 Fuente de ingresos
          br(),
          sidebarLayout(
            sidebarPanel(
              pickerInput( ## dropdown grupo de edad
                inputId = "grupo_edad2",
                label = "Seleccione el grupo de edad",
                choices = unique(df_6_sin_total$grupo_edad),
                selected = unique(df_6_sin_total$grupo_edad)[1:length(unique(df_6_sin_total$grupo_edad))],
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ), ## cierre dropdown

              pickerInput( ## dropdown fuente de ingresos
                inputId = "fuente_ing",
                label = "Seleccione la fuente de ingresos",
                choices = unique(df_6_sin_total$fuente_ingresos),
                selected = unique(df_6_sin_total$fuente_ingresos)[1:length(unique(df_6_sin_total$fuente_ingresos))],
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ), ## cierre dropdown

              sliderTextInput( ## Slider brecha
                inputId = "brecha",
                label = "Seleccione valor brecha",
                choices = round(sort(df_6_sin_total$brecha_ing)),
                selected = c(min(round(df_6_sin_total$brecha_ing)), max(round(df_6_sin_total$brecha_ing))),
                from_min = min(round(df_6_sin_total$brecha_ing)),
                from_max = max(round(df_6_sin_total$brecha_ing)) - 1,
                to_min = min(round(df_6_sin_total$brecha_ing)) + 1,
                to_max = max(round(df_6_sin_total$brecha_ing)),
                dragRange = TRUE,
                hide_min_max = TRUE
              ), ## cierre slider

              sliderTextInput( ## Slider anio
                inputId = "anio_2",
                label = "Seleccione año",
                choices = sort(df_6_sin_total$anio),
                selected = c(min(df_6_sin_total$anio), max(df_6_sin_total$anio)),
                from_min = min(df_6_sin_total$anio),
                from_max = max(df_6_sin_total$anio),
                to_min = min(df_6_sin_total$anio),
                to_max = max(df_6_sin_total$anio),
                animate = TRUE,
                dragRange = TRUE,
                hide_min_max = TRUE
              ) ## cierre slider
            ), ## Cierre sidebarPanel

            
            mainPanel(
              br(),
              h4("Brecha salarial"),
              h5("Relación entre el ingreso de las mujeres con respecto al de los varones según la fuente de ingresos y grupo de edad"),
              plotOutput(outputId = "graph_brecha_fuente"), ## Plot output - brecha fuente ingreso
              h6("Ciudad Autonoma de Buenos Aires"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()
              
              ) ### cierre MainPanel
            
          ) ## Cierre sidebarLayout
        ), ## Cierre TAB 3.2

        tabPanel(
          "Rama de actividad", ## TAB 3.3 Rama de actividad
          br(),
          sidebarLayout(
            sidebarPanel(
              pickerInput( ## dropdown rama
                inputId = "rama",
                label = "Seleccione la rama de actividad",
                choices = unique(df_7_sin_total$rama),
                selected = df_7_sin_total$rama[1:length(unique(df_7_sin_total$rama))],
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ), ## cierre dropdown rama

              sliderTextInput( ## Slider brecha
                inputId = "brecha_2",
                label = "Seleccione valor brecha",
                choices = round(sort(df_7_sin_total$brecha_ing)),
                selected = c(min(round(df_7_sin_total$brecha_ing)), max(round(df_7_sin_total$brecha_ing))),
                from_min = min(round(df_7_sin_total$brecha_ing)),
                from_max = max(round(df_7_sin_total$brecha_ing)),
                to_min = min(round(df_7_sin_total$brecha_ing)),
                to_max = max(round(df_7_sin_total$brecha_ing)),
                dragRange = TRUE,
                hide_min_max = TRUE
              ) ## cierre slider
            ), ## cierre sidebarPanel

            mainPanel(
              br(),
              h4("Brecha salarial"),
              h5("Relación entre el ingreso promedio de las mujeres con respecto al de los varones según la rama de actividad a la que se dedica el establecimiento donde trabaja la persona"),
              plotOutput(outputId = "graph_brecha_rama"), ## Plot output - brecha rama actividad
              h6("Año: 2017 - Ciudad Autonoma de Buenos Aires"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()
              ) ## cierre mainPanel
            
            
            
          ) ## cierre sidebarLayout
        ) ## Cierre TAB 3.3
      ) ## Cierre SUB TABSETPANEL
    ), ## Cierre TAB 3



    tabPanel(
      "Creditos", ## TAB 4 - Creditos y bibliografia
      fluidRow(
        br(),
        column(1),
        column(
          10, ## columna principal
          h4("Autores"),
          tags$div( ## tag div
              tags$ul( ## tags lista
                  tags$li(
                      tags$p("Ailén Rocío Ventaja")
                  ),
                  tags$li(
                      tags$p("Alejandro Ingercher")
                  ),
                  tags$li(
                      tags$p("Laureano Fuentes")
                  ),
                  tags$li(
                      tags$p("Santiago Lator ", HTML('&nbsp;'),"[", icon("github"), a(href = "https://github.com/santiagolator", "Github"),"|", icon("linkedin"), a(href ="https://www.linkedin.com/in/santiago-lator-arias-291879153/", "Linkedin") , "]")
                  )
              ) ## cierre tags lista
          ),
          
          tags$hr(), ## ---------


          h4("Docente"),
          tags$div(
            tags$ul(
              tags$li(
                tags$p("Romina Méndez", HTML('&nbsp;'), "[ ", a(href = "https://eant.tech/", "EANT"), " ]")
                
              )
            )
          ),
       
          tags$hr(), ## ---------   
          
          h4("Bibliografia"),
          tags$div(
            tags$ul(
              tags$li(
                tags$p("D'ALESSANDRO, Mercedes (2019).", HTML('&nbsp;'), tags$i("Economia feminista. Las mujeres, el trabajo y el amor."), HTML('&nbsp;'), "Ciudad Autonoma de Buenos Aires: Sudamericana.")
                
              )
            )
          ),
          
          tags$hr(), ## ---------

          h4("Codigo"),
          p("El lenguaje elegido para el desarrollo del proyecto es R, y el IDE que vamos a utilizar es ", a(href = "https://www.r-project.org/about.html", "RStudio.", .noWS = "outside"), .noWS = c("after-begin", "before-end")),
          #br(),
          h5("Paquetes"),
          tags$div( ## tag div
            tags$ul( ## tags lista
              tags$li(
                tags$a(href = "https://www.tidyverse.org/", "tidyverse")
              ),
              tags$li(
                tags$a(href = "https://www.estadisticaciudad.gob.ar", "ggplot2")
              ),
              tags$li(
                tags$a(href = "https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html", "viridis")
              ),
              tags$li(
                tags$a(href = "https://github.com/hrbrmstr/hrbrthemes", "hrbrthemes")
              ),
              tags$li(
                tags$a(href = "https://shiny.rstudio.com/", "shiny")
              ),
              tags$li(
                tags$a(href = "https://rstudio.github.io/shinythemes/", "shinythemes")
              ),
              tags$li(
                tags$a(href = "https://github.com/dreamRs/shinyWidgets", "shinyWidgets")
              ),
              tags$li(
                tags$a(href = "https://github.com/yixuan/showtext", "showtext")
              )
            ) ## cierre tags lista
          ), ## cierre tag div
          #br(),
          h5("Repositorio"),
          tags$div( ## tag div
            tags$ul( ## tags lista
              tags$li(
                tags$a(href = "https://github.com/santiagolator/data_analytics/tree/master/R/shinny_apps/proyecto-final-EANT", "Github")
              )
            ) ## cierre tags lista
          ), ## Cierre div
          br(),
          br()
        ) ## Cierre columna principal
      )
    ) ## Cierre TAB 4
  )
))
