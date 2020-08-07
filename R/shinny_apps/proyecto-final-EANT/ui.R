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
          h5("Acotamos el estudio a la Ciudad Autónoma de Buenos Aires e incluimos algunos datos sobre el contexto nacional."),
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
              ),
              tags$li(
                tags$a(href = "https://www.indec.gob.ar/indec/web/Nivel4-Tema-4-31-117", "Encuesta sobre Trabajo No Remunerado y Uso del Tiempo (INDEC)")
              ),
              tags$li(
                tags$a(href = "https://datos.gob.ar/", "Datos Argentina")
              )
            ) ## cierre tags lista
          ), ## cierre tag div
          br(),
          br()
        ), ## cierre column
        column(1)
        
      ) ## cierre fluidRow
      
    ), ## Cierre tabPanel
    

    tabPanel(
      "Trabajo no remunerado", ## TAB 2 - Trabajo no remunerado
      br(),
      #h4("Definición"),
      #br(),
      tags$blockquote("Nos referimos a ",tags$strong("trabajo doméstico no remunerado"), "cuando se trata de tareas de limpieza, gestión,  alimentación, refacción, acompañamiento escolar, cuidado de mascotas, de niños/as y/o de personas que requieran cuidados especiales, por las que no se percibe retribución económica."),
      #br(),
      #h5("Gráficos"),
      br(),
      tabsetPanel( ## SUB TABSET
        
        tabPanel("Contexto nacional - Participacion", ## TAB 1.1 Tasa participacion
                 br(),
                 navlistPanel( ## NAVLIST GENERO
                   
                   tabPanel("Mujeres",
                            br(),
                            h4("Tasa de participacion"),
                            h5("Porcentaje de participación de mujeres de 18 años y más en tareas domésticas no remuneradas, según provincia"),
                            br(),
                            leafletOutput(outputId = "map_tasa1"),
                            br(),
                            h6("Argentina - Año: 2013"),
                            tags$h6("Fuente: Encuesta sobre Trabajo No Remunerado y Uso del Tiempo (INDEC)", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://www.indec.gob.ar/", "https://www.indec.gob.ar/")),
                            br()
                            ),
                   

                   tabPanel("Varones",
                            br(),
                            h4("Tasa de participacion"),
                            h5("Porcentaje de participación de varones de 18 años y más en tareas domésticas no remuneradas, según provincia"),
                            br(),
                            leafletOutput(outputId = "map_tasa2"),
                            br(),
                            h6("Argentina - Año: 2013"),
                            tags$h6("Fuente: Encuesta sobre Trabajo No Remunerado y Uso del Tiempo (INDEC)", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://www.indec.gob.ar/", "https://www.indec.gob.ar/")),
                            br()
                            )
                   
                 ) ## Cierre NAVLIST
                 
        ), ## Cierre TAB1.1
        
        
        tabPanel("Contexto nacional - Nivel educativo", ## TAB 1.2 Nivel educativo
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     
                     checkboxGroupButtons( ## Checkbox GENERO
                       inputId = "sexo5",
                       label = "Seleccione el género",
                       choiceValues = unique(df_10$Genero),
                       choiceNames = c("Mujeres", "Varones"),
                       selected = unique(df_10$Genero)[1:2],
                       justified = TRUE
                     ), ## cierre checkbox
                     
                     pickerInput( ## Dropdown Provincia
                       inputId = "provincia",
                       label = "Seleccione provincia (hasta 6)",
                       choices = unique(df_10$Provincia),
                       selected = unique(df_10$Provincia)[1:6],
                       options = list(`actions-box` = TRUE,
                                      `max-options` = 6),
                       multiple = TRUE
                     ), ## cierre dropdown
                     
                     pickerInput( ## Dropdown nivel educativo
                       inputId = "nivel_instruccion_2",
                       label = "Seleccione grado de educacion",
                       choices = unique(df_10$Nivel.educativo),
                       selected = unique(df_10$Nivel.educativo)[1:6],
                       options = list(`actions-box` = TRUE),
                       multiple = TRUE
                     ), ## cierre dropdown
                     
                   ), ## Cierre sidebarPanel
                   
                   
                   mainPanel( ## inicio mainPanel
                     br(),
                     h4("Tasa de participacion"),
                     h5("Porcentaje de participación en tareas domésticas no remuneradas según género y nivel educativo, por provincia"),
                     plotOutput(outputId = "graph_provincia"), ## Plot output - GRUPO EDAD
                     h6("Argentina - Año: 2013"),
                     tags$h6("Fuente: Encuesta sobre Trabajo No Remunerado y Uso del Tiempo (INDEC)", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://www.indec.gob.ar/", "https://www.indec.gob.ar/")),
                     br()

                   ) ## Cierre mainPanel
                   
                   
                 ) ## Cierre sidebarLayout
          
        ), ## Cierre TAB 1.2
        
        
        tabPanel(
          "Grupo de edad", ## TAB 2.1 - Grupo Edad
          br(),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupButtons( ## Checkbox SEXO
                inputId = "sexo",
                label = "Seleccione el género",
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
              h6("Ciudad Autonoma de Buenos Aires - Año: 2016"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()

              ) ## Cierre mainPanel
            
          ) ## cierre sidebarLayout
        ), ## cierre TAB 2.1

        tabPanel(
          "Nivel educativo", ## TAB 2.2 - Nivel instruccion
          br(),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupButtons( ## Checkbox SEXO
                inputId = "sexo2",
                label = "Seleccione el género",
                choiceValues = unique(df_2_sin_total$sexo),
                choiceNames = c("Mujeres", "Varones"),
                selected = unique(df_2_sin_total$sexo)[1:2],
                justified = TRUE
              ), ## cierre checkbox

              pickerInput( ## dropdown NIVEL INSTRUCCION
                inputId = "nivel_instruccion",
                label = "Seleccione grado de educacion",
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
              h5("Promedio horas por nivel educativo"),
              plotOutput(outputId = "graph_nivel_instruccion"), ## Plot output - Nivel instruccion
              h6("Ciudad Autonoma de Buenos Aires - Año: 2016"),
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
                label = "Seleccione el género",
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
              h5("Promedio horas diarias segun quintil de ingreso familiar"),
              plotOutput(outputId = "graph_quintil"), ## Plot output - quintil ingreso
              h6("Ciudad Autonoma de Buenos Aires - Año: 2016"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()
                      
            ) ## cierre MainPanel
            
          ) ## cierre sidebarLayout
        ), ## Cierre TAB 2.3
        
        
        tabPanel(
          "Actividad", ## TAB 2.4 - Promedio horas diarias por actividad no remunerada
          br(),
          sidebarLayout(
            sidebarPanel(
              
              checkboxGroupButtons( ## Checkbox GENERO
                inputId = "sexo4",
                label = "Seleccione el género",
                choiceValues = unique(df_9_horasact$sexo),
                choiceNames = c("Mujeres", "Varones"),
                selected = unique(df_9_horasact$sexo)[1:2],
                justified = TRUE
              ), ## cierre checkbox
              
              pickerInput( ## dropdown ACTIVIDAD
                inputId = "actividad",
                label = "Seleccione la actividad",
                # choiceValues = unique(df_3_sin_total$quintil_ing_familiar),
                # choiceNames = c("1°", "2°","3°","4°","5°"),
                choices = unique(df_9_horasact$grupos_actividad),
                selected = unique(df_9_horasact$grupos_actividad)[1:length(unique(df_9_horasact$grupos_actividad))],
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ), ## cierre dropdown
              
            ), ## cierre sidebarPanel
            
            mainPanel(
              br(),
              h4("Uso del tiempo"),
              h5("Promedio de horas diarias dedicadas a tareas de cuidado, actividades de mercado y ocio"),
              plotOutput(outputId = "graph_actividad"), ## Plot output - actividad
              h6("Ciudad Autonoma de Buenos Aires - Año: 2016"),
              tags$h6("Fuente: Buenos Aires Data", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://data.buenosaires.gob.ar/", "https://data.buenosaires.gob.ar/")),
              br()
              
            ) ## cierre mainPanel
            
          ) ## cierre sidebarLayout
          
          
        ) ## Cierre TAB 2.4
        
        
        
      ) ## Cierre SUB TABSETPANEL
    ), ## Cierre TAB 2


    tabPanel(
      "Brecha salarial", ## TAB 3 - Brecha salarial
      br(),
      tags$blockquote("Se conoce como ",tags$strong("brecha salarial entre hombres y mujeres"), "a la diferencia existente entre los salarios percibidos por los trabajadores de ambos sexos, calculada sobre la base de la diferencia media entre los ingresos brutos por hora de todos los trabajadores."),
      br(),
      tabsetPanel( ## SUB TABSETPANEL
        
        tabPanel("Contexto nacional", ## TAB 1.2 Promedio anual
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     
                     sliderTextInput( ## Slider AÑO
                       inputId = "anio_3",
                       label = "Seleccione año",
                       choices = sort(unique(df_remuneracion_promedio$indice_tiempo)),
                       selected = c(min(unique(df_remuneracion_promedio$indice_tiempo)), max(unique(df_remuneracion_promedio$indice_tiempo))),
                       from_min = min(unique(df_remuneracion_promedio$indice_tiempo)),
                       from_max = max(unique(df_remuneracion_promedio$indice_tiempo)) - 1,
                       to_min = min(unique(df_remuneracion_promedio$indice_tiempo)) + 1,
                       to_max = max(unique(df_remuneracion_promedio$indice_tiempo)),
                       dragRange = TRUE,
                       animate = TRUE,
                       hide_min_max = TRUE
                     ), ## cierre slider
                     

                   ), ## cierre sidebarPanel
                   
                   mainPanel( ## inicio mainPanel Promedio anual
                     br(),
                     h4("Promedio anual nacional"),
                     h5("ingresos por genero de 1996 al 2017"),
                     plotOutput(outputId = "graph_promedio_anual"), ## Plot output - GRUPO EDAD
                     h6("Argentina - Años: 1996-2017"),
                     tags$h6("Fuente: Datos Argentina", HTML('&nbsp;'),"-", HTML('&nbsp;'), a(href = "https://datos.gob.ar/", "https://datos.gob.ar/")),
                     br()

                     
                   ) ## Cierre mainPanel
                   
                 )
                 
        ), ## Cierre TAB 1.2
        
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
                h6("Ciudad Autonoma de Buenos Aires - Años: 1990 - 2016"),
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
              h6("Ciudad Autonoma de Buenos Aires - Años: 2013 -2017"),
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
              h6("Ciudad Autonoma de Buenos Aires - Año: 2017"),
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
                      tags$p("Ailén Rocío Ventaja", HTML('&nbsp;'),"[", icon("linkedin"), a(href = "https://www.linkedin.com/in/ailenventaja/", "Linkedin"), "]")
                  ),
                  tags$li(
                      tags$p("Alejandro Ingercher", HTML('&nbsp;'),"[", icon("github"), a(href = "https://github.com/aleingercher", "Github"),"|", icon("linkedin"), a(href ="https://www.linkedin.com/in/alejandro-ingercher-casas-234aba41", "Linkedin") , "]")
                  ),
                  tags$li(
                      tags$p("Laureano Fuentes", HTML('&nbsp;'),"[", icon("linkedin"), a(href = "http://linkedin.com/in/laureanofuentes", "Linkedin"), "]")
                  ),
                  tags$li(
                      tags$p("Santiago Lator ", HTML('&nbsp;'),"[", icon("github"), a(href = "https://github.com/santiagolator", "Github"),"|", icon("linkedin"), a(href ="https://www.linkedin.com/in/santiago-lator-arias-291879153/", "Linkedin") , "]")
                  )
              ) ## cierre tags lista
          ),
          
          tags$hr(), ## ---------


          h4("Equipo docente"),
          tags$div(
            tags$ul(
              tags$li(
                tags$p("Romina Méndez y Julio Spairani", HTML('&nbsp;'), "[ ", a(href = "https://eant.tech/", "EANT"), " ]")
                
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
              ),
              tags$li(
                tags$a(href = "https://github.com/wilkox/treemapify", "treemapify")
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
          h5("Paleta de colores"),
          tags$div( ## tag div
            tags$ul( ## tags lista
              tags$li(
                tags$p(tags$a(href = "https://www.color-hex.com/color-palette/95221", "Brisk"), HTML('&nbsp;'),"|", HTML('&nbsp;'), includeHTML("include.html") )
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
