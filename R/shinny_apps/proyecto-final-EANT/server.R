#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library("shiny")
# library("tidyverse")
# library("ggplot2")
# library("viridis")

# Define server logic
shinyServer(function(input, output) {

  # Grafico 1 - Trabajo remunerado por GRUPO DE EDAD
  output$graph_grupo_edad <- renderPlot({
    color_puntos <- c("#872642", "#F6C026")

    df_sin_total %>%
      filter(
        sexo %in% input$sexo,
        grupo_edad %in% input$grupo_edad
      ) %>%
      ggplot(aes(
        x = grupo_edad,
        y = promedio_hs_diarias,
        color = sexo
      )) +
      geom_point(size = 3.5) +
      coord_cartesian(ylim = c(0.5, 4.5)) +
      scale_color_manual(
        values = color_puntos,
        name = "Sexo   ",
        breaks = c("m", "v"),
        labels = c("  Mujeres ", "  Varones")
      ) +
      labs(
        #title = "Trabajo no remunerado",
        #subtitle = "Promedio horas diarias por grupo de edad\n",
        #caption = "\nAño: 2016 - Ciudad Autonoma de Buenos Aires\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
        x = "\nGrupo de edad",
        y = "Prom. horas diarias\n"
      ) +
      theme_ipsum_rc() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0, color = "#a3a3a3"),
        legend.position = "bottom"
      )
  }) ## Fin grafico1


  # Grafico 2 - Trabajo remunerado por NIVEL INSTRUCCION
  output$graph_nivel_instruccion <- renderPlot({
    color_barras <- c("#872642", "#F6C026")

    df_2_sin_total %>%
      filter(
        sexo %in% input$sexo2,
        nivel_instruccion %in% input$nivel_instruccion,
        promedio_hs_diarias >= input$prom_horas[1],
        promedio_hs_diarias <= input$prom_horas[2]
      ) %>%
      ggplot(aes(fill = sexo, y = promedio_hs_diarias, x = nivel_instruccion)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      scale_fill_manual(
        values = color_barras,
        name = "Sexo",
        breaks = c("m", "v"),
        labels = c("Mujeres", "Varones")
      ) +
      labs(
        # title = "Trabajo no remunerado",
        # subtitle = "Promedio horas diarias por nivel de instruccion\n",
        # caption = "\nAño: 2016 - Ciudad Autonoma de Buenos Aires\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
        x = "",
        y = "\nProm. horas diarias"
      ) +
      theme_ipsum_rc() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0, color = "#a3a3a3"),
        axis.text.y = element_text(size = 10, hjust = 0)
      )
  }) ## Fin grafico2


  # Grafico 3 - Trabajo remunerado por QUINTIL DE INGRESO
  output$graph_quintil <- renderPlot({
    color_barras <- c("#872642", "#F6C026")

    df_3_sin_total %>%
      filter(
        sexo %in% input$sexo3,
        quintil_ing_familiar %in% input$quintil,
        promedio_hs_diarias >= input$prom_horas2[1],
        promedio_hs_diarias <= input$prom_horas2[2]
      ) %>%
      ggplot(aes(fill = sexo, y = promedio_hs_diarias, x = quintil_ing_familiar)) +
      geom_bar(stat = "identity", position = "dodge") +
      # geom_label(aes(label = promedio_hs_diarias), fill = "white", hjust = "center", size = 3) +
      scale_fill_manual(
        values = color_barras,
        name = "Sexo",
        breaks = c("m", "v"),
        labels = c("Mujeres", "Varones")
      ) +
      labs(
        # title = "Trabajo no remunerado",
        # subtitle = "Promedio horas diarias segun quintil de ingreso familiar\n",
        # caption = "\nAño: 2016 - Ciudad Autonoma de Buenos Aires\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
        x = "",
        y = "Prom. horas diarias\n"
      ) +
      theme_ipsum_rc() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0, color = "#a3a3a3")
      )
  }) ## Fin grafico3


  # Grafico 4 - Brecha de ingreso normalizada
  output$graph_brecha_norm <- renderPlot({
    df_5 %>%
      filter(
        anio >= input$anio_1[1],
        anio <= input$anio_1[2]
      ) %>%
      ggplot(aes(x = anio, y = brecha_ing)) +
      geom_smooth(method = "loess", color = "#4c4556", se = F, linetype = "dashed") + ## tendencia y grado de confianza
      geom_line(color = "#872642", size = 1.1) +
      coord_cartesian(ylim = c(0.7, 1.15)) +
      labs(
        # title = "Brecha salarial",
        # subtitle = "Relación entre los ingresos laborales mensuales de la ocupación principal, \nnormalizados por la cantidad de horas, de mujeres y varones\n",
        # caption = "\nCiudad Autonoma de Buenos Aires\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
        x = "",
        y = ""
      ) +
      theme_ipsum_rc() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 11),
        plot.caption = element_text(hjust = 0, color = "#a3a3a3")
      )
  }) ## Fin grafico 4


  # Grafico 5 - Brecha de ingreso por fuente
  output$graph_brecha_fuente <- renderPlot({
    color_barras_2 <- c("#4C4556", "#872642", "#F6C026", "#A0D3F9")

    df_6_sin_total %>%
      filter(
        anio >= input$anio_2[1],
        anio <= input$anio_2[2],
        brecha_ing >= input$brecha[1],
        brecha_ing <= input$brecha[2],
        grupo_edad %in% input$grupo_edad2,
        fuente_ingresos %in% input$fuente_ing
      ) %>%
      ggplot(aes(x = anio, y = brecha_ing, fill = fuente_ingresos)) +
      geom_bar(stat = "identity") + ### LE digo aggplot que voy a indicar el parametro "y"
      # geom_label(aes(label = round(brecha_ing), fill = "white", hjust = "center")) +
      facet_grid(~grupo_edad) +
      # coord_flip() +
      scale_fill_manual(
        values = color_barras_2,
        name = "Fuente ingresos   "
      ) +
      labs(
        # title = "Brecha salarial",
        # subtitle = "Relación entre el ingreso de las mujeres con respecto al de los varones\nsegún la fuente de ingresos y grupo de edad\n",
        # caption = "\nCiudad Autonoma de Buenos Aires\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
        x = "",
        y = ""
      ) +
      theme_ipsum_rc(base_family = "Roboto Condensed",
                     subtitle_family = "Roboto Condensed") +
      theme(
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 11),
        plot.caption = element_text(hjust = 0, color = "#a3a3a3"),
        legend.position = "bottom"
      )
  }) ## Fin grafico 5


  ## Grafico 6 - Brecha por rama de actividad
  output$graph_brecha_rama <- renderPlot({
    color_barras_2 <- c("#4C4556", "#872642", "#F6C026", "#A0D3F9")

    df_7_sin_total %>%
      filter(
        rama %in% input$rama,
        brecha_ing >= input$brecha_2[1],
        brecha_ing <= input$brecha_2[2]
      ) %>%
      ggplot(aes(x = rama, y = brecha_ing, fill = rama)) +
      geom_bar(stat = "identity") + ### Le digo a ggplot que voy a indicar el parametro "y"
      geom_label(aes(label = round(brecha_ing)), fill = "white", hjust = "center") +
      coord_cartesian(ylim = c(-50, 0)) +
      scale_fill_manual(values = color_barras_2) +
      labs(
        # title = "Brecha salarial",
        # subtitle = "Relación entre el ingreso promedio de las mujeres con respecto al de los varones según\nla rama de actividad a la que se dedica el establecimiento donde trabaja la persona",
        # caption = "\nAño: 2017 - Ciudad Autonoma de Buenos Aires\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
        x = "",
        y = ""
      ) +
      theme_ipsum_rc() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 10.2),
        plot.caption = element_text(hjust = 0, color = "#a3a3a3"),
        legend.position = "none"
      )
  }) ## Cierre grafico 6
  
  
  ## Mapa 1 - Tasa participacion mujeres
    output$map_tasa1 <- renderLeaflet({
      
     #tasa_participacion_m <- df_tidy$tasa_participacion_m
      #tasa_participacion_v <- df_tidy$tasa_participacion_v
      
      #filteredData <- reactive({
      #  quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
      #})
 
    leaflet(data = argentina) %>%  ### inicio leaflet mapa 1
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(df_tidy$tasa_participacion_m), ### elemento cambiante
                  fillOpacity = 0.8,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = popup_m)%>%
      addLegend("bottomright", pal = pal, values = ~df_tidy$tasa_participacion_m, ### elemento cambiante
                title = "Tasa de participacion de mujeres",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1)
    
    # observe({
    #   leafletProxy("map_tasa1", data = filteredData()) %>%
    #     clearShapes() %>%
    #     addPolygons(
    #       fillColor = ~pal(input$tasa_mapa)
    #     ) %>% 
    #   addLegend(
    #     "bottomright", pal = pal, values = ~input$tasa_mapa,
    #     title = "Tasa de participacion de mujeres",
    #     labFormat = labelFormat(suffix = "%"),
    #     opacity = 1
    #     )
    # })
    
    
  }) ## Fin Mapa 1
    
    


  ## Mapa 2 - Tasa participacion varones
    output$map_tasa2 <- renderLeaflet({

    leaflet(data = argentina) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(df_tidy$tasa_participacion_v),
                  fillOpacity = 0.8,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = popup_v) %>%
      addLegend("bottomright", pal = pal, values = ~df_tidy$tasa_participacion_v,
                title = "Tasa de participacion de varones",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1)
  }) ## Fin mapa 2
  
})
