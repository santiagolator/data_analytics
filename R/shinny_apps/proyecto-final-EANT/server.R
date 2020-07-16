#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
#library("plotly")
#library("treemap")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
# Grafico 1 - Trabajo remunerado por GRUPO DE EDAD    
output$graph_grupo_edad = renderPlot({

    color_puntos <- c("#872642", "#F6C026")
    
    df_sin_total %>% 
        filter(sexo %in% input$sexo,
               grupo_edad %in% input$grupo_edad) %>% 
        ggplot(aes(
            x = grupo_edad, 
            y = promedio_hs_diarias,
            color = sexo)) +
        geom_point(size = 3.5) +
        coord_cartesian(ylim = c(0.5,4.5)) +
        scale_color_manual(values = color_puntos,
                           name="Sexo",
                           breaks=c("m", "v"),
                           labels=c("Mujeres", "Varones")) +
        labs(title = "Trabajo no remunerado", 
             subtitle = "Promedio horas diarias por grupo de edad\n",
             caption = "\nAño: 2016\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
             x = "\nGrupo de edad",
             y = "Prom. horas diarias\n") +
        theme_tinyhand() +
        theme(plot.title = element_text(hjust=0), 
              plot.subtitle = element_text(hjust = 0),
              plot.caption = element_text(hjust = 0, color = "#a3a3a3"),
              legend.position="bottom")   
}) ## Fin grafico1

# Grafico 2 - Trabajo remunerado por NIVEL INSTRUCCION
output$graph_nivel_instruccion = renderPlot({
    
    color_barras <- c("#872642", "#F6C026")
    
    df_2_sin_total %>% 
        filter(sexo %in% input$sexo2,
               nivel_instruccion %in% input$nivel_instruccion,
               promedio_hs_diarias >= input$prom_horas[1],
               promedio_hs_diarias <= input$prom_horas[2]) %>% 
        ggplot(aes(fill=sexo, y=promedio_hs_diarias, x=nivel_instruccion)) + 
        geom_bar(stat="identity", position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = color_barras,
                          name="Sexo",
                          breaks=c("m", "v"),
                          labels=c("Mujeres", "Varones")) +
        labs(title = "Trabajo no remunerado", 
             subtitle = "Promedio horas diarias por nivel de instruccion\n",
             caption = "\nAño: 2016\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
             x = "",
             y = "\nProm. horas diarias") +
        theme_tinyhand() +
        theme(plot.title = element_text(hjust=0), 
              plot.subtitle = element_text(hjust = 0),
              plot.caption = element_text(hjust = 0, color = "#a3a3a3"),
              axis.text.y = element_text(size = 10, hjust = 0))
}) ## Fin grafico2

# Grafico 3 - Trabajo remunerado por QUINTIL DE INGRESO
output$graph_quintil = renderPlot({

    color_barras <- c("#872642", "#F6C026")
    
    df_3_sin_total %>% 
        filter(sexo %in% input$sexo2,
               quintil_ing_familiar %in% input$quintil,
               promedio_hs_diarias >= input$prom_horas2[1],
               promedio_hs_diarias <= input$prom_horas2[2]) %>%
        ggplot(aes(fill=sexo, y=promedio_hs_diarias, x=quintil_ing_familiar)) + 
        geom_bar(stat="identity", position = "dodge") +
        #geom_label(aes(label = promedio_hs_diarias), fill = "white", hjust = "center", size = 3) +
        scale_fill_manual(values = color_barras,
                          name="Sexo",
                          breaks=c("m", "v"),
                          labels=c("Mujeres", "Varones")) +
        labs(title = "Trabajo no remunerado", 
             subtitle = "Promedio horas diarias segun quintil de ingreso familiar\n",
             caption = "\nAño: 2016\nFuente: Buenos Aires Data - https://data.buenosaires.gob.ar/",
             x = "",
             y = "Prom. horas diarias\n") +
        theme_tinyhand() +
        theme(plot.title = element_text(hjust=0), 
              plot.subtitle = element_text(hjust = 0),
              plot.caption = element_text(hjust = 0, color = "#a3a3a3"))    
    
    
}) ## Fin grafico3




})
