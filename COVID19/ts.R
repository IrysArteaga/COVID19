
library(forecast)


esa_cases <- read.csv("esa_casos.csv", sep = ";", header = T)
 
 # ESA cases per epidemiological week
esa_cases$fecha <- as.Date(esa_cases$fecha, format="%d/%m/%Y")
esa_cases$weeks <- epiweek(esa_cases$fecha)
as.data.frame(esa_cases)

 #Group by weeks
esa_week <- esa_cases %>% group_by(weeks) %>% summarise(importados=sum(importados),locales=sum(locales),import_acum=max(import_acum), locales_acum=max(locales_acum))
as.data.frame(esa_week)
 
 
 #-------------------------------------------------------------------------
 #data frame to be plotted
df <- data.frame(fecha=esa_cases$fecha, impor=esa_cases$importados, loc=esa_cases$locales)
df$fecha <- format(as.Date(df$fecha, format="%Y/%m/%d"), "%d-%m")
 
df_ac <- data.frame(fecha=esa_cases$fecha, impor_ac=esa_cases$import_acum, loc_ac=esa_cases$locales_acum)
df_ac$fecha <- format(as.Date(df_ac$fecha, format="%Y/%m/%d"), "%d-%m")
 
 # per week
mdf <- data.frame(weeks=esa_week$weeks, impor=esa_week$importados, loc=esa_week$locales)
mdf_ac <- data.frame(weeks=esa_week$weeks, impor_ac=esa_week$import_acum, loc_ac=esa_week$locales_acum)
 
df$x <- seq(1,length(df$fecha),1)
df_ac$x <- seq(1,length(df_ac$fecha),1)
 
# proy<-function(x,y,tipom,ym){
#   P<-forecast(y, level=c(90,95))
#   trace1 <- list(
#     line = list(
#       color = "#03ddff", 
#       fillcolor = "#03ddff"),
#     mode = 'lines+markers', 
#     name = tipom, 
#     type = "scatter", 
#     x = x-1,
#     y = y)
#   trace2 <- list(
#     line = list(
#       color = "#f0f2f2"), 
#     mode = 'lines+markers', 
#     name = "95% de Confianza", 
#     fill = "toself",
#     type = "scatter", 
#     x = c(max(x)+0:4,max(x)+4:0),
#     y =  c(P$lower[1:5,2],P$upper[5:1,2]),
#     xaxis = "x", 
#     yaxis = "y", 
#     hoveron = "points")
#   
#   trace3 <- list(
#     line = list(
#       color = "#b4bfbf", 
#       fillcolor = "#b4bfbf"), 
#     mode = 'lines+markers', 
#     name = "90% de Confianza",
#     fill = "toself",
#     type = "scatter", 
#     x = c(max(x)+0:4,max(x)+4:0),
#     y =  c(P$lower[1:5,1],P$upper[5:1,1]),
#     xaxis = "x", 
#     yaxis = "y", 
#     hoveron = "points")
#   trace4 <- list(
#     line = list(
#       color = "#6b89b0", 
#       fillcolor = "#6b89b0"),
#     
#     mode = 'lines+markers', 
#     name = "Predicción", 
#     type = "scatter", 
#     x = max(x)+0:4,
#     y = (P$mean[1:5]))
#   layout <- list(
#     title = "Conteo de casos",
#     xaxis = list(
#       title = "Fecha", 
#       domain = c(0, 1)
#     ), 
#     yaxis = list(
#       title = ym, 
#       domain = c(0, 1)
#     ), 
#     margin = list(
#       b = 40, 
#       l = 60, 
#       r = 10, 
#       t = 25
#     )
#   )
#   gf <- plot_ly()
#   gf <- add_trace(gf, line=trace1$line, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x,
#                     y=trace1$y)
#   gf <- add_trace(gf, fill=trace2$fill,  line=trace2$line, mode=trace2$mode, name=trace2$name, type=trace2$type, x=trace2$x, 
#                     y=trace2$y, hoveron=trace2$hoveron)
#   gf <- add_trace(gf,fill=trace3$fill,  line=trace3$line, mode=trace3$mode, 
#                     name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y,
#                     xaxis=trace3$xaxis, yaxis=trace3$yaxis, hoveron=trace3$hoveron)
#   gf <- add_trace(gf, line=trace4$line, mode=trace4$mode, name=trace4$name, type=trace4$type,
#                     x=trace4$x, y=trace4$y)
#   gf <- layout(gf, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin)
#   tablapred<-data.frame(Fecha = as.character.Date(max(x)+0:4,format="%d de %B %Y"), Confirmados = round(P$mean[1:5]))
#   return(list(gf,tablapred))
#   
# }

# function to plot forecast
proy<-function(x,y,tipom,ym){
  P<-forecast(y, level=c(90,95))
  trace1 <- list(
    line = list(color = "#03ddff", fillcolor = "#03ddff"),
    mode = 'lines+markers', 
    name = tipom, 
    type = "scatter", 
    x = x-1,
    y = y)
  trace2 <- list(
    line = list(color = "#f0f2f2"), 
    mode = 'lines+markers', 
    name = "95% de Confianza", 
    fill = "toself",
    type = "scatter", 
    x = c(max(x)+0:4,max(x)+4:0),
    y =  c(P$lower[1:5,2],P$upper[5:1,2]),
    xaxis = "x", 
    yaxis = "y", 
    hoveron = "points")
  
  trace3 <- list(
    line = list(color = "#b4bfbf", fillcolor = "#b4bfbf"), 
    mode = 'lines+markers', 
    name = "90% de Confianza",
    fill = "toself",
    type = "scatter", 
    x = c(max(x)+0:4,max(x)+4:0),
    y =  c(P$lower[1:5,1],P$upper[5:1,1]),
    xaxis = "x", 
    yaxis = "y", 
    hoveron = "points")
  trace4 <- list(
    line = list(color = "#6b89b0", fillcolor = "#6b89b0"),
    mode = 'lines+markers', 
    name = "Predicción", 
    type = "scatter", 
    x = max(x)+0:4,
    y = (P$mean[1:5]))
  layout <- list(
    title = "Proyección de casos",
    xaxis = list(title = "Días", domain = c(0, 1)), 
    yaxis = list(title = ym, domain = c(0, 1)), 
    margin = list(b = 40, l = 60, r = 10, t = 25))
  gf <- plot_ly()
  gf <- add_trace(gf, line=trace1$line, mode=trace1$mode, name=trace1$name,
                  type=trace1$type, x=trace1$x, y=trace1$y)
  gf <- add_trace(gf, fill=trace2$fill,  line=trace2$line, mode=trace2$mode, 
                  name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, hoveron=trace2$hoveron)
  gf <- add_trace(gf,fill=trace3$fill,  line=trace3$line, mode=trace3$mode, 
                    name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y,
                    xaxis=trace3$xaxis, yaxis=trace3$yaxis, hoveron=trace3$hoveron)
  gf <- add_trace(gf, line=trace4$line, mode=trace4$mode, name=trace4$name, type=trace4$type,
                    x=trace4$x, y=trace4$y)
  gf <- layout(gf, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin)
  
  tabla<-data.frame(Día = as.character.Date(max(x)+0:4), Confirmados = round(P$mean[1:5]), LI =round(P$lower[1:5,1]), LS=round(P$upper[1:5,2]))
  
  return(list(gf,tabla))
}


tabPanel("Proyecciones",
         sidebarLayout(
           sidebarPanel(width = 3,
             h3("Proyección de casos"),
             p("Para realizar el pronóstico de la evolución de contagios locales y totales se ha usado el análisis de series de tiempo."),
             br(),
             p("Dado que se disponen de pocos datos hasta la fecha, se genera la proyección para cinco días con un nivel de confianza del 90% y del 95%, se muestran los casos que podrían caer dentro de los intervalos de confianza.")
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("Casos Locales",
                        fluidRow(
                          column(width = 6.5, class="well",
                                 h4("Casos locales por día"),
                                 plotlyOutput("plotf1")
                          ),
                          column(width = 6.5, class="well",
                                 h4("Casos locales acumulados"),
                                 plotlyOutput("plotf2")
                          )
                        )
               ),
               tabPanel("Casos Totales",
                        fluidRow(
                          column(width = 6.5, class="well",
                                 h4("Casos diarios totales"),
                                 plotlyOutput("plotf3")
                          ),
                          column(width = 6.5, class="well",
                                 h4("Casos acumulados totales"),
                                 plotlyOutput("plotf4")
                          )
                        )
               ),
               tabPanel("Tablas",
                        fluidRow(
                          column(width = 6, class="well",
                                 h4("Casos locales"),
                                 tableOutput("tabla1")
                          ),
                          column(width = 6, class="well",
                                 h4("Casos totales"),
                                 tableOutput("table2")
                          )
                        )
              )
               
             )
           )
         )
),








# plot forecast for cumulative local cases
f1 <- proy(df_ac$x,df_ac$loc_ac,"Casos locales" ,"Casos")
plotf1 <- f1[1]

#table
tabla1 <- f1[2]

# plot forecast for cumulative cases: locales + importados
f2 <-proy(esa_cases$x,esa_cases$acumulado_total,"Casos diarios" ,"Casos")
plotf2 <- f2[1]
# table
tabla2 <- f2[2]


#*20Dat0s20*
