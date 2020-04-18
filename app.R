## Original tool: COVID-2019 interactive mapping tool
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), February 2020

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(waiter)) install.packages("waiter", repos = "http://cran.us.r-project.org")
if(!require(profvis)) install.packages("profvis", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")





# update data with automated script
#source("jhu_data_full.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)

# set mapping colour for each outbreak
covid_col = "#f55b02"
covid_other_col = "#807b6f"

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv", sep = ";")
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")


### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
    plot_df = subset(cv_aggregated, date<=plot_date)
    g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 1) +
        ylab("Casos Acumulados") + theme_bw() + 
        scale_colour_manual(values=c(covid_col)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
    plot_df_new = subset(cv_aggregated, date<=plot_date)
    g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
        geom_bar(position="stack", stat="identity") + 
        ylab("Casos nuevos") + theme_bw() + 
        scale_fill_manual(values=c(covid_col)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}


# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, 
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
            xlim(c(cv_min_date,current_date+1)) +
            xlab("Fecha")
    }
    
    if (start_point=="Day of 100th confirmed case") {
        cv_cases = subset(cv_cases, days_since_case100>0)
        g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, 
                                 text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
            xlab("Días desde el 100mo. caso")
    }
    
    if (start_point=="Day of 10th death") {
        cv_cases = subset(cv_cases, days_since_death10>0)
        g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, 
                                 text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
            xlab("Días desde la 10ma. muerte")
    }
    
    g1 = g +
        geom_bar(position="stack", stat="identity") + 
        ylab("Nuevos casos") + theme_bw() + 
        scale_fill_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
            xlim(c(cv_min_date,current_date+1)) + xlab("Fecha")
    }
    
    if (start_point=="Day of 100th confirmed case") {
        cv_cases = subset(cv_cases, days_since_case100>0)
        g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
            xlab("Días desde el caso 100")
    }
    
    if (start_point=="Day of 10th death") {
        cv_cases = subset(cv_cases, days_since_death10>0)
        g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
            xlab("Días desde la 10ma. muerte")
    }
    
    g1 = g + geom_line(alpha=1) + geom_point(size = 1, alpha = 1) +
        ylab("Casos Acumulados") + theme_bw() + 
        scale_colour_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


# function to plot cumulative cases per week 
country_cases_week = function(cases_week,start_point="Date"){
    g = ggplot(cases_week, aes(x = weeks, y = new_outcome, colour = region), text = paste("País:", country, "\n", new_outcome)) +
        xlab("Semanas")
    g1 = g + geom_line(alpha = 1) + geom_point(size = 1, alpha = 1) +
        ylab("Nuevos casos") + theme_bw() +
        scale_colour_manual(values=country_cols) +
        scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1) %>% layout(legend = list(font = list(size=11)))
}


# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"))  {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
            xlim(c(cv_min_date,current_date+1)) +
            xlab("Fecha")
    }
    
    if (start_point=="Day of 100th confirmed case") {
        cv_cases = subset(cv_cases, days_since_case100>0)
        g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
            xlab("Días desde el caso 100")
    }
    
    if (start_point=="Day of 10th death") {
        cv_cases = subset(cv_cases, days_since_death10>0)
        g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
            xlab("Días desde la 10ma. muerte")
    }
    
    g1 = g + geom_line(alpha=1) + geom_point(size = 1, alpha = 1) +
        ylab("Casos Acumulados (log10)") + theme_bw() +
        scale_y_continuous(trans="log10") +
        scale_colour_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
    cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 100th case and 10th death
cv_cases$days_since_case100 = cv_cases$days_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
    country_name = as.character(unique(cv_cases$country))[i]
    country_db = subset(cv_cases, country==country_name)
    country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
    country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
    cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
    cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_100 = subset(cv_today, cases>=100)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                days_since_case100, days_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
    continent_name = as.character(unique(cv_cases_continent$continent))[i]
    continent_db = subset(cv_cases_continent, continent==continent_name)
    continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
    continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
    cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
    cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
}
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# Cases per week
cv_cases$date <- as.Date(cv_cases$date, format("%Y-%m-%d"))
cv_cases$weeks <- epiweek(cv_cases$date)
cases_week <- subset(cv_cases, !is.na(continent_level)) %>% group_by(country,weeks, global_level, continent_level) %>% 
    summarise(cases=max(cases),new_cases=sum(new_cases), deaths=max(deaths),new_deaths=sum(new_deaths), days_since_case100=max(days_since_case100), days_since_death10=max(days_since_death10))
as.data.frame(cases_week)

# cases per week al Continent level
continent_week <- cases_week %>% group_by(continent_level, weeks) %>% 
    summarise(cases=max(cases),new_cases=sum(new_cases), deaths=max(deaths),new_deaths=sum(new_deaths), days_since_case100=max(days_since_case100), days_since_death10=max(days_since_death10))
as.data.frame(continent_week)

# cases per week at Global level
global_week <- cases_week %>% group_by(global_level, weeks) %>% 
    summarise(cases=max(cases),new_cases=sum(new_cases), deaths=max(deaths),new_deaths=sum(new_deaths), days_since_case100=max(days_since_case100), days_since_death10=max(days_since_death10))
as.data.frame(global_week)

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,10,50,100,500)
cv_pal <- colorBin("Spectral", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
        position = "bottomright",
        overlayGroups = c("2019-COVID (Activos)", "2019-COVID (Nuevos)", "2019-COVID (Acumulados)"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("2019-COVID (NUevos)", "2019-COVID (Acumulados)"))  %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-50,~80,80) %>%
    addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
              title = "<small>Casos activos por cada 100,000 hab.</small>") #%>%
#fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
    if (i==1) { cv_aggregated$new[i] = 0 }
    if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
#cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(12, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls = rep(c(brewer.pal(12,"Paired"), brewer.pal(8, "Spectral"), brewer.pal(8, "Dark2"), brewer.pal(12,"Set3"), brewer.pal(8, "PRGn"), brewer.pal(8, "Accent"),  brewer.pal(9, "Set1"),  brewer.pal(8, "Set2")),3)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names




### SHINY UI ###
ui <- bootstrapPage(
#    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("cerulean"), collapsible = TRUE,
               "ESA-COVID-19", id="nav",
               
               tabPanel("ESA-COVID-19",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 80, left = 20, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          h3(textOutput("reactive_case_count"), align = "right"),
                                          h4(textOutput("reactive_death_count"), align = "right"),
                                          span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                          span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                          h6(textOutput("clean_date_reactive"), align = "right"),
                                          h6(textOutput("reactive_country_count"), align = "right"),
                                          tags$i(h6("Actualizado diariamente. Para actualizaciones de última hora visite: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."),tags$br(),
                                                    tags$a(href="https://covid19.gob.sv/", "sitio oficial de El Salvador."))),
                                          tags$i(h6("Los casos confirmados presentan variación debido el reporte de cada país.")),
                                          plotOutput("epi_curve", height="130px", width="100%"),
                                          plotOutput("cumulative_plot", height="130px", width="100%"),
                                          
                                          sliderInput("plot_date",
                                                      label = h5("Seleccione la fecha del gráfico"),
                                                      min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                      max = as.Date(current_date,"%Y-%m-%d"),
                                                      value = as.Date(current_date),
                                                      timeFormat = "%d %b", 
                                                      animate=animationOptions(interval = 2000, loop = FALSE))
                            ),
                            
                            
                        )
               ),
               
               tabPanel("Situación Mundial",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select", "Nivel:",   
                                            choices = c(Global="Global", Continente="Continent", País="Country"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select", "País/Región:",   
                                            choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = c("El Salvador", "Guatemala", "USA", "Italy","Mexico" ,"Spain", "Honduras", "Costa Rica", "Panama", "Mainland China"),
                                            multiple = TRUE), 
                                
                                pickerInput("outcome_select", "Resultado:",   
                                            choices = c(Casos="Cases", Muertes="Deaths"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                pickerInput("start_date", "Fecha de inicio:",   
                                            choices = c(Fecha="Date", Día_del_caso_cien="Day of 100th confirmed case", Día_de_la_10ma_muerte= "Day of 10th death"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = "Date",
                                            multiple = FALSE), 
                                "Seleccione el nivel, región y/ país para actualizar los gráficos. Se muestran los países que tienen al menos 100 casos confirmados."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Diarios", plotlyOutput("country_plot")),
                                    tabPanel("Semanal", plotlyOutput("country_plot_w")),
                                    tabPanel("Acumulados", plotlyOutput("country_plot_cumulative")),
                                    tabPanel("Acumulados (log10)", plotlyOutput("country_plot_cumulative_log"))
                                )
                            )
                        )
               ),
               

               tabPanel("Datos",
                        numericInput("maxrows", "Filas a mostrar", 25),
                        verbatimTextOutput("rawtable"),
                        downloadButton("downloadCsv", "Descargar CSV"),tags$br(),tags$br(),
                        "Adaptado de los datos publicados en: ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                           "Johns Hopkins Center for Systems Science and Engineering.")
               ),
               
               tabPanel("Acerca del dashboard",
                        tags$div(
                            tags$h4("Última actualización"), 
                            h6(paste0(update)),
                            "Este dashboard es actualizado diariamente. Se recomienda visitar los siguientes sitios para información de última hora :",tags$br(),
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                            tags$a(href="https://covid19.gob.sv/.", "Sitio oficial de El Salvador"),tags$br(),
                            "El propósito de este dashboard es dar visibilidad de la situación actual de la pandemia Covid-19 a nivel mundial. Muestra la evolución de casos diarios y semanalmente.",tags$br(),
                            tags$h4("Créditos"),
                            "La idea original de este dashboard pertenece a:", tags$br(),
                            "- Dr. Edward Parker, de The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "- Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            
                            "Un artículo sobre el sitio fue publicado en: ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),tags$br(),
                            "El mapa fue usado en el programa BBC World Service",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            
                            tags$br(),tags$br(),tags$h4("Código"),
                            "El código y los datos usados para generar este Shiny dashboard están disponibles en ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),tags$br(),
                            
                            
                            tags$br(),tags$br(),tags$h4("Autores"),
                            "Iris Arteaga, Egresada de Lic. en Estadística, Universidad de El Salvador",tags$br(),
                            "Daniel Henríquez, Egresado de Lic. en Estadística, Universidad de El Salvador",tags$br(),
                        )
               )
               
    )          
)





### SHINY SERVER ###

server = function(input, output, session) {
    
    # covid tab 
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(input$plot_date),"%d %B %Y")
    })
    
    reactive_db = reactive({
        cv_cases %>% filter(date == input$plot_date)
        #  reactive = cv_cases %>% filter(date == "2020-04-07")
    })
    
    reactive_db_last24h = reactive({
        cv_cases %>% filter(date == input$plot_date & new_cases>0)
    })
    
    reactive_db_large = reactive({
        large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$id)
        #large_countries = reactive %>% filter(alpha3 %in% worldcountry$id)
        worldcountry_subset = worldcountry[worldcountry$id %in% large_countries$alpha3, ]
        large_countries = large_countries[match(worldcountry_subset$id, large_countries$alpha3),]
        large_countries
    })
    
    reactive_db_large_last24h = reactive({
        large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$id)
        large_countries = large_countries[order(large_countries$alpha3),]
        large_countries
    })
    
    reactive_polygons = reactive({
        worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
    })
    
    reactive_polygons_last24h = reactive({
        worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
    })
    
    output$reactive_case_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " Casos")
    })
    
    output$reactive_death_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$death), big.mark=","), " Muertes")
    })
    
    output$reactive_recovered_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " Recuperados")
    })
    
    output$reactive_active_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " Casos activos")
    })
    
    output$reactive_case_count_China <- renderText({
        paste0("Mainland China: ", prettyNum(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
               prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
    })
    
    output$reactive_case_count_row <- renderText({
        paste0("Other: ", prettyNum(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
               prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
    })
    
    output$reactive_country_count <- renderText({
        paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " países/regiones afectados")
    })
    
    output$reactive_new_cases_24h <- renderText({
        paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " nuevos en las últimas 24 hrs.")
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.4, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>% #group = "2019-COVID (Acumulados)",
            #  label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recuperados: %d<br/>Casos por 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$recovered, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
            #  labelOptions = labelOptions(
            #               style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
            #              textsize = "15px", direction = "auto") %>%
            
            addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (Nuevos)",
                             label = sprintf("<strong>%s (últimas 24h)</strong><br/>Casos confirmados: %g<br/>Muertes: %d<br/>Recuperados: %d<br/>Casos por 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                             label = sprintf("<strong>%s (Acumulados)</strong><br/>Casos Confirmados: %g<br/>Muertes: %d<br/>Recuperados: %d<br/>Casos por 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (Activos)",
                             label = sprintf("<strong>%s (Activos)</strong><br/>Casos confirmados: %g<br/>Casos por 100,000: %g<br/><i><small>Excluye individuos que se han <br/>recuperado (%g) o muerto (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) # %>%
            
    })
    
    output$cumulative_plot <- renderPlot({
        cumulative_plot(cv_aggregated, input$plot_date)
    })
    
    output$epi_curve <- renderPlot({
        new_cases_plot(cv_aggregated, input$plot_date)
    })
    

    # update region selections
    observeEvent(input$level_select, {
        if (input$level_select=="Global") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = "Global", selected = "Global")
        }
        
        if (input$level_select=="Continent") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = c("Africa", "Asia", "Europe", "North America", "Central America", "South America"), 
                              selected = c("Africa", "Asia", "Europe", "North America","Central America", "South America"))
        }
        
        if (input$level_select=="Country") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                              selected = cv_today_100$country)
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db = reactive({
        if (input$level_select=="Global") { 
            db = cv_cases_global
            db$region = db$global_level
        }
        if (input$level_select=="Continent") { 
            db = cv_cases_continent 
            db$region = db$continent
        }
        if (input$level_select=="Country") { 
            db = cv_cases
            db$region = db$country
        }
        
        if (input$outcome_select=="Cases") { 
            db$outcome = db$cases
            db$new_outcome = db$new_cases
        }
        
        if (input$outcome_select=="Deaths") { 
            db$outcome = db$deaths 
            db$new_outcome = db$new_deaths 
        }
        
        db %>% filter(region %in% input$region_select)
    })
    
    # create dataframe with selected countries per week
    country_reactive_db_w = reactive({
        
        if (input$level_select=="Global") {
            db_w = global_week
            db_w$region = db_w$global_level
        }
        if (input$level_select=="Continent") {
            db_w = continent_week
            db_w$region = db_w$continent_level
        }
        if (input$level_select=="Country") {
            db_w = cases_week
            db_w$region = db_w$country
        }
        if (input$outcome_select=="Cases") {
            db_w$outcome = db_w$cases
            db_w$new_outcome = db_w$new_cases
        }

        if (input$outcome_select=="Deaths") {
            db_w$outcome = db_w$deaths
            db_w$new_outcome = db_w$new_deaths
        }
        db_w %>% filter(region %in% input$region_select)
    })

    # country-specific plots
    output$country_plot <- renderPlotly({
        country_cases_plot(country_reactive_db(), start_point=input$start_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative <- renderPlotly({
        country_cases_cumulative(country_reactive_db(), start_point=input$start_date)
    })
    
    # country- specific plot per week
    output$country_plot_w <- renderPlotly({
        country_cases_week(country_reactive_db_w(), start_point = input$start_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative_log <- renderPlotly({
        country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date)
    })
    
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_data_", cv_today$date[1], ".csv", sep="")
        },
        content = function(file) {
            write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                            recovered, new_recovered, active_cases, 
                                            per100k, newper100k, activeper100k)), file)
        }
    )
    
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                         recovered, new_recovered, active_cases, 
                                         per100k, newper100k, activeper100k)), input$maxrows), row.names = FALSE)
        options(orig)
    })
    
}


shinyApp(ui, server)
