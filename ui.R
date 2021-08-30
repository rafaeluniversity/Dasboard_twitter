############################ GRUPO 3 #############################
#INTEGRANTES
# - Galarza Barrezueta José
# - Zambrano Mendoza Rafael
# - Fernández Manrique Victor
# - Rodriguez Mendoza Yandry
# - Herraez Intriago Manuel
# - Aragundy Castro Jose

#MATERIA/CARRERA:
# Institución: Universidad Técnica de Manabí
# Carrera: Ingeniería en Sistemas de Información
# Materia: Minería de Datos
# Docente: Dr.Jorge Párraga Álava
# Nivel: 5to Semestre
# Paralelo: A

if(!require(bit64)) install.packages('bit64')
if(!require(shinydashboard)) install.packages("shinydashboard")
#(!require(rtweet)) install.packages("rtweet")
if(!require(tm)) install.packages("tm")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(SentimentAnalysis)) install.packages("SentimentAnalysis")
if(!require(wordcloud2)) devtools::install_github("lchiffon/wordcloud2")
if(!require(forestmangr)) install.packages("forestmangr")
if(!require(tidytext)) install.packages("tidytext")
if(!require(tweetrmd)) devtools::install_github("gadenbuie/tweetrmd")
if(!require(webshot2)) remotes::install_github("rstudio/webshot2")
if(!require(emo)) remotes::install_github("hadley/emo")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(forcats)) install.packages("forcats")
if(!require(Rcpp)) install.packages("Rcpp")
if (!require("textclean")) install.packages("textclean")
if (!require("syuzhet")) install.packages("syuzhet")
if (!require("textreg")) install.packages("textreg")
if (!require("shinycssloaders")) install.packages("shinycssloaders")

library(textclean) #libreria con funciones interesantes para eliminar enlaces, menciones de tweets,hashtags de tweet,ademas lo hace muy rápido
library(Rcpp)
library(forcats)
library("rtweet")
library("tm")
library("ggplot2")
library("plotly")
library("dplyr")
library("lubridate")
library("SentimentAnalysis")
library("wordcloud2") 
library("forestmangr")
library("tidytext")
library("tweetrmd")
library("webshot2")
library(emo)
library("openxlsx")
library(tidyr)
library(tidyverse)
require(data.table)
library(shiny)
library(shinydashboard)
library(syuzhet)
library(textreg)
library(shinycssloaders)

body=dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css")
    ),
    tabItems(
        tabItem(
            tabName = "home",
            fluidRow(
                valueBox("+100k", "Tweeks", icon = icon("twitter"),color = "aqua"),
                valueBox("4 días", "Analizados", icon = icon("calendar-day"),color = "yellow"),
                valueBox("10 Gráficos","dinámicos", icon = icon("chart-bar"),color = "green")
            ),
            fluidRow(
                box(title = "RESUMEN:",status = "success",width = 12, 
                    'Afganistán vive estos días el resurgimiento del poder del Talibán con la toma de control del país tras la retirada de las tropas estadounidenses después de 20 años de ocupación.
                El avance del Talibán hizo que de miles de personas huyeran de sus hogares y que ahora traten de abandonar el país.
                El Talibán dijo que "no habrá venganza" y prometió "un gobierno inclusivo islámico", pero el futuro del país aún es incierto.',
                    br(),br(),"Este dashboard contiene gráficos del análisis de las conversaciones en Twitter de los días del 17 al 20 de agosto del 2021.
                Mostrando como resultados gráficas estadísticas de los usuarios, hashtags más usados, comportamiento en el tiempo, nube de 
                palabras, análisis de sentimientos, polaridad de los tweets, etc. Todo lo mencionado en torno a la crisis por la que esta pasando afganistán"
                )
            ),
            fluidRow(
                box(
                    title = "NOTA:", width = 4, solidHeader = TRUE, status = "warning",
                    "Todos los datos de los tweets utilizados para este dashboard fueron recoplidados desde la API de Twitter mediante el uso de la libreria de",tags$b("rtweet"),".",br(),br()
                ),
                box(
                    title = "GRUPO 3 - INTEGRANTES:", width = 4, solidHeader = TRUE, status = "info",
                    "- Galarza Barrezueta José", br(),
                    "- Zambrano Mendoza Rafael", br(),
                    "- Fernández Manrique Victor", br(),
                    "- Rodriguez Mendoza Yandry", br(),
                    "- Herraez Intriago Manuel", br(),
                    "- Aragundy Castro Jose"
                ),
                box(
                    title = "MATERIA/CARRERA:", width = 4, solidHeader = TRUE, status = "info",
                    tags$b("Institución:")," Universidad Técnica de Manabí", br(),
                    tags$b("Carrera:")," Ingeniería en Sistemas de Información", br(),
                    tags$b("Materia:")," Minería de Datos", br(),
                    tags$b("Docente:")," Dr.Jorge Párraga Álava", br(),
                    tags$b("Nivel:")," 5to Semestre", br(),
                    tags$b("Paralelo:")," A"
                )
            )
        ),
        tabItem(tabName = "retwitteados",
                fluidRow(
                    tags$h3("Tweets con mayor cantidad de retweets",align = "center"),
                    box(width=4,
                        title = "Filtros de Tweets:", status = "primary",
                        sliderInput("slider_tweets", "Número de tweets:", 1, 10, 1),
                        tags$b("Nota: "),tags$p("Los tweets toman un momento en cargarse")
                    ),
                    box(width=8,
                        status = "primary", 
                        uiOutput("tweets_retwitteados")
                    )
                )
        ),
        tabItem(tabName = "favoritos",
                fluidRow(
                    tags$h3("Tweets con mayor cantidad de likes",align = "center"),
                    box(width=4,
                        title = "Filtros de Tweets:", status = "primary",
                        sliderInput("slider_tweets2", "Número de tweets:", 1, 10, 1),
                        tags$b("Nota: "),tags$p("Los tweets toman un momento en cargarse")
                    ),
                    box(width=8,
                        status = "primary", uiOutput("tweets_favoritos")
                    )
                )
        ),
        
        tabItem(tabName = "comportamiento-tiempo",
                fluidRow(
                    tags$h3("comportamiento en el tiempo",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        dateRangeInput(inputId = "rango",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-20",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary", 
                        shinycssloaders::withSpinner(
                            plotlyOutput("grafico_frecuencias"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "ciudades",
                fluidRow(
                    tags$h3("Ciudades con más tweets sobre afganistán",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        sliderInput("slider", "Número de ciudades:", 2, 20, 7),
                        dateRangeInput(inputId = "rango2",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-20",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary",
                        shinycssloaders::withSpinner(
                            plotlyOutput("grafico_ciudades"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "users",
                fluidRow(
                    tags$h3("Usuarios con más tweets sobre afganistán",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        sliderInput("slider2", "Número de usuarios:", 2, 20, 7),
                        dateRangeInput(inputId = "rango3",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-18",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary",
                        shinycssloaders::withSpinner(
                            plotlyOutput("grafico_usuarios"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "hashtags",
                fluidRow(
                    tags$h3("Hastags mayormente utilizados en los tweets",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        sliderInput("slider3", "Número de hashtags:", 2, 20, 7),
                        dateRangeInput(inputId = "rango4",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-18",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary", 
                        shinycssloaders::withSpinner(
                            plotlyOutput("grafico_hashtags"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "menciones",
                fluidRow(
                    tags$h3("Menciones mayormente utilizadas en los tweets",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        sliderInput("slider4", "Número de menciones:", 2, 20, 7),
                        dateRangeInput(inputId = "rango5",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-18",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary",
                        shinycssloaders::withSpinner(
                            plotlyOutput("grafico_menciones"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "emojis",
                fluidRow(
                    tags$h3("Emojis mayormente utilizados en los tweets",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        sliderInput("slider5", "Número de emojis:", 2, 20, 7),
                        dateRangeInput(inputId = "rango6",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-18",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary", 
                        shinycssloaders::withSpinner(
                            plotlyOutput("grafico_emojis"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "nube_palablas",
                fluidRow(
                    tags$h3("Nube de palabras más frecuentes",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        sliderInput("slider6", "Número de palabras:", 50, 200, 50),
                        dateRangeInput(inputId = "rango7",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-20",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary", 
                        shinycssloaders::withSpinner(
                            wordcloud2Output("grafico_nube_palabras"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "barra_emociones",
                fluidRow(
                    uiOutput(outputId ='title_pg_sentimiento'),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        selectInput("tipo_grafico", label = "Graficar:", 
                                    choices = list("Analisis de sentimiento" = 1,
                                                   "Polaridad de los tweets" = 2
                                    ),
                                    selected = 1),
                        dateRangeInput(inputId = "rango8",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-18",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary",
                        shinycssloaders::withSpinner(
                            plotlyOutput("g_barra_sentimientos"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "nube_emociones",
                fluidRow(
                    tags$h3("Nube de palabras más frecuentes en cada emoción",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        selectInput("select_sentimiento", label = "Sentimiento:", 
                                    choices = list("Alegria" = 1,
                                                   "Anticipación" = 2,
                                                   "Confianza" = 3,
                                                   "Disgusto"= 4,
                                                   "Ira"= 5,
                                                   "Miedo"= 6,
                                                   "Sorpresa"= 7,
                                                   "Tristeza"= 8
                                                   ),
                                                selected = 1),
                        sliderInput("slider7", "Número de palabras:", 100, 200, 100),
                        dateRangeInput(inputId = "rango9",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-20",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es")
                    ),
                    box(width=9,
                        status = "primary", 
                        shinycssloaders::withSpinner(
                            wordcloud2Output("g_nube_sentimientos"),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        ),
        tabItem(tabName = "mapa_paises",
                fluidRow(
                    tags$h3("Mapa de los paises y su cantidad de tweets",align = "center"),
                    box(width=3,
                        title = "Filtros del gráfico:", status = "primary",
                        dateRangeInput(inputId = "rango10",
                                       label = "Intervalo de fecha:", 
                                       start  = "2021-08-17",
                                       end    = "2021-08-20",
                                       min    = "2021-08-17",
                                       max    = "2021-08-20",
                                       format = "yyyy/mm/dd",
                                       separator = " - ",
                                       language ="es"),
                        selectInput("select_mapa", label = "Tipo de mapa:", 
                                    choices = list("Básico"="basic",
                                                   "Oscuro" = "dark",
                                                   "Claro" = "light",
                                                   "Satélite" = "satellite"
                                    ),
                                    selected="dark"
                    )),
                    box(width=9,
                        status = "primary",
                        shinycssloaders::withSpinner(
                            plotlyOutput("g_mapa_paises",height = 600),
                            type = 4, color = "#007bff", size = 1
                        )
                    )
                )
        )
    )
)

sidebar=dashboardSidebar(
    sidebarMenu(id = "sidebarID",
                menuItem("Inicio ", tabName = "home", icon = icon("home")),
                menuItem(" Tweets populares",icon = icon("twitter"),
                         menuSubItem('Tweets más retwitteados', tabName = 'retwitteados',icon=icon("retweet")),
                         menuSubItem('Tweets con más likes', tabName = 'favoritos',icon=icon("heart"))),
                menuItem("Gráficos dinámicos",icon = icon("glyphicon glyphicon-stats",lib="glyphicon"),
                         menuSubItem("Comportamiento en el tiempo ", tabName = "comportamiento-tiempo",icon = icon("chart-area")),
                         menuSubItem("Ciudades más activas ", tabName = "ciudades",icon = icon("city")),
                         menuSubItem("Usuarios más activos ", tabName = "users",icon = icon("users")),
                         menuSubItem("Hashtags más usados ", tabName = "hashtags",icon = icon("hashtag")),
                         menuSubItem("Menciones más usadas ", tabName = "menciones",icon = icon("at")),
                         menuSubItem("Emojis más usados ", tabName = "emojis",icon = icon("icons")),
                         menuSubItem("Mapa países con sus tweets ", tabName = "mapa_paises", icon = icon("map"))
                ),
                menuItem(" Análisis de los tweets",icon = icon("vial"),
                         menuSubItem('Nube de palabras frecuentes', tabName = 'nube_palablas',icon=icon("cloud")),
                         menuItem('Análisis de sentimientos', tabName = 'analisis_sentimientos',icon=icon("laugh-beam"),
                                     menuSubItem("Graficos de barras ", tabName = "barra_emociones",icon = icon("chart-bar")),
                                     menuSubItem("Nubes de palabras", tabName = "nube_emociones",icon = icon("cloud")))
                )
    )
)

shinyUI(
    dashboardPage(title=tags$head(tags$link(rel="shortcut icon", href="afganistan.ico"),tags$title("Crisis afganistán")),
    skin = "black",
    dashboardHeader(title = "Crisis Afganistán"),
    sidebar,
    body
))
options(shiny.jquery.version = 1)