packages <- c("shiny", "shinythemes", "shinyWidgets", "shinyanimate", "leaflet", "lubridate", "osrm", "chron", "sp", "shinyjs", "shinycssloaders","plotly","shinyalert","highcharter","raster","DT","geosphere")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#library(shiny)
#library(shinythemes)
#library(shinyWidgets)
#library(shinyanimate)
#library(leaflet)
#library(tidyverse)
#library(lubridate)
#library(osrm)
#library(chron)
#library(sp)
#library(shinyjs)
#library(shinycssloaders)
#library(plotly)
#library(shinyalert)
#library(highcharter)
#library(raster)
#library(DT)
#library(geosphere)

ui <- fluidPage(
  chooseSliderSkin(
    skin = "Square",
  ),
  useShinyjs(),
  navbarPage(theme = shinytheme("sandstone"),
             title="ggwpShiny",
             id = "navbar",
             position = "fixed-top",
             tabPanel("Inicio",
                      value = "home",
                      withAnim(),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                      ),
                      br(),
                      br(),
                      wellPanel(h1("Inicio", style = "color: rgb(0,0,0)"),
                                h4("Nuestro equipo está formado por Kevin N. Dietrich, Antonio Ferrer Sánchez y Sergio Juanes Tébar.",
                                   style = "color: rgb(0,0,0)"),
                                h4("Somos estudiantes de la ETSE, en la Universidad de Valencia.",
                                   style = "color: rgb(0,0,0)"),
                                style = "background: rgb(176, 176, 176)"),
                      hr(),
                      wellPanel(h1("¿Qué hemos planteado?", style = "color: rgb(0,0,0)"),
                                h4("Disponemos de una base de datos con trayectos entre municipios de España y Portugal con otros municipios de toda Europa, donde siempre aparecerá un municipio de uno de estos dos como origen o destino.", style = "color: rgb(0,0,0), text-align:justify"),
                                h4("El objetivo del proyecto ha sido realizar, mediante una aplicación web, un análisis descriptivo general de dicha base de datos así como una visualización de estos mediante herramientas de gráficas y mapas.", style = "color: rgb(0,0,0)"),
                                h4("Se ha considerado un estudio de los municipios tanto univariante como bivariante, es decir, tanto municipio a municipio como tomando pares origen - destino. Todo esto ha sido posible gracias a una base de datos adicional con información sobre los lugares obtenida mediante técnicas de analítica web.", style = "color: rgb(0,0,0), text-align:justify"),
                                h4("Por último, se ha considerado el algoritmo PageRank de Google para, estableciendo una analogía entre páginas web y ciudad / municipios, determinar una serie de rankings para los 20584 lugares distintos que aparecen como origen y/o destino.", style = "color: rgb(0,0,0), text-align:justify"),
                                style = "background: rgb(176, 176, 176)"),
                      hr()
             ),
             tabPanel("Análisis exploratorio de datos",
                      value = "aed",
                      withAnim(),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                      ),
                      wellPanel(h1("Análisis exploratorio de datos", style = "color: rgb(0,0,0)"),
                                h4("Breve descripción de las variables disponibles, de sus características generales y de los aspectos a tener en cuenta, así como una representación gráfica de las mismas.", style = "color: rgb(0,0,0)"),
                                style = "background: rgb(176, 176, 176)"),
                      hr(),
                      fluidRow(style = "position:relative",
                               column(4,
                                      wellPanel(id = "num_viajes",
                                                p("11 346 914",class="num"),
                                                h4("Trayectos registrados",style="text-align:center;"),
                                                style="text-align:center;height:300px;align-content: center;display:flex;justify-content:center;flex-direction:column")
                               ),
                               column(4,
                                      wellPanel(id = "tiempo",
                                                p("1 Nov 2017",class="num2"),
                                                p("31 Oct 2019",class="num2"),
                                                h4("Tiempo de registro de los datos",style="text-align:center;"),
                                                p("2 años",class="num"),
                                                style="text-align:center;height:300px;align-content: center;display:flex;justify-content:center;flex-direction:column;"
                                      )
                               ),
                               column(4,
                                      wellPanel(id="espyport",img(src="espypor.svg",width="200px"),
                                                p("España y Portugal",class="num2"),
                                                h4("siempre presentes en origen o destino",style="text-align:center;"),
                                                style="text-align:center;height:300px;"
                                      )
                               )
                      ),
                      fluidRow(
                        column(4,
                               wellPanel(
                                 p("18780",class="num"),
                                 h4("puntos de origen diferentes", style="text-align:center;")
                               )
                        ),
                        column(4,
                               wellPanel(
                                 p("9935354", class = "num"),
                                 h4("datos perdidos, lo cual supone un ~8% del total", style="text-align:center;")
                               ),
                        ),
                        column(4,
                               wellPanel(
                                 p("31 países", class = "num"),
                                 h4("extra, tales como Francia, Alemania e Italia", style="text-align:center;")
                               ),
                        )
                      ),
                      fluidRow(
                        column(6,
                               wellPanel(
                                 "Aquí presentamos un análisis gráfico tanto del precio medio del viaje por 
                                  kilómetro, así como de las tasas de asientos y viajes confirmados, obtenidas 
                                  tras promediar sobre sobre todos los días disponibles en la base de datos. 
                                  Asimismo también presentamos una visualización de las correlaciones de 
                                  Pearson entre los promedios temporales de las diferentes características 
                                  numéricas. Como cabría esperar, las correlaciones más positivas aparecen 
                                  para pares de variables tales como los viajes confirmados y los asientos 
                                  confirmados mientras que vemos, también, correlaciones negativas para 
                                  variables como el promedio de asientos ofertados con respecto al promedio 
                                  del precio / km del viaje.", style="text-align:justify"
                               )
                        ),
                        column(6,
                               wellPanel(
                                 "En este bloque presentamos una visualización que nos permite ver la cantidad de lugares distribuidos según el país correspondiente.", style="text-align:justify"
                               )
                        )
                      ),
                      fluidRow(
                        column(6,
                               tabsetPanel(
                                 tabPanel("Series temporales",
                                          wellPanel(
                                            uiOutput("output_3") %>% withSpinner(color="#0dc5c1"),
                                            plotlyOutput("bivariante_1") %>% withSpinner(color="#0dc5c1")
                                          )
                                 ),
                                 tabPanel("Correlaciones",
                                          wellPanel(
                                            plotlyOutput("heatmap_1") %>% withSpinner(color="#0dc5c1")
                                          )
                                 ),
                                 tabPanel("Análisis univariable",
                                          wellPanel(
                                            uiOutput("output_6") %>% withSpinner(color="#0dc5c1"),
                                            uiOutput("output_7") %>% withSpinner(color="#0dc5c1")
                                          )
                                 )
                               )
                        ),
                        column(6,
                               wellPanel(
                                 uiOutput("output_4") %>% withSpinner(color="#0dc5c1"),
                                 plotlyOutput("bivariante_4") %>% withSpinner(color="#0dc5c1")
                               )
                        )
                      )
             ),
             tabPanel("Análisis univariante",
                      value = "univariante",
                      withAnim(),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                      ),
                      wellPanel(h1("Análisis univariante", style = "color: rgb(0,0,0)"),
                                h4("Analizaremos los datos relacionados con un único municipio, analizando mediante un mapa las diferentes conexiones del municipio con el resto de los presentes en la base de datos. Permitimos también al usuario analizar un cierto lugar como origen así como destino visualizando sus principales conexiones mediante un treemap.", style = "color: rgb(0,0,0)"),
                                style = "background: rgb(176, 176, 176)"),
                      hr(),
                      fluidRow(
                        column(4,
                               wellPanel(
                                 uiOutput("univ") %>% withSpinner(color="#0dc5c1")
                               ),
                               wellPanel(
                                 "Selecciona un intervalo de tiempo a analizar",
                                 br(),br(),
                                 uiOutput("fecha1_univ"),
                                 uiOutput("fecha2_univ"),
                                 fluidRow(
                                   column(4,"Tráfico: "),column(8,textOutput("trafico"))),
                                 fluidRow(
                                   column(6,"Factor de impacto: "),column(6,textOutput("impacto"))
                                 ),br(),
                                 radioButtons("origen_destino","Analizar el lugar como",choices=c("Origen","Destino"), inline = T)
                               )
                        ),
                        column(8,wellPanel(
                          h2("Conexiones del lugar seleccionado presentes en la base de datos")
                        ),
                        plotOutput("mapa_conexiones")%>% withSpinner(color="#0dc5c1")
                        )
                      ),
                      wellPanel(
                        highchartOutput("treemap") %>% withSpinner(color="#0dc5c1")
                      ),
                      hr()
             ),
             tabPanel("Análisis bivariante",
                      value = "bivariante",
                      withAnim(),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                      ),
                      wellPanel(h1("Análisis bivariante", style = "color: rgb(0,0,0)"),
                                h4("Se lleva a cabo un estudio y análisis visual de los viajes entre el 
                                       origen y destino seleccionados por el usuario.", style = "color: rgb(0,0,0)"),
                                p("Analizamos las relaciones entre dos lugares (origen y destino) mediante 
                                      el uso de mapas dinámicos e interactivos así como utilizando 
                                      representaciones gráficas. Dicho análisis se realiza tanto a nivel 
                                      general como particular atendiendo a las elecciones del usuario.",
                                  style = "color: rgb(0,0,0)"),
                                style = "background: rgb(176, 176, 176)"),
                      fluidRow(
                        column(3,
                               wellPanel(
                                 uiOutput("origen_in"),
                                 uiOutput("destino_in") %>% withSpinner(color="#0dc5c1")
                               )
                        ),
                        column(9,
                               wellPanel(
                                 leafletOutput("ruta") %>% withSpinner(color="#0dc5c1")
                               )
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(6,
                               wellPanel(
                                 "En esta gráfica es posible visualizar la evolución del precio medio por 
                                   kilómetro de los viajes a lo largo del tiempo, así como las tasas de 
                                   asientos y viajes confirmados con respecto al total y de ofertantes nuevos."
                               )  
                        ),
                        column(6,
                               wellPanel(
                                 "Aquí presentamos la posibilidad de visualizar la proporción de asientos 
                                   y viajes confirmados, así como de ofertantes nuevos, para un día en 
                                   concreto utilizando la combinación origen - destino dispuesta arriba."
                               )
                        )
                      ),
                      fluidRow(
                        column(6,
                               wellPanel(
                                 uiOutput("output_1"),
                                 uiOutput("output_8"),
                                 plotlyOutput("bivariante_2") %>% withSpinner(color="#0dc5c1")
                               )
                        ),
                        column(6,
                               wellPanel(
                                 fluidRow(
                                   column(7,
                                          uiOutput("output_2") %>% withSpinner(color="#0dc5c1")
                                   ),
                                   column(5,
                                          uiOutput("output_fecha_1") %>% withSpinner(color="#0dc5c1")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          plotlyOutput("bivariante_3") %>% withSpinner(color="#0dc5c1")
                                   )
                                 )
                               )
                        )
                      ),
                      hr()
             ),
             tabPanel("Blablarank",
                      value = "blablarank",
                      withAnim(),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                      ),
                      wellPanel(h1("BlaBlaRank", style = "color: rgb(0,0,0)"),
                                h4("Un ranking para evaluar el impacto y tendencia de los lugares."),
                                p("Inspirado en el PageRank de Google, hemos interpretado los lugares 
                                      conectados por BlaBlaCar como nodos de una red de conexiones, lo 
                                      que nos ha permitido establecer un paralelismo con el famoso 
                                      algoritmo de posicionamiento web y evaluar la importancia de los destinos.", style = "color: rgb(0,0,0)"),
                                style = "background: rgb(176, 176, 176)"),
                      tabsetPanel(
                        tabPanel("Rank",
                                 br(),
                                 fluidRow(
                                   column(4,
                                          wellPanel(uiOutput("fecha_rank")),
                                          wellPanel(h4("Ranking"),DT::dataTableOutput("ranklist") %>% withSpinner(color="#0dc5c1"))
                                   ),
                                   column(8,
                                          wellPanel("En este mapa se puede encontrar una visualización interactiva de los ranks para municipios de España y Portugal en el día seleccionado."),
                                          wellPanel(leafletOutput("rank_abs",height=500) %>% withSpinner(color="#0dc5c1"))
                                   )
                                 )
                        ),
                        tabPanel("Variación del Rank",
                                 br(),
                                 fluidRow(
                                   column(3,wellPanel("Selecciona dos días entre los que deseas analizar la evolución del Rank",
                                                      br(),br(),
                                                      uiOutput("fecha1"),
                                                      uiOutput("fecha2")
                                   ),
                                   wellPanel("Si seleccionas un municipio en el mapa, podrás ver una serie temporal del Rank entre las dos fechas")
                                   ),
                                   column(9,
                                          wellPanel("El mapa interactivo proporciona la variación entre los días elegidos como porcentaje respecto al día 1. Un incremento de +100% equivaldría a duplicar el Rank"),
                                          wellPanel(leafletOutput("rank") %>% withSpinner(color="#0dc5c1"))    
                                   )
                                 ),
                                 wellPanel(highchartOutput("timerank"))
                        )
                      )
             ),
             tags$style(type="text/css", "body {padding-top: 70px;}")
             
             
  ))