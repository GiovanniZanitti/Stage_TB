library(shiny)
library(leaflet)
library(magrittr)
library(leaflet)
library(plotly)
source("Analyse.R")
# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MOOCIMT"),
  
  # Sidebar with controls 
  sidebarLayout(
    sidebarPanel(
      selectInput("sel_MOOC", #Nom donné à l'input
                  "MOOC", #Titre de la zone
                  choices = unique(as.character(dff$name_course)) #Choix proposés
                  ),
      textOutput("texttest"),
      uiOutput("selec_Session"),
      textOutput("testtext"),
      h1(textOutput("Nb_inscrit")),
      h4(textOutput("Nb_debut")),
      textOutput("Nb_debut2"),
      h4(textOutput("Nb_fin")),
      textOutput("Nb_fin2"),
      h4(textOutput("deb_fin"))
      ),
    # Affichage principal
    mainPanel(
      #Liste d onglets
      tabsetPanel(
        #Onglets
        tabPanel("Info generales", #Nom de l'onglet
                h1("Inscriptions"), #h1() : format titre 1, textOutput() : Appel d'un "renderText()"
                 # On "casse" le tableau en deux colonne
                 fluidRow(
                   #Colonne 1 (largeur 6 sur 12), Genre des inscrits
                   column(6,plotOutput("pie_inscrit")),
                   #Colonne 2, Classe d'age des inscrits
                   column(6,plotlyOutput("age_inscrit"))
                 ),
                 dateRangeInput("dates", label = "Date range"),
                 # Evolution des inscriptions
                 plotlyOutput("courbe_inscrit"),
                 
                 h1("Questionnaire du début de cours"),
                textOutput("Nb_debut3"),
                 fluidRow(
                   column(6,plotOutput("pie_debut")),
                   column(6,plotlyOutput("age_debut"))
                 ),
                 h1("Questionnaire de fin de cours"),
                textOutput("Nb_fin3"),
                 fluidRow(
                   column(6,plotOutput("pie_fin")),
                   column(6,plotlyOutput("age_fin"))
                 ),
                 h1("Evolution entre debut et fin"),
                 h6("Ces tableaux donnent l'evolution de la part de chaque groupe d'individu entre le questionnaire du debut et le questionnaire de fin"),
                 fluidRow(
                   column(6,dataTableOutput("tab_Genres")),
                   column(6,dataTableOutput("tab_Ages"))
                 )
                 
                 ),
        
        
        
        tabPanel("Descriptif", #Titre de l'onglet
                 fluidRow(
                   column(4,selectInput("sel_Quest","Questionnaire",
                                        choices = c("debut","fin"),
                                        selected = "debut")),
                   column(4,uiOutput("selec_VarBiv1")),
                   column(4,uiOutput("selec_VarBiv2"))
                   
                 ),
                 # Carte trajet d'un conducteur dans la journee
                 h2(textOutput("text_uni")),
                 textOutput("Nb_desc"),
                 plotlyOutput("plotUni",width = "100%", height = "600px"),
                 h2(textOutput("text_biv1")),
                 h2(textOutput("text_biv2")),
                 plotlyOutput("plotBiv",width = "100%", height = "600px")
        ),
        tabPanel("Jauges",
                 h2(textOutput("text_jauge")),
                 plotOutput("JJauge",width = "100%", height = "600px")),
        tabPanel("Carte", #Titre de l'onglet
                 # Titre
                 h2(textOutput("text_map")),
                 textOutput("text_map2"),
                 # Carte trajet d'un conducteur dans la journee
                 leafletOutput("MapPos", width = "100%", height="500"),
                 h3(textOutput("text_top10")),
                 textOutput("Hors France"),
                 leafletOutput("map_top10")
        
        )
        # tabPanel("Tous les MOOCs",
        #          sliderInput("range", "Nombre de MOOC suivi au minimum : ", min = 0, max = max(nb_mooc_p$nb), value = 15, step = 1),
        #          textOutput("Nb_tt"),
        #          fluidRow(
        #            column(6,plotOutput("pie_gend_tt")),
        #            column(6,plotlyOutput("p_age_tt"))
        #          )
        )
      )
      
    )
    
  )
)