# Code écrit par Giovanni Zanitti (giovanni.zanitti@gmail.com)
# Dernière MAJ 04/08/16
# Ce fichier contient toute les lignes de commandes nécessaire à produire l'interface du tableau de bord
# Il va de paire avec le fichier UI.R


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
      #Choix du MOOC
      selectInput("sel_MOOC", #Nom donné à l'input
                  "MOOC", #Titre de la zone
                  choices = unique(as.character(dff$name_course)) #Choix proposés
                  ),
      # Choix de la session
      uiOutput("selec_Session"),
      # Nombre d'inscrit
      h1(textOutput("Nb_inscrit")),
      # Nombre de répondant au questionnaire de début
      h4(textOutput("Nb_debut")),
      # Pourcentage associé
      textOutput("Nb_debut2"),
      # Nombre de répondant au questionnaire de fin
      h4(textOutput("Nb_fin")),
      # Pourcentages associés
      textOutput("Nb_fin2"),
      # Nombre de répondant aux deux questionnaires
      h4(textOutput("deb_fin"))
      ),
    # Affichage principal
    mainPanel(
      #Liste d onglets
      tabsetPanel(
        ########################### Onglet "Info générales"
        tabPanel("Info generales", #Nom de l'onglet
                h1("Inscriptions"), #h1() : format titre 1, textOutput() : Appel d'un "renderText()"
                 # On "casse" le tableau en deux colonne
                fluidRow(
                  #Colonne 1 (largeur 6 sur 12), Nb d'inscrit concernés par le diagramme des genres (cf "Nb_pie_inscrit" dans server.R)
                  column(6,textOutput("Nb_pie_inscrit")),
                  #Colonne 2 (largeur 6 sur 12), Nb d'inscrit concernés par le diagramme des classes d'âges
                  column(6,textOutput("Nb_age_inscrit"))
                ),
                 fluidRow(
                   #Colonne 1 (largeur 6 sur 12), Genre des inscrits
                   column(6,plotOutput("pie_inscrit")),
                   #Colonne 2, Classe d'age des inscrits
                   column(6,plotOutput("age_inscrit"))
                 ),
                # Objet permettant d'ajouter des barres verticales sur la courbe
                 dateRangeInput("dates", label = "Date"),
                 # Evolution des inscriptions
                 plotOutput("courbe_inscrit"),
                 
                # Titre 
                 h1("Questionnaire du début de cours"),
                fluidRow(
                  column(6,textOutput("Nb_debut3")),
                  column(6,textOutput("Nb_age_debut"))
                )
                ,
                 fluidRow(
                   column(6,plotOutput("pie_debut")),
                   column(6,plotOutput("age_debut"))
                 ),
                
                # Titre
                 h1("Questionnaire de fin de cours"),
                fluidRow(
                  column(6,textOutput("Nb_fin3")),
                  column(6,textOutput("Nb_age_fin"))
                )
                ,
                 fluidRow(
                   column(6,plotOutput("pie_fin")),
                   column(6,plotOutput("age_fin"))
                 ),
                
                # Titre
                 h1("Evolution entre debut et fin"),
                 h6("Ces tableaux donnent l'evolution de la part de chaque groupe d'individu entre le questionnaire du debut et le questionnaire de fin"),
                 fluidRow(
                   column(6,dataTableOutput("tab_Genres")),
                   column(6,dataTableOutput("tab_Ages"))
                 )
                 
                 ),
        
        
        ########################### Onglet "Graphique à une variable"
        tabPanel("Graphique à une variable", #Titre de l'onglet
                 fluidRow(
                   #Choix du questionnaire (stocké dans "input$sel_Quest")
                   column(4,selectInput("sel_Quest","Questionnaire",
                                        choices = c("debut","fin"),
                                        selected = "debut")),
                   #Choix de la variable (cf "selec_VarUni" dans server.R)
                   column(4,uiOutput("selec_VarUni"))
                 ),
                 # Texte titre du graphique
                 h2(textOutput("text_uni")),
                 # Rappel de l'intitulé de la question
                 h3(textOutput("Quest_Uni")),
                 # NOmbre d'inscrits concernés par le graphique
                 textOutput("Nb_desc"),
                 # Graphique
                 plotOutput("plotUni",width = "100%", height = "600px")
        ),
        
        
        ########################### Onglet "Graphique à deux variables"
        tabPanel("Graphique à deux variables",
                 fluidRow(
                   # Choix du questionnaire (stocké dans "input$sel_Questt")
                   column(4,selectInput("sel_Questt","Questionnaire",
                                        choices = c("debut","fin"),
                                        selected = "debut")),
                   # Choix de la variable 1
                   column(4,uiOutput("selec_VarBiv1")),
                   # Choix de la variable 2
                   column(4,uiOutput("selec_VarBiv2"))
                 ),
                 h2(textOutput("text_biv1")),
                 h2(textOutput("text_biv2")),
                 plotOutput("plotBiv",width = "100%", height = "600px")
                 ),
        # tabPanel("Jauges",
        #          h2(textOutput("text_jauge")),
        #          plotOutput("JJauge",width = "100%", height = "600px")),
        
        ########################### Onglet "Carte"
        tabPanel("Carte", #Titre de l'onglet
                 # Titre
                 h2(textOutput("text_map")),
                 textOutput("text_map2"),
                 # Carte des inscrits par pays d'origines en fonction du MOOC et de la session sélectionnés
                 leafletOutput("MapPos", width = "100%", height="500")
                 # h3(textOutput("text_top10")),
                 # textOutput("Hors France"),
                 # leafletOutput("map_top10")
        
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