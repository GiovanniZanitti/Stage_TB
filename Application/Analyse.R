# Code écrit par Giovanni Zanitti (giovanni.zanitti@gmail.com)
# Dernière MAJ 04/08/16
# Ce fichier contient l'import des données pour le tableau de bord ainsi que quelques manipulations sur celles-ci
# Il est appelé par les fichiers server.R et UI.R et est donc indispensable au bon fonctionnement du tableau de bord



library(sqldf)
library(ggplot2)
library(leaflet)
library(gtools)
library(dplyr)
library(mapdata)
library(ggplot2)
library(dplyr)
library(stringr)
library(proto)
library(gridSVG)
library(ggmap)
library(grid)
library(RColorBrewer)


#Fonctions

ReplaceNA <- function(df){
  m_df <- as.matrix(df)
  m_df[which(m_df == "")] <- "Vide"
  m_df[which(m_df == "None")] <- "Vide"
  return(data.frame(m_df))
}

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

geom_tooltip <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", real.geom = NULL, ...) {
  rg <- real.geom(mapping = mapping, data = data, stat = stat, ## â 
                  position = position, ...)
  
  rg$geom <- proto(rg$geom, { ## â¡
    draw <- function(., data, ...) {
      grobs <- list()
      for (i in 1:nrow(data)) {
        grob <- .super$draw(., data[i,], ...) ## â¢
        grobs[[i]] <- garnishGrob(grob,  ## â£
                                  `data-tooltip`=data[i,]$tooltip)
      }
      ggplot2:::ggname("geom_tooltip", gTree(
        children = do.call("gList", grobs)
      ))
    }
    required_aes <- c("tooltip", .super$required_aes)
  })
  
  rg ## â¤
}

#########################    Import des donnÃ©es

#### CoordonnÃ©es des pays

coord_country <- read.csv(file = "data/coord_country.csv", header = T)


#### Cours

course <- read.csv("data/Courses.csv", header = T, encoding="UTF-8")


#### Inscriptions

inscr <- read.csv(file = "data/list-enrollments-MinesTelecom.csv", header = T)
# Calcul de l'âge
inscr <- cbind(inscr, Age = as.numeric(format(as.Date(inscr$enrollment_time), format = "%Y")) - as.numeric(paste(inscr$year_of_birth)))
inscr$Age <- as.numeric(inscr$Age)
names(inscr)[1] <- "course"
# Passage de "enrollment_time" en type "Date" pour pouvoir l'utiliser dans les graphiques comme telle
inscr = cbind(inscr,Date = as.Date(inscr$enrollment_time))


### dff (Final dataframe)

dff <- data.frame()
df_var = data.frame(course = NULL,session = NULL, Quest = NULL,variable = NULL)
# Pour chaque MOOC
for(i in list.files("data/MOOC/")){
  # Pour chaque session de chaque MOOC
  for(j in list.files(paste("data/MOOC/",i, sep = ""))){
    # Pour chaque fichier de chaque session de chaque MOOC
    for(k in list.files(paste("data/MOOC/",i,"/",j, sep =""))){
      # On importe le fichier
      data <- read.csv(paste("data/MOOC/",i,"/",j,"/",k,sep = ""), header = T)
      # On crée deux nouvelles variables donnant le code du cours et la session
      data <- cbind(data, short_code = substr(data$course,0,18), session = sub(".*/", "", data$course))
      # On récupère le nom exact du cours
      data = sqldf("Select data.*, course.name_course from data, course where short_code = code_course")
      #On récupère le nom des variables de ce fichier
      vec <- names(data)
      df_var_temp <- data.frame(course = as.character(unique(data$name_course)), session = as.character(unique(data$session)), Quest = as.character(unique(data$Quest)), variable = vec)
      # On stocke les variables dans le df "df_var"
      df_var <- rbind(df_var,df_var_temp)
      # On stocke le fichier importé et modifié dans dff
      dff <- smartbind(dff,data)
    }
  }
}

#Suppression premi?re colonne
dff <- dff[-1]
#Calcul de l'?ge
dff <- cbind(dff, Age = as.numeric(format(as.Date(dff$enrollment_time), format = "%Y")) - as.numeric(paste(dff$year_of_birth)))

#Calcul des classes d'?ge
dff <- cbind(dff, C_Age = cut(dff$Age, breaks = c(0, 13, 17, 24, 34, 44, 54, 64, 100), include.lowest = TRUE))
inscr <- cbind(inscr, C_Age = cut(inscr$Age, breaks = c(0, 13, 17, 24, 34, 44, 54, 64, 100), include.lowest = TRUE))
#D?composition du code cours en "short_code" et en session
inscr = cbind(inscr, short_code = substr(inscr$course,0,18), session = sub(".*/", "", inscr$course))
inscr = sqldf("Select inscr.*, course.name_course from inscr, course where short_code = code_course")
#Remplacement des valeurs vides
dff<-ReplaceNA(dff)
#Passage de la variable Age et year_of_birth en quantitative
dff$Age <- as.numeric(dff$Age)
dff$year_of_birth <- as.numeric(dff$year_of_birth)
#Suppression des accents
dff$country<-Unaccent(dff$country)
#Liste de toutes les variables de dff
List_Var_Tot = as.character(names(dff))


# Import du fichier où sont stockés les intitulés des questions
Questi <- read.csv("data/questions.csv", header = T, encoding = "UTF-8")

########### Donn?es pour les cartes


df_map <- sqldf("select inscr.*,c.lat, C.lon from inscr, coord_country c where inscr.country = c.country")

df_map = df_map[!is.na(df_map$lat),]

#dataset
df_map <- sp::SpatialPointsDataFrame(
  cbind(
    df_map$lon,
    df_map$lat
  ),
  data.frame(Country = df_map$country, course = df_map$name_course, session = df_map$session)
)




#Variable servant dans server.R pour rendre r?actif le choix des variables
final <- NULL
for (i in List_Var_Tot){
  commands <- paste("'",i,"' = dff[dff$Quest == input$sel_Quest & dff$name_course == input$sel_MOOC & dff$session == input$sel_Sess,]$",i,",", sep = "")
  final <- paste(final,commands,sep = "")
}
final <- substr(final,start = 0, stop = nchar(final)-1)



cols_gender <- setNames(c("lightblue","pink","lightgreen"),c("m","f","o"))

# Satis = data.frame(course = NA, session = NA)
# for (i in unique(as.character(dff$name_course))){
#   for (j in unique(as.character(dff[dff$name_course == i,]$session))){
#   MOOC_fin <- dff[dff$name_course == i & dff$Quest == 'fin' & dff$session == j,
#                   -which(is.na(dff[dff$name_course == i & dff$Quest == 'fin' & dff$session == j,][1,]))]
#   
#   MOOC_fin <- MOOC_fin[,grep("Satis",substr(colnames(MOOC_fin),0,5))]
#   l <- names(MOOC_fin)
#   Satis = smartbind(Satis,l)
#   Satis[nrow(Satis),]$session <- j
#   Satis[nrow(Satis),]$course <- i
#   }
# }
# 
# Satis<-Satis[-1,]


#Code servant pour les jauges
Satis = data.frame(course = NA)
for (i in unique(as.character(dff$name_course))){

    MOOC_fin <- dff[dff$name_course == i & dff$Quest == 'fin',
                    -which(is.na(dff[dff$name_course == i & dff$Quest == 'fin',][1,]))]
    
    MOOC_fin <- MOOC_fin[,grep("Satis",substr(colnames(MOOC_fin),0,5))]
    l <- names(MOOC_fin)
    Satis = smartbind(Satis,l)

    Satis[nrow(Satis),]$course <- i
  }


Satis<-Satis[-1,]



#Code pour avoir les donn?es pour le TOP 10 des pays repr?sent?s

nb_mooc_p <- sqldf("Select DISTINCT user,count(course) as nb,gender,age,education,country,C_Age from inscr group by user order by count(course) DESC")

n_course = levels(course$name_course)
Course_country_nb <- sqldf("Select name_course,country,count(user) as nb from inscr group by name_course, country ")
Course_country_nb <- ReplaceNA(Course_country_nb)
tab_nb_cours_country <- NULL
for (i in levels(course$name_course)){
  Nb_country_course <- sqldf(paste("Select country,sum(nb) as s_nb from Course_country_nb where name_course = '",i,"' group by country order by s_nb DESC", sep = ""))
  Nb_country_course <- data.frame(t(Nb_country_course))
  colnames(Nb_country_course) = as.character(unlist(Nb_country_course[1,]))
  Nb_country_course <- Nb_country_course[-1,]
  Nb_country_course <- cbind(course = i, Nb_country_course)
  #print(Nb_country_course)
  
  tab_nb_cours_country <- smartbind(tab_nb_cours_country,Nb_country_course)
}


tab_nb_cours_country <- tab_nb_cours_country[-1,]



inscr$session <- as.character(inscr$session)
dff$session <- as.character(dff$session)

