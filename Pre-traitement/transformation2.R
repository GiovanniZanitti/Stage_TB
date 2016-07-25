library(sqldf)
library(stringr)
library(gtools)

# Chemin du fichier :
path = "C:/Users/gzanitti/Documents/Enquete/Pre-traitement/data/Comprendre_4G/session01/MinesTelecom_04010_session01_Questionnaire_de_satisfaction_2016-07-21-135141"

# Chemin du 
path_rep = "C:/Users/gzanitti/Documents/Enquete/Pre-traitement/data/Comprendre_4G/session01/rep_quest_fin_04010"

#Code du cours :
code_cours = "MinesTelecom/04010/session01"

#Nom du questionnaire (debut ou fin) :
Nom_Quest = "fin"




cours_sess = sub(".*/data", "", path)
cours_sess = sub("/MinesTelecom.*","",cours_sess)
cours = sub("/sess.*","",cours_sess)
session = sub(".*/","",cours_sess)
sub(".*/","",path)
# Importation des données

df <- read.csv(paste(path,".csv", sep = ""), header = T)

# Ne pas modifier l'emplacement ni le titre des fichiers ci-dessous
inscr <- read.csv("C:/Users/gzanitti/Documents/Enquete/Pre-traitement/data/list-enrollments-MinesTelecom.csv", header = T) 
rep <- read.csv(paste(path_rep,".csv", sep = ""), header = T)


# Fonctions

ReplaceNA <- function(df){
  m_df <- as.matrix(df)
  m_df[which(is.na(m_df))] <- "Vide"
  return(data.frame(m_df))
}

# Remplacement des valeurs vide par le mot "vide"

df <- ReplaceNA(df)
inscr <- ReplaceNA(inscr)
names(inscr)[1] <- "course"

rep$Choice <- as.character(rep$Choice)

var_rep <- names(rep)[-1]
var_df <- names(df[,grep("q",colnames(df))])

dff <- data.frame()
for(k in 6:ncol(df)){
  df[,k] <- str_replace_all(as.character(df[,k]), "\\[", "")
  df[,k] <- str_replace_all(as.character(df[,k]), "u", "")
  df[,k] <- str_replace_all(as.character(df[,k]), "\'", "")
  df[,k] <- str_replace_all(as.character(df[,k]), "\\]", "")
  
  n_df = data.frame(user = NA,choice = NA)
  for(i in 1:nrow(df)){
    choice = unlist(str_split(df[i,k], ","))
    for(j in 1:length(choice)){
      n_df = rbind(n_df,c(as.character(df[i,]$id),choice[j]))
    }
  }
  n_df = n_df[-1,]
  df_temp <- df[,1:5]
  df_temp <- sqldf(paste("Select df_temp.*, n_df.choice as",var_df[k-5],"from df_temp, n_df where id = user"))
  df_temp <- sqldf(paste("Select df_temp.*, rep.",var_rep[k-5]," from df_temp, rep where df_temp.",var_df[k-5]," = rep.Choice", sep = ""))
  dff <- smartbind(dff,df_temp)
}


dff <- dff[,-grep("q",substr(colnames(dff),0,1))]




#test <- sqldf("Select * from inscr where inscr.course ='MinesTelecom/04003S04/session04'")

df_final <- sqldf(paste("Select inscr.course,dff.*,inscr.enrollment_time,inscr.country,inscr.certificate,inscr.unenroll 
                        from dff,inscr 
                        where dff.id = inscr.user and inscr.course = '",code_cours,"'", sep = ""))

df_final <- cbind(Quest = Nom_Quest,df_final)




dir.create(paste("C:/Users/gzanitti/Documents/Enquete/Application/data/MOOC",cours,"/", sep = ""))
dir.create(paste("C:/Users/gzanitti/Documents/Enquete/Application/data/MOOC",cours,"/",session,"/", sep= ""))
write.csv(df_final,file.path(paste("C:/Users/gzanitti/Documents/Enquete/Application/data/MOOC",cours,"/",session,"/",sub(".*/","",path),"_traite.csv", sep = "")))


