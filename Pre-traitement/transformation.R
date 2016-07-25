library(sqldf)

# Chemin du répertoire de travail :
path = "C:/Users/gzanitti/Documents/Enquete/"

## Questionnaire du début de cours ("MOOC/fichier") :
path_deb = "Innov_Entre_Num/MinesTelecom_04014_session01_Mieux_vous_connaitre_2016-06-23-140410"

## Réponses associées à ce questionnaire de début de cours ("MOOC/Fichier") :
path_rep_deb = "Innov_Entre_Num/rep_quest_deb_04014"

## Questionnaire de fin de cours ("MOOC/Fichier") :
path_fin = "Innov_Entre_Num/MinesTelecom_04014_session01_Questionnaire_de_satisfaction_2016-06-23-141224"

## Réponses associées à ce questionnaire de fin de cours ("MOOC/Fichier") :
path_rep_fin = "Innov_Entre_Num/rep_quest_fin_04014"

#Code du cours :
code_cours = "MinesTelecom/04014/session01"


# Importation des données

deb <- read.csv(paste(path,"Pre-traitement/data/",path_deb,".csv", sep = ""), header = T)
fin <- read.csv(paste(path,"Pre-traitement/data/",path_fin,".csv", sep = ""), header = T)
# Ne pas modifier l'emplacement ni le titre des fichiers ci-dessous
inscr <- read.csv(paste(path, "Pre-traitement/data/list-enrollments-MinesTelecom.csv", sep = ""), header = T) 
rep_deb <- read.csv(paste(path,"Pre-traitement/data/",path_rep_deb,".csv", sep = ""), header = T)
rep_fin <- read.csv(paste(path,"Pre-traitement/data/",path_rep_fin,".csv", sep = ""), header = T)

# Fonctions

ReplaceNA <- function(df){
  m_df <- as.matrix(df)
  m_df[which(is.na(m_df))] <- "Vide"
  return(data.frame(m_df))
}

# Remplacement des valeurs vide par le mot "vide"

deb <- ReplaceNA(deb)
fin <- ReplaceNA(fin)
inscr <- ReplaceNA(inscr)
names(inscr)[1] <- "course"



var_rep_deb <- names(rep_deb)[-1]
var_deb <- names(deb[,grep("q",colnames(deb))])

deb_clear <- sqldf(paste("Select deb.*, rep_deb.",var_rep_deb[1]," from deb, rep_deb where deb.",var_deb[1]," = rep_deb.Choice", sep = ""))
for(i in 2:length(var_deb)){
  deb_clear <- sqldf(paste("Select deb_clear.*, rep_deb.",var_rep_deb[i]," from deb_clear, rep_deb where deb_clear.",var_deb[i]," = rep_deb.Choice", sep = ""))
}

deb_clear <- deb_clear[,-grep("q",substr(colnames(deb_clear),0,1))]




#test <- sqldf("Select * from inscr where inscr.course ='MinesTelecom/04003S04/session04'")

df_final <- sqldf(paste("Select inscr.course,deb_clear.*,inscr.enrollment_time,inscr.country,inscr.certificate,inscr.unenroll 
                        from deb_clear,inscr 
                        where deb_clear.id = inscr.user and inscr.course = '",code_cours,"'", sep = ""))

df_final <- cbind(Quest = "debut",df_final)





#### FIN

var_rep_fin <- names(rep_fin)[-1]
var_fin <- names(fin[,grep("q",colnames(fin))])
fin_clear <- sqldf(paste("Select fin.*, rep_fin.",var_rep_fin[1]," from fin, rep_fin where fin.",var_deb[1]," = rep_fin.Choice", sep = ""))
for(i in 2:length(var_fin)){
  fin_clear <- sqldf(paste("Select fin_clear.*, rep_fin.",var_rep_fin[i]," from fin_clear, rep_fin where fin_clear.",var_fin[i]," = rep_fin.Choice", sep = ""))
}
fin_clear <- fin_clear[,-grep("q",substr(colnames(fin_clear),0,1))]




df_final_fin <- sqldf(paste("Select inscr.course,fin_clear.*,inscr.enrollment_time,inscr.country,inscr.certificate,inscr.unenroll 
                        from fin_clear,inscr 
                        where fin_clear.id = inscr.user and inscr.course = '",code_cours,"'", sep = ""))

df_final_fin  <- cbind(Quest = "fin",df_final_fin)


dir.create(paste(path,"Application/data/MOOC/",sub("/.*", "", path_deb),sep = ""))
write.csv(df_final,file.path(paste(path,"Application/data/MOOC/",path_deb,"_traite.csv", sep = "")))
write.csv(df_final_fin,file.path(paste(path,"Application/data/MOOC/",path_fin,"_traite.csv", sep = "")))

