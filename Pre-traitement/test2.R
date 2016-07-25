library(ggplot2)
library(plotly)

sqldf("Select count(id) from df_final")
df_final <- cbind(df_final, Age = as.numeric(format(as.Date(df_final$enrollment_time), format = "%Y")) - as.numeric(paste(df_final$year_of_birth)))

df_final <- cbind(df_final, C_Age = cut(df_final$Age, breaks = c(0, 13, 17, 24, 34, 44, 54, 64, 100), include.lowest = TRUE))

p_age <- ggplot(data = sqldf("Select DISTINCT id,C_Age from df_final"), aes(C_Age), fill = factor(C_Age)) + #DonnÃ©es du graphique
  # Type de graphique
  geom_bar(aes(y = ((..count..)/sum(..count..))*100), color="white", fill="green") + 
  # Taille des Ã©lÃ©ments des axes
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, vjust=1)) + 
  # Suppression de la lÃ©gende
  theme(legend.position="none") + 
  # Titre
  ggtitle("% des inscrit par classe d'age") +
  # Titre de l'ordonnÃ©e
  ylab("(%)") +
  # Titre de l'abscisse
  xlab("Classe d'age")

# Affichage avec plotly
ggplotly(p_age)


p_age <- ggplot(data = dff[dff$name_course == input$sel_MOOC &  dff$Quest == "debut" ,], aes(C_Age), fill = factor(C_Age)) +
  geom_bar(aes(y = ((..count..)/sum(..count..))*100), color="white", fill="green") + 
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, vjust=1)) + 
  theme(legend.position="none") + 
  ggtitle("% des inscrit par classe d'age") +
  ylab("(%)") +
  xlab("Classe d'age")
p_age
ok <- sqldf("Select DISTINCT id, Age from dff where Quest = 'debut' and name_course = 'Comprendre la 4G' and session = 'session01'")

p_num <- ggplot(data = sqldf("Select DISTINCT id, Age from dff where Quest = 'debut' and name_course = 'Comprendre la 4G' and session = 'session01'"),aes(Age)) +
  geom_histogram(binwidth = 5, color="white", fill="green") + 
  xlab("Age") 
ggplotly(p_num)










dff[dff$Quest == input$sel_Quest & dff$name_course == input$sel_MOOC & dff$session == input$sel_Sess,]
