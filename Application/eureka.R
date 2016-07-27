kok <- c(0,2,4,8,5,3)
mean(kok)
w <- "mean"
w
ok<-call(w,kok)
eval(ok)

final <- NULL
for (i in List_Var_Tot){
  commands <- paste("'",i,"' = dff[dff$Quest == input$sel_Quest,]$",i,",", sep = "")
  final <- paste(final,commands,sep = "")
}
final <- substr(final,start = 0, stop = nchar(final)-1)
print(final)
nchar(final)

test <- "abcde,"

substr(test,start = 0, stop = nchar(test)-1)

switch(input$selvar1,
       "Age" = dff[dff$Quest == input$sel_Quest,]$Age,
       "Gender" = dff[dff$Quest == input$sel_Quest,]$gender,
       "Niveau_Depart" = dff[dff$Quest == input$sel_Quest,]$Niveau_Depart,
       "Heures_Travail_Prevues" = dff[dff$Quest == input$sel_Quest,]$Heures_Travail_Prevues,
       "Heures_Travail_Passees" = dff[dff$Quest == input$sel_Quest,]$Heures_Travail_Passees,
       "level_of_education" = dff[dff$Quest == input$sel_Quest,]$level_of_education,
       "Categ_Socio_Prof" = dff[dff$Quest == input$sel_Quest,]$Categ_Socio_Prof,
       "Taille_Ent" = dff[dff$Quest == input$sel_Quest,]$Taille_Ent,
       "Domaine_Activite" = dff[dff$Quest == input$sel_Quest,]$Domaine_Activite,
       "Connaissance_Cours" = dff[dff$Quest == input$sel_Quest,]$Connaissance_Cours,
       "Premier_Objectif" = dff[dff$Quest == input$sel_Quest,]$Premier_Objectif,
       "Recommand" = dff[dff$Quest == input$sel_Quest,]$Recommand,
       "Inscri_Proch_Sess" = dff[dff$Quest == input$sel_Quest,]$Inscri_Proch_Sess)


eval(parse(text = paste("mean(x,", myoptions, ")")))

blabla

fct <- "i +"
i = 0
eval(call(fct,2))
commands <- "mean(kok)"
eval(parse(text = commands))
a