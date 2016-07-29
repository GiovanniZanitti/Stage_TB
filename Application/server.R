# Packages utilisÃÂÃÂ©s

library(shiny)
library(leaflet)
library(magrittr)
library(plotly)
library(ggplot2)
library(leaflet)
library(sqldf)
library(reshape2)
library(gridExtra)
library(scales)

#Appel ? d'autres fichiers
source("Analyse.R")
source("jauge.R")



# DÃÂÃÂ©finition du server Shiny
shinyServer(function(input, output) {
  
  ###################### UI et reactive ############################
  
  Var_act <- reactive({
    #names(dff[dff$Quest == input$sel_Quest & dff$name_course == input$sel_MOOC,][1,-which(is.na(dff[dff$Quest == input$sel_Quest & dff$name_course == input$sel_MOOC,][1,]))])[-c(1:4)]
    ok <- sqldf(paste("Select variable from df_var where course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' and Quest = '",input$sel_Quest,"'", sep = ""))
    ok <- as.character(ok$variable)[-c(1:5)]
    ok <- ok[-c((length(ok)-2):length(ok))]
    c(ok,"Age","C_Age")
    })
  
  Var_actt <- reactive({
    #names(dff[dff$Quest == input$sel_Quest & dff$name_course == input$sel_MOOC,][1,-which(is.na(dff[dff$Quest == input$sel_Quest & dff$name_course == input$sel_MOOC,][1,]))])[-c(1:4)]
    ok <- sqldf(paste("Select variable from df_var where course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' and Quest = '",input$sel_Questt,"'", sep = ""))
    ok <- as.character(ok$variable)[-c(1:5)]
    ok <- ok[-c((length(ok)-2):length(ok))]
    c(ok,"Age","C_Age")
  })
  
  
  Session <- reactive({
    as.character(unlist(sqldf(paste("Select distinct session from dff where name_course = '",input$sel_MOOC,"'", sep = ""))))
  })
  
  Uni <- reactive({
    eval(parse(text = paste("switch(input$selVarUni,", final, ")")))
  })
  
  Biv1 <- reactive({
    eval(parse(text = paste("switch(input$selVarBiv1,", final, ")")))
  })
  
  
  
  Biv2 <- reactive({
    eval(parse(text = paste("switch(input$selVarBiv2,", final, ")")))
  })
  
  
  
  output$selec_VarUni <- renderUI ({
    selectInput("selVarUni","Variable 1",
                choices = Var_act())
  })
  
  output$selec_VarBiv1 <- renderUI ({
    selectInput("selVarBiv1","Variable 1",
                choices = Var_actt())
  })
  
  output$selec_VarBiv2 <- renderUI ({
    selectInput("selVarBiv2","Variable 2",
                choices = Var_actt()[Var_actt() != input$selVarBiv1])
  })
  
  output$selec_Session <- renderUI ({
    selectInput("sel_Sess","Session",
                choices = Session())
  })
  
  
  ############################### Informations generales ####################
  
  
  
  
  
  ####### Inscrit
  
  output$Nb_inscrit <- renderText({
    nb <- sqldf(paste("Select count(course) from inscr where name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    paste("Nombre d'inscrits : ",nb)
  })
  
  output$Nb_pie_inscrit <- renderText({
    gd <- data.frame(sqldf(paste("Select gender,count(course) as nb from inscr where name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by gender", sep = "")))
    gd <- gd[!gd$gender == "",]
    gd <- gd[!gd$gender == "Vide",]
    nb <- sum(gd$nb)
    paste(nb,"inscrits concernés")
    
  }) 
  output$pie_inscrit <- renderPlot({
    gd <- data.frame(sqldf(paste("Select gender,count(course) as nb from inscr where name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by gender", sep = "")))
    gd <- gd[!gd$gender == "",]
    gd$nb <- (gd$nb/sum(gd$nb))*100
    
    pie(gd$nb, labels = paste(gd$gender,";",round(gd$nb,2),"%"), col = c("lightsalmon2","slateblue2","lightgreen"), main = "Repartition des genres")
  })
  
 output$Nb_age_inscrit <- renderText({
   ok<-sqldf(paste("Select user,C_Age from inscr where name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
   nb <- nrow(ok[which(is.na(ok)),])
   paste(nrow(ok)-nb,"inscrits concernés")
 })

  output$age_inscrit <- renderPlot({
    
   
    
    p_age <- ggplot(data = sqldf(paste("Select * from inscr where name_course = '",input$sel_MOOC,"'", sep = "")), aes(C_Age), fill = factor(C_Age)) + #DonnÃ©es du graphique
      # Type de graphique
      geom_bar(aes(y = ((..count..)/sum(..count..))*100), color="white", fill="green") +   
      geom_text(aes(y = ((..count..)/sum(..count..))*100, label = scales::percent((..count..)/sum(..count..))), vjust = -0.25,stat = "count", color="black") +
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
    p_age
    
  })
  
  
  output$courbe_inscrit <- renderPlot({
    p <- ggplot(data=inscr[inscr$name_course == input$sel_MOOC & inscr$session == input$sel_Sess,], aes(x=Date))  + geom_line(stat = "count") +
      ggtitle("Evolution du nombre d'inscription au MOOC") +
      geom_vline(xintercept=as.numeric(as.Date(input$dates)),
                 linetype=4, colour="black")
    p
  })
  
  
  
  
  ##### Resume debut
  
  
  
  
  
  output$Nb_debut <- renderText({
    #R?cup?ration des donn?es via SQL
    nb <- sqldf(paste("Select count(DISTINCT id) from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    #Affichage
    paste("Nombre de repondant au questionnaire de debut de cours : ",nb)
  })
  
  output$Nb_debut2 <- renderText({
    nb <- sqldf(paste("Select count(course) from inscr where name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    nb2 <- sqldf(paste("Select count(DISTINCT id) from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    
    paste(round((nb2/nb)*100,2),"% de tous les inscrits")
  })
  
  output$Nb_debut3 <- renderText({
    gd <- sqldf(paste("Select gender,count(DISTINCT id) as nb from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by gender",sep = ""))
    gd <- gd[!gd$gender == "",]
    gd <- gd[!gd$gender == "Vide",]
    nb <- sum(gd$nb)
    print(paste(nb,"inscrits concernés"))
  })
  
  
  output$pie_debut <- renderPlot({
    gd <- sqldf(paste("Select gender,count(DISTINCT id) as nb from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by gender",sep = ""))
    gd <- gd[!gd$gender == "",]
    gd <- gd[!gd$gender == "Vide",]
    gd$nb <- (gd$nb/sum(gd$nb))*100
    
    pie(gd$nb, labels = paste(gd$gender,";",round(gd$nb,2),"%"), 
        col = c("lightsalmon2","slateblue2","lightgreen"), 
        main = "Repartition des genres")
  })
  
  output$Nb_age_debut <- renderText({
    ok<-sqldf(paste("Select DISTINCT id,Age from dff where name_course = '",input$sel_MOOC,"' and Quest = 'debut' and session = '",input$sel_Sess,"'",sep = ""))
    nb <- nrow(ok[which(is.na(ok)),])
    paste(nrow(ok)-nb,"inscrits concernés")
  })
  
  output$age_debut <- renderPlot({
    p_age <- ggplot(data = sqldf(paste("Select DISTINCT id,C_age from dff where name_course = '",input$sel_MOOC,"' and Quest = 'debut' and session = '",input$sel_Sess,"'", sep ="")), aes(C_Age), fill = factor(C_Age)) +
      geom_bar(aes(y = ((..count..)/sum(..count..))*100), color="white", fill="green") +    
      geom_text(aes(y = ((..count..)/sum(..count..))*100, label = scales::percent((..count..)/sum(..count..))), vjust = -0.25,stat = "count", color="black") +
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=0, vjust=1)) + 
      theme(legend.position="none") + 
      ggtitle("% des inscrit par classe d'age") +
      ylab("(%)") +
      xlab("Classe d'age")
    p_age
    
  })
  
  

  ##### Resume fin
  

  output$Nb_fin <- renderText({
    nb <- sqldf(paste("Select count(DISTINCT id) from dff where Quest = 'fin' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    paste("Nombre de repondant au questionnaire de fin de cours : ",nb)
  })
  
  output$Nb_fin2 <- renderText({
    nb <- sqldf(paste("Select count(course) from inscr where name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    nb2 <- sqldf(paste("Select count(DISTINCT id) from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    nb3 <- sqldf(paste("Select count(DISTINCT id) from dff where Quest = 'fin' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    
    paste(round((nb3/nb)*100,2),"% de tous les inscrits et",round((nb3/nb2)*100,2),"% des repondants au debut")
  })
  
  
  output$Nb_fin3 <- renderText({
    nb <- sqldf(paste("Select count(DISTINCT id) from dff where Quest = 'fin' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    paste(nb,"inscrits concernés")
  })
  
  output$pie_fin <- renderPlot({
    gd <- sqldf(paste("Select gender,count(DISTINCT id) as nb from dff where Quest = 'fin' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by gender",sep = ""))
    gd <- gd[!gd$gender == "",]
    gd <- gd[!gd$gender == "Vide",]
    gd$nb <- (gd$nb/sum(gd$nb))*100
    
    pie(gd$nb, labels = paste(gd$gender,";",round(gd$nb,2),"%"), 
        col = c("lightsalmon2","slateblue2","lightgreen"), 
        main = "Repartition des genres")
  })
  
  output$Nb_age_fin <- renderText({
    ok<-sqldf(paste("Select DISTINCT id,Age from dff where name_course = '",input$sel_MOOC,"' and Quest = 'fin' and session = '",input$sel_Sess,"'",sep = ""))
    nb <- nrow(ok[which(is.na(ok)),])
    paste(nrow(ok)-nb,"inscrits concernés")
  })
  
  output$age_fin <- renderPlot({
    
    p_age <- ggplot(data = sqldf(paste("Select DISTINCT id,C_age from dff where name_course = '",input$sel_MOOC,"' and Quest = 'fin'", sep ="")), aes(C_Age), fill = factor(C_Age)) +
      geom_bar(aes(y = ((..count..)/sum(..count..))*100), color="white", fill="green") +    
      geom_text(aes(y = ((..count..)/sum(..count..))*100, label = scales::percent((..count..)/sum(..count..))), vjust = -0.25,stat = "count", color="black") +
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=0, vjust=1)) + 
      theme(legend.position="none") + 
      ggtitle("% des inscrit par classe d'age") +
      ylab("(%)") +
      xlab("Classe d'age")
    p_age
    
  })
  
  
  
  
  ##### Evolution

  output$tab_Genres <- renderDataTable({
    
    gd <- sqldf(paste("Select gender,count(DISTINCT id) as nb from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by gender", sep = ""))
    gd <- gd[!gd$gender == "",]
    gd <- gd[!gd$gender == "Vide",]
    rownames(gd) <- gd$gender
    
    gd$nb <- (gd$nb/sum(gd$nb))*100
    
    gd2 <- sqldf(paste("Select gender,count(DISTINCT id) as nb from dff where Quest = 'fin' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by gender", sep = ""))
    gd2 <- gd2[!gd2$gender == "",]
    gd2 <- gd2[!gd2$gender == "Vide",]
    rownames(gd2) <- gd2$gender
    
    gd2$nb <- (gd2$nb/sum(gd2$nb))*100
    
    df_gend_bind <- data.frame(t(smartbind(t(gd)[2,],t(gd2)[2,])))
    
    colnames(df_gend_bind) <- c("debut","fin")
    
    res_Gend = data.frame(gender = rownames(df_gend_bind),diff = as.numeric(as.character(df_gend_bind$fin)) - as.numeric(as.character(df_gend_bind$debut)))
    
    res_Gend[which(is.na(res_Gend$diff)),]$diff <- - as.numeric(
      as.character(df_gend_bind[rownames(df_gend_bind) == res_Gend[which(is.na(res_Gend$diff)),]$gender,]$debut)
    )
    
    
    #Boucles pour afficher le r?sultat en "+/- x pts"
    vec <- NULL
    for(i in 1:nrow(res_Gend)){
      vec <- c(vec,
               if(as.numeric(res_Gend[i,]$diff) > 0){
                 paste("+",round(res_Gend[i,]$diff,2)," pts", sep ="")
               }
               else {
                 paste(round(res_Gend[i,]$diff,2)," pts", sep ="")
               }
      )
    }
    res_Gend <- cbind(as.character(res_Gend[,1]), diff = vec)
    res_Gend
    
  })
  
  output$tab_Ages <- renderDataTable({
    
    
    #Import des donn?es du questionnaire de d?but
    df_age <- sqldf(paste("Select C_age, count(DISTINCT id) as nb from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by C_age", sep = ""))
    #Transformation en pourcentage
    df_age$nb <- (df_age$nb/sum(df_age$nb))*100
    df_age <- df_age[!is.na(df_age$C_Age),]
    rownames(df_age) <- df_age$C_Age
    
    #Import des donn?es du questionnaire de fin
    df_age2 <- sqldf(paste("Select C_age, count(DISTINCT id) as nb from dff where Quest = 'fin' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"' group by C_age", sep = ""))
    df_age2$nb <- (df_age2$nb/sum(df_age2$nb))*100
    df_age2 <- df_age2[!is.na(df_age2$C_Age),]
    rownames(df_age2) <- df_age2$C_Age
    
    
    #Jointure des deux tables
    df_age_bind <- data.frame(t(smartbind(t(df_age)[2,],t(df_age2)[2,])))
    
    colnames(df_age_bind) <- c("debut","fin")
    
    res_Age = data.frame(C_Age = rownames(df_age_bind),diff = as.numeric(as.character(df_age_bind$fin)) - as.numeric(as.character(df_age_bind$debut)))
    res_Age[which(is.na(res_Age$diff)),]$diff <- - as.numeric(
      as.character(df_age_bind[rownames(df_age_bind) == res_Age[which(is.na(res_Age$diff)),]$C_Age,]$debut)
    )
    
    #Boucles pour afficher le r?sultat en "+/- x pts"
    vec <- NULL
    for(i in 1:nrow(res_Age)){
      vec <- c(vec,
               if(as.numeric(res_Age[i,]$diff) > 0){
                 paste("+",round(res_Age[i,]$diff,2)," pts", sep ="")
               }
               else {
                 paste(round(res_Age[i,]$diff,2)," pts", sep ="")
               }
      )
    }
    
    res_Age <- cbind(as.character(res_Age[,1]), diff = vec)
    res_Age
  })
  
  output$deb_fin <- renderText({
    deb <- sqldf(paste("Select DISTINCT id from dff where Quest = 'debut' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'",sep = ""))
    fin <- sqldf(paste("Select DISTINCT id from dff where Quest = 'fin' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'",sep = ""))
    ff <- sqldf("Select count(*) from deb, fin where deb.id = fin.id")
    
    paste("Nombre de repondant aux deux questionnaires :",ff)
  })
  
  
  ############################### Descriptif ################################
  
  output$text_uni <- renderText({
    paste("Distribution de ",input$selVarUni)
  })
  
  output$Nb_desc <- renderText({
    nb <- sqldf(paste("Select count(DISTINCT id) from dff where Quest = '",input$sel_Quest,"' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'", sep = ""))
    paste(nb,"personnes concernés")
  })
  
  output$plotUni <- renderPlot({
    #Si la variable choisie est quantitative
    if(is.numeric(Uni())){
      
      df <- sqldf(paste("Select DISTINCT id, ",input$selVarUni," from dff where Quest = '",input$sel_Quest,"' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'",sep = ""))
      p_num <- ggplot(data = df,aes(eval(parse(text = input$selVarUni)))) +
        geom_histogram(binwidth = 5, color="white", fill="green") + 
        xlab(input$selVarUni) 
      p_num
    }
    #Si la variable est qualitative
    else {
      df <- sqldf(paste("Select DISTINCT id, ",input$selVarUni," from dff where Quest = '",input$sel_Quest,"' and name_course = '",input$sel_MOOC,"' and session = '",input$sel_Sess,"'",sep = ""))
      
      p_char <- ggplot(data = df, aes(factor(eval(parse(text = input$selVarUni))), fill = factor(eval(parse(text = input$selVarUni))))) +
        geom_bar(aes(y = ((..count..)/sum(..count..))), color="white", fill="lightblue") +  
        geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),stat = "count", vjust = -0.25, color="black") +
        scale_y_continuous(labels = percent) +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=90)) + 
        theme(legend.position="none") + 
        xlab(input$selVarUni) +
        ylab("Pourcentage") 
      p_char
    }
  })
  
  output$text_biv1 <- renderText({
    paste("Question 1 :",Questi[Questi$code_Q == input$selVarBiv1,]$label_Q)
  })
  output$text_biv2 <- renderText({
    paste("Question 2 :",Questi[Questi$code_Q == input$selVarBiv2,]$label_Q)
  })
  
  
  output$plotBiv <- renderPlot({
    #Si la variable 1 est quanti
    if(is.numeric(Biv1())){
      #Si la variable 2 est quanti
      if(is.numeric(Biv2())){
        p <- ggplot(dff[dff$Quest == input$sel_Questt & dff$name_course == input$sel_MOOC & dff$session == input$sel_Sess,], aes(Biv1(), Biv2())) +
          xlab(input$selVarBiv1) +
          ylab(input$selVarBiv2)
        p + geom_point()
      }
      #Si la variable 2 est quali
      else {
        p_biv <- ggplot(data = dff[dff$Quest == input$sel_Questt & dff$name_course == input$sel_MOOC & dff$session == input$sel_Sess,], aes(factor(Biv2()),Biv1(), fill = factor(Biv2())))+
          geom_boxplot() + 
          theme(text = element_text(size=15),
                axis.text.x = element_text(angle=90)) + 
          theme(legend.position="none") +
          xlab(input$selVarBiv1) +
          ylab(input$selVarBiv2) 
          
          
          p_biv
      }
    }
    #Si la variable 1 est quali
    else {
      #Si la variable 2 est quanti
      if(is.numeric(Biv2())){
        p_biv <- ggplot(data = dff[dff$Quest == input$sel_Questt & dff$name_course == input$sel_MOOC & dff$session == input$sel_Sess,], aes(factor(Biv1()),Biv2(), fill = factor(Biv1())))+
          geom_boxplot() + 
          theme(text = element_text(size=15),
                axis.text.x = element_text(angle=90)) + 
          theme(legend.position="none") +
          xlab(input$selVarBiv1) +
          ylab(input$selVarBiv2)
        
        p_biv
      }
      #Si la variable 2 est quali 
      else {
        tab_cont <- round((table(Biv1(), Biv2())/nrow(dff[dff$name_course == input$sel_MOOC & dff$Quest == input$sel_Questt & dff$session == input$sel_Sess,]))*100,2)
        lig_cont <- melt(tab_cont)
        names(lig_cont) <- c("iplan","vplan","value")
        p_biv <- ggplot(lig_cont, aes(iplan, vplan)) +
          geom_point(aes(size = value), alpha=0.8, color="lightblue", show_guide=FALSE) +
          geom_text(aes(label = paste(value,"%")), color="black") +
          scale_size(range = c(1,15)) +
          theme_bw() + 
          theme(text = element_text(size=15),
                axis.text.x = element_text(angle=90)) +
          xlab(input$selVarBiv1) +
          ylab(input$selVarBiv2)
        p_biv
      }
      
      
    }
  })
  
  
  ########################## Jauge ###########################
  
  
  output$text_jauge <- renderText({
    paste("Jauges de satisfaction du MOOC",input$sel_MOOC,input$sel_Sess)
  })
  
  output$JJauge <- renderPlot({
    
    
    Var_Satis <-Satis[Satis$course == input$sel_MOOC,-which(is.na(Satis[Satis$course == input$sel_MOOC,]))]
    
    jauge <- NULL
    for (i in Var_Satis[-1]){
      commands <- paste("gg_Gauge(round(mean(as.numeric(dff[dff$Quest == 'fin' & dff$name_course == input$sel_MOOC,]$",levels(i),"))*20), '",levels(i),"'),", sep = "")
      jauge <- paste(jauge,commands,sep = "")
    }
    jauge <- substr(jauge,start = 0, stop = nchar(jauge)-1)
    
    eval(parse(text = paste("grid.draw(arrangeGrob(",jauge,",ncol = 3))")))
    
    
  })
  
  
  ########################## Map #############################
  
  
  output$text_map <- renderText({
    paste("Provenance des inscrits au MOOC",input$sel_MOOC,input$sel_Sess)
  })
  
  output$text_map2 <- renderText({
    paste(nrow(df_map[df_map$course == input$sel_MOOC & df_map$session == input$sel_Sess,]),"inscrits ayant donnés une réponse")
  })
  
  output$MapPos <- renderLeaflet({
    map <- leaflet() %>%
      #Ajout de la carte
      addTiles() %>%
      #Ajout des marqueurs cercles
      addCircleMarkers(
        data = df_map[df_map$course == input$sel_MOOC & df_map$session == input$sel_Sess,],
        clusterOptions = markerClusterOptions() #Option permettant de rendre la carte dynamique au zoom
      )
    #Affichage de la carte
    map
  })
  
  ######################### Tous les mooc ####################
  
  ####Pas utilis? pour l'instant
  
  
  output$Nb_tt <- renderText({
    nb <- sqldf(paste("Select count(user) from nb_mooc_p where nb_mooc_p.nb >=",input$range, sep = ""))
    paste("Nombre de personne inscri a plus de",input$range,"cours : ",nb,".\n Attention, les personnes n'ayant pas renseignÃ© leur genre sont pris en compte ici (pas dans le 'camembert')")
  })
  
  output$pie_gend_tt <- renderPlot({
    gd <- sqldf(paste("Select gender,count(user) as nb from nb_mooc_p where nb_mooc_p.nb >=",input$range,"group by gender"))
    gd <- gd[!gd$gender == "",]
    gd <- gd[!gd$gender == "Vide",]
    gd$nb <- (gd$nb/sum(gd$nb))*100
    
    
    pie(gd$nb, labels = paste(gd$gender,";",round(gd$nb,2),"%"), 
        col = c("lightsalmon2","slateblue2","lightgreen"), 
        main = "Repartition des genres")
    
  })
  
  output$p_age_tt <- renderPlotly({
    p_age <- ggplot(data = nb_mooc_p[nb_mooc_p$nb >= input$range,], aes(C_Age), fill = factor(C_Age)) +
      geom_bar(aes(y = ((..count..)/sum(..count..))*100), color="white", fill="green") +
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle=0, vjust=1)) + 
      theme(legend.position="none") + 
      ggtitle("% des inscrit par classe d'age") +
      ylab("(%)") +
      xlab("Classe d'age")
    ggplotly(p_age)
  })
  
  output$text_top10 <- renderText({
    paste("Top 10 des pays reprÃ©sentÃ©s dans le MOOC : ",input$sel_MOOC)
  })
  
  output$map_top10 <- renderLeaflet({
    df_map2 <- data.frame(t(tab_nb_cours_country[tab_nb_cours_country$course == input$sel_MOOC,-1]))
    colnames(df_map2) = "nb"
    df_map2<-cbind(country = rownames(df_map2),df_map2)
    df_map2$nb = as.numeric(as.character(df_map2$nb))
    df_map2 <- data.frame(df_map2[order(df_map2$nb,decreasing=TRUE),])
    colnames(df_map2) = c("country","nb")
    
    df_map2$country <- as.character(df_map2$country)
    df_map2$nb <- as.numeric(as.character(df_map2$nb))
    df_map2 <- df_map2[!df_map2$country == "France",]
    df_map2 <- df_map2[!df_map2$country == "Vide",]
    df_map2 = df_map2[1:10,]
    
    
    
    
    df_map2 <- sqldf("select df_map2.*,c.lat, C.lon from df_map2, coord_country c where df_map2.country = c.country")
    
    df_map2 = df_map2[!is.na(df_map2$lat),]
    
    
    pal <- colorFactor(sort(heat.colors(10),decreasing = TRUE),
                       #Par rapport aux valeurs de nb
                       domain = df_map2[1:10,]$nb)
    
    
    
    #dataset
    df_map2 <- sp::SpatialPointsDataFrame(
      cbind(
        df_map2$lon,
        df_map2$lat
      ),
      data.frame(country = df_map2$country, nb = df_map2$nb)
    )
    
    map2 <- leaflet() %>%
      addTiles() %>%
      addCircles(
        data = df_map2,
        color = ~pal(nb),
        fillOpacity = 0.7,
        stroke = FALSE,
        radius = 500000,
        popup = paste(df_map2$country,":",df_map2$nb)
      )
    #Affichage de la carte
    map2
  })
  
  
})


