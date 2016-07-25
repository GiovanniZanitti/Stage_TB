library(ggplot2)
library(gridExtra)
library('extrafont')
library(grid)

source("Analyse.R")

gg_Gauge <- function(pos, mytitle) {
  # Définition des intervalles critiques
  breaks=c(0,25,50,75,100)
  # Initialisation des valeurs des polygones critiques
  get_poly <- function(a,b,r1=0.5,r2=1.0) {
    th_start <- pi*(1-a/100)
    th_end   <- pi*(1-b/100)
    th       <- seq(th_start,th_end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  
ggplot()+ 
    # Ajout des polygones au panel graphique
    geom_polygon(data=get_poly(breaks[1],breaks[2]),aes(x,y),fill="tomato2")+
    geom_polygon(data=get_poly(breaks[2],breaks[3]),aes(x,y),fill="orange")+
    geom_polygon(data=get_poly(breaks[3],breaks[4]),aes(x,y),fill="khaki1")+
    geom_polygon(data=get_poly(breaks[4],breaks[5]),aes(x,y),fill="forestgreen")+
    # Création du curseur
    geom_polygon(data=get_poly(pos-1,pos+1,0.2),aes(x,y))+
    # Affichage de la valeur de la zone critique
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    # Ajout de la valeur centrale
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    # Ajout du titre entré en paramètre
    labs(title=paste(mytitle,"\n",sep =""))+
  
    # Suppression de tous les éléments créés par défaut (axes, ticks, cadrillage, bords)
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank(),
          plot.title = element_text(vjust=5),
          # Définition du style du titre
          text = element_text(size=11, color="blue", face="bold")) 
}

#gg_Gauge(round(mean(as.numeric(dff[dff$Quest == "fin" & dff$course == "MinesTelecom/04013/session01",]$Satis_Supports_Ecrits))*20), "Satis_TP")


# grid.newpage()
# eval(parse(text = paste("grid.draw(arrangeGrob(",jauge,",ncol = 2")))
# 
# 
# grid.arrange(gg_Gauge(round(mean(as.numeric(dff[dff$Quest == "fin" & dff$course == "MinesTelecom/04013/session01",]$Satis_Videos))*20), "Satis_TD"),
#                      gg_Gauge(round(mean(as.numeric(dff[dff$Quest == "fin" & dff$course == "MinesTelecom/04013/session01",]$Satis_Supports_Ecrits))*20), "Satis_TP"),
#                       ncol =2)
# ok <- "gg_Gauge(round(mean(as.numeric(dff[dff$Quest == 'fin' & dff$course == 'MinesTelecom/04013/session01',]$Satis_Videos))*20), 'Satis_TD'),
#                      gg_Gauge(round(mean(as.numeric(dff[dff$Quest == 'fin' & dff$course == 'MinesTelecom/04013/session01',]$Satis_Supports_Ecrits))*20), 'Satis_TP')"
# 
# eval(parse(text = paste("grid.arrange(",ok,", ncol = 2)", sep = "")))
# 
# gg_Gauge(round(mean(as.numeric(dff[dff$Quest == "fin",]$Satis_TD))*20), "Satis_TD")
# 
# essai <- ok[,grep("Satis",substr(colnames(ok),0,5))]
# 
# jauge <- NULL
# for (i in List_Var_Tot){
#   commands <- paste("gg_Gauge(round(mean(as.numeric(dff[dff$Quest == 'fin' & dff$course == input$sel_MOOC,]$",i,"))*20), '",i,"'),", sep = "")
#   jauge <- paste(jauge,commands,sep = "")
# }
# jauge <- substr(jauge,start = 0, stop = nchar(jauge)-1)
# 
# 
# ok <- dff[1,-which(is.na(dff[dff$course == "MinesTelecom/04013/session01" & dff$Quest == 'fin',][1,dff$course == "MinesTelecom/04013/session01" & dff$Quest == 'fin']))]
# ok <- dff[dff$course == "MinesTelecom/04013/session01" & dff$Quest == 'fin',-which(is.na(dff[dff$course == "MinesTelecom/04013/session01" & dff$Quest == 'fin',][1,]))]
# colnames(ok)
