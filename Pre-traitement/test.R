library(stringr)
library(sqldf)
library(gtools)
path = "C:/Users/gzanitti/Documents/Enquete/Pre-traitement/data/Comprendre_4G/MinesTelecom_04010S02_session02__2016-07-21-084756"

df <- read.csv(paste(path,".csv", sep = ""), header = T)
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
  dff <- smartbind(dff,df_temp)
}

unlist(str_split(df[13,]$q1, ","))[1]

var_df <- names(df[,grep("q",colnames(df))])

df_clear <- df[,1:5]

df_clear <- sqldf("Select df_clear.*, n_df.choice as q1 from df_clear, n_df where id = user")



n_df = data.frame(user = NA,choice = NA)
for(i in 1:nrow(df)){
  choice = unlist(str_split(df[i,6], ","))
  for(j in 1:length(choice)){
    n_df = rbind(n_df,c(as.character(df[i,]$id),choice[j]))
  }
}

test <- sqldf("select * from inscr where course = 'MinesTelecom/04010/session01'")


sqldf("Select dff.*, rep.Connaissance_Cours from dff, rep where dff.q1 = rep.Choice")

deb_clear <- sqldf(paste("Select deb.*, rep_deb.",var_rep_deb[1]," from deb, rep_deb where deb.",var_deb[1]," = rep_deb.Choice", sep = ""))
df_clear <- sqldf(paste("Select dff.*, rep.",var_rep[1]," from dff, rep where dff.",var_df[1]," = rep.Choice", sep = ""))


sub(".*/data", "", path)
