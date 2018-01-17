ceroO_E = 0
ceroT_S = 0
contOE = as.numeric(as.character(df$Original_Estimate))
contTS = as.numeric(as.character(df$Time_Spent))
for(i in 1:769){
  if(contOE[i] == 0){
    ceroO_E = ceroO_E + 1 
  }
  if(contTS[i] == 0){
    ceroT_S = ceroT_S + 1 
  }
}
ceroO_E
ceroT_S

summary(df$Issue_Type)
summary(df$Sprint)
summary(df$Component_s)
summary(df$Assignee)
summary(df$Priority)
summary(df$Epic_Link)