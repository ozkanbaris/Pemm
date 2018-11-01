library(tidyverse)
library(reshape2)

library(gridExtra)
library(grid)
library(ggplotify)

library(riverplot )
library(ggalluvial)
library(alluvial)


flowCP <- function(df){
  df$prev <- paste("L", df$prev, sep="") 
  df$postv <- paste("L", df$postv, "P",sep="") 
  
  posx<-c( rep(1,length(unique(df$prev))),rep(2,length(unique(df$postv))))
  
  nodes<-  data.frame( ID= c(sort(unique(df$prev)),sort(unique(df$postv))),
                       x= posx ,
                       stringsAsFactors= FALSE )
  
  edges <- data.frame(N1 = df$prev, N2 = df$postv, Value = df$freq)
  as.grob(expression(plot(makeRiver(nodes, edges))))
  
  # 
  # nodes <- data.frame( ID= LETTERS[1:3],
  #                      x= c( 1, 1, 2 ),
  #                      col= c( "yellow", NA, NA ),
  #                      labels= c( "Node A", "Node B", "Node C" ),
  #                      stringsAsFactors= FALSE )
}


postpem <- read_csv('PEMMPST.csv')
prepem  <- read_csv('PEMMPR.csv') 
melted_post<-melt(postpem, id.vars="Persoon", value.name = "postv")  %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                                comp=str_replace(str_sub(variable,2,10), plev ,"") )  
melted_pre<-melt(prepem, id.vars="Persoon", value.name = "prev")   %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                             comp=str_replace(str_sub(variable,2,10), plev ,"") )
assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = 'Persoon'))  %>% select(-variable.x, -variable.y)  %>% mutate(vdiff=postv-prev) 
allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) %>% filter(!is.na(prev) | !is.na(postv) ) %>% summarise(freq=n())
allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest()

aluvCP <-function(df){
  df2<- df
  as.grob(~alluvial(
    select(df2, prev, postv),
    freq=df2$freq,
    col = ifelse(df2$vdiff >= 0, "orange", "grey"),
    border = ifelse(df2$vdiff >= 0, "orange", "grey"),
    layer = df2$vdiff < 0,
    alpha = 0.8,
    blocks=FALSE))
  
}
allflowsPP <- allflowsPP%>% mutate(gmodel = map(data, aluvCP))


df11<- allflowsPP$data[[1]] 
df22<- allflowsPP$data[[2]] 

g1<-aluvCP(df11)
g2<-aluvCP(df22)

# 
# g1<-flowCP(df1)
# g2<-flowCP(df2)

grid.arrange(g1,g2)
