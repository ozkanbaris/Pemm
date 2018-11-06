library(tidyverse)
library(reshape2)

library(gridExtra)
library(grid)
library(ggplotify)

library(riverplot )
library(ggalluvial)
library(alluvial)
source("LoadPemm.R") 


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
flowCP <- function(df){
  df$prev <- paste("L", df$prev, sep="") 
  df$postv <- paste("L", df$postv, "P",sep="") 
  
  posx<-c( rep(1,length(unique(df$prev))),rep(2,length(unique(df$postv))))
  
  nodes<-  data.frame( ID= c(sort(unique(df$prev)),sort(unique(df$postv))),
                       x= posx ,
                       stringsAsFactors= FALSE )
  
  edges <- data.frame(N1 = df$prev, N2 = df$postv, Value = df$freq)
  as.grob(expression(plot(makeRiver(nodes, edges))))
 
 
}

assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = 'Persoon'))  %>% select(-variable.x, -variable.y)  %>% mutate(vdiff=postv-prev) 
allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) %>% filter(!is.na(prev) | !is.na(postv) ) %>% summarise(freq=n())
allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest()
allflowsPP <- allflowsPP%>% mutate(gmodel = map(data, aluvCP))

grid.arrange(g1,g2)




