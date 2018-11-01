library(riverplot )
library(reshape2)
library(tidyverse)
library(gridExtra)
library(grid)

flowCP <- function(df){
df$prev <- paste("L", df$prev, sep="") 
df$postv <- paste("L", df$postv, "P",sep="") 

posx<-c( rep(1,length(unique(df$prev))),rep(2,length(unique(df$postv))))
  
  nodes<-  data.frame( ID= c(sort(unique(df$prev)),sort(unique(df$postv))),
                       x= posx ,
                       stringsAsFactors= FALSE )
  
  edges <- data.frame(N1 = df$prev, N2 = df$postv, Value = df$freq)
  makeRiver(nodes, edges)
  
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

assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = 'Persoon'))  %>% select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)

allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) %>% filter(!is.na(prev) | !is.na(postv) ) %>% summarise(freq=n())

allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest()

allflowsPP <- allflowsPP%>%
  mutate(gmodel = map(data, flowCP))


cells<-assdata  %>% group_by(enabler,comp,plev) %>% nest() %>% select (-data)
levs<- as_data_frame(distinct(cells,plev))
enablers<- as_data_frame(distinct(cells,enabler))
comps<-as_data_frame(distinct(cells,comp))


grobs<-list(grobTree(rectGrob(gp=gpar(fill=1, alpha=0.5)), textGrob("enablers")),
  grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5)), textGrob("Levels")), 
   grobTree(rectGrob(gp=gpar(fill=4, alpha=0.5)), textGrob(levs[[1,1]])),
   grobTree(rectGrob(gp=gpar(fill=5, alpha=0.5)), textGrob(levs[[2,1]])), 
      grobTree(rectGrob(gp=gpar(fill=6, alpha=0.5)), textGrob(levs[[3,1]])),
        grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(levs[[4,1]])),
           grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(enablers[1,1])),
             grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob("D")))
                

compsGB1<-lapply(1:4, function(ii) allflowsPP$gmodel[[ii]])

grobs<-append(grobs,compsGB1)


compsGB2<-lapply(1:4, function(ii) allflowsPP$gmodel[[4+ii]])


grobs<- grobs  %>% append(compsGB2)




# headingGS <- lapply(1:2, function(ii) 
#   grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
# 
# for (e in enablers) {
#   enablerGS<- lapply(1:5, function(ii) 
#   grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(levs[ii])))
#  
# }
# 
# for (c in comps) {
#   compGS <- grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(levs[ii])))
#   compGS <- lapply(1:4, function(ii) 
#   grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(levs[ii])))
# }

lay <- rbind(
  c(1,2,2,3,4,5,6),
  c(1,7,8,9,10,11, 12),
  c(1,7,13,14,15,16,17)
  )

grid.arrange(grobs = grobs, layout_matrix = lay)





