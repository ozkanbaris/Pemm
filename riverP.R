library(tidyverse)
library(reshape2)

library(gridExtra)
library(grid)
library(ggplotify)

library(riverplot )


nodeCol<-function (x){
  ifelse(grepl("0",x,  fixed = TRUE),"grey",
         ifelse(grepl("1", x, fixed = TRUE),"red", 
                ifelse(grepl("2",x, fixed = TRUE),"yellow",   
                     ifelse(grepl( "3",x,fixed = TRUE),"green", "grey"))))
}

flowCP <- function(df22){
  df22$prev <- paste("L", df22$prev, sep="") 
  df22$postv <- paste("L", df22$postv, "P",sep="") 
  lccol<-as.vector(lapply(sort(unique(df22$prev)), nodeCol ))
  rccol<-as.vector(lapply(sort(unique(df22$postv)), nodeCol ))
  posx<-c( rep(1,length(unique(df22$prev))),rep(2,length(unique(df22$postv))))
  IDs<-c(sort(unique(df22$prev)),sort(unique(df22$postv)))
  nodes<-  data.frame( ID= IDs,
                       x= posx ,
                       col= unlist(append(lccol,rccol)),
                       stringsAsFactors= FALSE )
  
  edges <- data.frame(N1 = df22$prev, N2 = df22$postv, Value = df22$freq)
  r<-makeRiver(nodes, edges)
  
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

allflowsPP <- allflowsPP%>% mutate(gmodel = map(data, flowCP))


cells<-assdata  %>% group_by(enabler,comp,plev) %>% nest() %>% select (-data)

levs<- as_vector(distinct(cells,plev))
enablers<-  as_vector(distinct(cells,enabler))

# header
grobs<-list(
  grobTree(rectGrob(gp=gpar(fill=1, alpha=0.5)), textGrob("enablers")),
   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5)), textGrob("Levels/")),
    grobTree(rectGrob(gp=gpar(fill=4, alpha=0.5)), textGrob(levs[1])),
     grobTree(rectGrob(gp=gpar(fill=5, alpha=0.5)), textGrob(levs[2])),
      grobTree(rectGrob(gp=gpar(fill=6, alpha=0.5)), textGrob(levs[3])),
        grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(levs[4])))



# create a custom style by first copying from the default style
custom.style <- riverplot::default.style()

# change the font size
custom.style$textcex <- 0.8

for (en in enablers) {

  grobs<-c(grobs,list(grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(en))))
  ecomps<-as_vector(filter(cells, enabler==en) %>% distinct(comp))
  
  # 
 for (enc in ecomps) {
   grobs<-c(grobs,  list(grobTree(rectGrob(gp=gpar(fill=1, alpha=0.5)), textGrob(enc))))
   
   encpleve<-as_vector(filter(cells, enabler==en, comp==enc) %>% select(plev))
   for (encp in encpleve) {
     
     pm<-select(filter(allflowsPP, enabler==en,  comp==enc,plev==encp),gmodel)
     # plot(pm$gmodel[[1]])
     grobs<-c(grobs,list(as.grob(~plot(pm$gmodel[[1]], plot_area=1.3, xscale=1.1,  default_style=custom.style,
                                                           node_margin = 0.1, nodewidth= 1.5, mar=c(0,0,0,0)))))
   }
   
 }
  
}



# 
# 

#                 
# 

# 
# df22<- allflowsPP$data[[2]] 
# nodeCol<-function (x){
#   ifelse(grepl("0",x,  fixed = TRUE),"grey",
#          ifelse(grepl("1", x, fixed = TRUE),"red", 
#                 ifelse(grepl("2",x, fixed = TRUE),"yellow",   
#                        ifelse(grepl( "3",x,fixed = TRUE),"green", "grey"))))
#   
#   
# }
# df22$prev <- paste("L", df22$prev, sep="") 
# df22$postv <- paste("L", df22$postv, "P",sep="") 
# 
# lccol<-as.vector(lapply(sort(unique(df22$prev)), nodeCol ))
# rccol<-as.vector(lapply(sort(unique(df22$postv)), nodeCol ))
# 
# posx<-c( rep(1,length(unique(df22$prev))),rep(2,length(unique(df22$postv))))
# 
# IDs<-c(sort(unique(df22$prev)),sort(unique(df22$postv)))
# 
# 
# nodes<-  data.frame( ID= IDs,
#                      x= posx ,
#                      col= unlist(append(lccol,rccol)),
#                      stringsAsFactors= FALSE )
# 
# edges <- data.frame(N1 = df22$prev, N2 = df22$postv, Value = df22$freq)
# 
# plot(makeRiver(nodes, edges) ,plot_area=1.3, xscale=1.1,  
#      node_margin = 0.1, nodewidth= 1.5, mar=c(0,0,0,0), asp=.8)
# 
# g2<- as.grob(~plot(makeRiver(nodes, edges) ,plot_area=1.3, xscale=1.1,  
#                    node_margin = 0.1, nodewidth= 1.5, mar=c(0,0,0,0)))
# 

# compsGB1<-lapply(1:76, function(ii) g2)
# 
# 
# compsGB1<-lapply(1:76, function (i) rectGrob())
widths=c()
lay <- rbind(
  c(1,2,2,3,4,5,6),
  c(1,7,c(8:12)),
  c(1,7,c(13:17)),
  c(1,7,c(18:22)),
  c(1,23,c(24:28)),
  c(1,23,c(29:33)),
  c(1,23,c(34:38)),
  c(1,39,c(40:44)),
  c(1,39,c(45:49)),
  c(1,39,c(50:54)),
  c(1,55,c(56:60)),
  c(1,55,c(61:65)),
  c(1,66,c(67:71)),
  c(1,66,c(72:76)))
# 
 grid.arrange(grobs= grobs, layout_matrix = lay)

# 
# 
# 
# 
# 
