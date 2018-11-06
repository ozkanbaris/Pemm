library(tidyverse)
library(reshape2)

library(gridExtra)
library(grid)
library(ggplotify)

library(riverplot )
library(ggalluvial)
library(alluvial)

source("LoadPemm.R") 


nodeCol<-function (x){
  ifelse(grepl("0",x,  fixed = TRUE),"grey",
         ifelse(grepl("1", x, fixed = TRUE),"red", 
                ifelse(grepl("2",x, fixed = TRUE),"yellow",   
                       ifelse(grepl( "3",x,fixed = TRUE),"green", "grey"))))
}
# 
# galluvCP <- function(df11) {
#   df11$wa=ifelse(df11$vdiff > 0, "green", "gray")
#   g1a<-as.grob(ggplot(df11,
#               aes(y = freq, axis1 = prev, axis2 = postv)) +
#     geom_alluvium(aes(fill = wa ), width = 1/12) +
#     geom_stratum(width = 1/12,  color = "grey") +
#     geom_label(stat = "stratum", label.strata = TRUE) +
#     scale_x_discrete(limits = c("Pre", "Post"), expand = c(.05, .05)) +
#     scale_fill_brewer(type = "qual", palette = "Set1") +
#     ggtitle("Change Maturity"))
# }
# 
# aluvCP <-function(df){
#   df11<- df%>% mutate(vdiff=postv-prev)
#  
#    as.grob(expression(alluvial(
#     select(df11, prev, postv),
#     freq=df11$freq,
#     col = ifelse(df11$vdiff >= 0, ifelse(df11$vdiff > 0, "green", "gray"), "red"),
#     border = ifelse(df11$vdiff == 0, "green", "red"),
#     layer = df11$vdiff < 0,
#     alpha = 0.8,
#     blocks=FALSE)))
#   
# }
# flowCP <- function(df22){
#   # df22$prev <- paste("L", df22$prev, sep="") 
#   # df22$postv <- paste("L", df22$postv, "P",sep="") 
#   lccol<-as.vector(lapply(sort(unique(df22$prev)), nodeCol ))
#   rccol<-as.vector(lapply(sort(unique(df22$postv)), nodeCol ))
#   posx<-c( rep(1,length(unique(df22$prev))),rep(2,length(unique(df22$postv))))
#   IDs<-c(sort(unique(df22$prev)),sort(unique(df22$postv)))
#   nodes<-  data.frame( ID= IDs,
#                        x= posx ,
#                        col= unlist(append(lccol,rccol)),
#                        stringsAsFactors= FALSE )
#   
#   edges <- data.frame(N1 = df22$prev, N2 = df22$postv, Value = df22$freq)
#   r<-makeRiver(nodes, edges)
#   
# }

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






assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = 'Persoon'))  %>% select(-variable.x, -variable.y)   
allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) %>% filter(!is.na(prev) | !is.na(postv) ) %>% summarise(freq=n())
allflowsPP <- allflows %>% mutate(vdiff=postv-prev) %>% group_by(enabler,comp,plev) %>% nest()

# aluvCP <-function(df2){
#   as.grob(function()alluvial(
#     select(df2, prev, postv),
#     freq=df2$freq,
#     col = ifelse(df2$vdiff >= 0, "orange", "grey"),
#     border = ifelse(df2$vdiff >= 0, "orange", "grey"),
#     layer = df2$vdiff < 0,
#     alpha = 0.8,
#     blocks=FALSE))
#   
# }


# allflowsPP <- allflowsPP%>% mutate(gmodel = map(data, aluvCP))


df11<- allflowsPP$data[[1]] 
df22<- allflowsPP$data[[2]] 

# 
# g1<-as.grob(~alluvial(
#   select(df11, prev, postv),
#   freq=df11$freq,
#   col = ifelse(df11$vdiff >= 0, ifelse(df11$vdiff > 0, "green", "gray"), "red"),
#   border = ifelse(df11$vdiff == 0, "green", "red"),
#   layer = df11$vdiff < 0,
#   alpha = 0.8,
#   blocks=FALSE))
# 
# df11$wa=ifelse(df11$vdiff > 0, "green", "gray")
# g1a<-as.grob(ggplot(df11,
#             aes(y = freq, axis1 = prev, axis2 = postv)) +
#   geom_alluvium(aes(fill = wa ), width = 1/12) +
#   geom_stratum(width = 1/12,  color = "grey") +
#   geom_label(stat = "stratum", label.strata = TRUE) +
#   scale_x_discrete(limits = c("Pre", "Post"), expand = c(.05, .05)) +
#   scale_fill_brewer(type = "qual", palette = "Set1") +
#   ggtitle("Change Maturity"))
# 
# grid.arrange(g1a,rectGrob(),g1a,rectGrob())
nodeCol<-function (x){
  ifelse(grepl("0",x,  fixed = TRUE),"grey",
         ifelse(grepl("1", x, fixed = TRUE),"red", 
                             ifelse(grepl("2",x, fixed = TRUE),"yellow",   
                                              ifelse(grepl( "3",x,fixed = TRUE),"green", "grey"))))
  

}

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

plot(makeRiver(nodes, edges) ,plot_area=1.3, xscale=1.1,  
     node_margin = 0.1, nodewidth= 1.5, mar=c(0,0,0,0), asp=.8)

g2<- as.grob(~plot(makeRiver(nodes, edges) ,plot_area=1.3, xscale=1.1,  
                     node_margin = 0.1, nodewidth= 1.5, mar=c(0,0,0,0)))

g3<-grobTree(rectGrob(gp=gpar(fill=1, alpha=0.5)), g2)
 
grid.arrange(g3)




# 
# g1<-flowCP(df1)
# g2<-flowCP(df2)

# grid.arrange(g1,g1,g1,rectGrob())
