library(tidyverse)
library(ggalluvial)
library(reshape2)
library(gridExtra)
library(grid)

# flowCP <- function(df){
#   df$prev <- paste("L", df$prev, sep="") 
#   df$postv <- paste("L", df$postv, "P",sep="") 
#   
#   posx<-c( rep(1,length(unique(df$prev))),rep(2,length(unique(df$postv))))
#   
#   nodes<-  data.frame( ID= c(sort(unique(df$prev)),sort(unique(df$postv))),
#                        x= posx ,
#                        stringsAsFactors= FALSE )
#   
#   edges <- data.frame(N1 = df$prev, N2 = df$postv, Value = df$freq)
#   # 
#   # nodes <- data.frame( ID= LETTERS[1:3],
#   #                      x= c( 1, 1, 2 ),
#   #                      col= c( "yellow", NA, NA ),
#   #                      labels= c( "Node A", "Node B", "Node C" ),
#   #                      stringsAsFactors= FALSE )
#   
#   makeRiver( nodes, edges)
#   


# }

postpem <- read_csv('PEMMPST.csv')
prepem  <- read_csv('PEMMPR.csv') 

melted_post<-melt(postpem, id.vars="Persoon", value.name = "postv")  %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                                 comp=str_replace(str_sub(variable,2,10), plev ,"") )  
melted_pre<-melt(prepem, id.vars="Persoon", value.name = "prev")   %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                             comp=str_replace(str_sub(variable,2,10), plev ,"") )
assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = 'Persoon'))  %>% select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)

allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) %>% filter(!is.na(prev) | !is.na(postv) ) %>% summarise(freq=n())

allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest()



ggplot(assdata %>% filter(enabler=="D", comp=="P"),  aes(y = 1, axis1 = prev, axis2 = postv)) +
  geom_alluvium(aes(fill = Persoon), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")



data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")


