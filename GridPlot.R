library(tidyverse)
library(reshape2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

postpem <- read_csv('PEMMPST.csv')
prepem  <- read_csv('PEMMPR.csv') 

melted_post<-melt(postpem, id.vars="Persoon")  %>% mutate(phase="postpem", enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                       comp=str_replace(str_sub(variable,2,10), plev ,"") )
melted_pre<-melt(prepem, id.vars="Persoon")   %>% mutate(phase="prepem", enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                     comp=str_replace(str_sub(variable,2,10), plev ,"") )

# aTYpes <- c("postpem", "prepem")
# enabler<- c(rep(c("D"),12),rep(c("P"),12),rep(c("O"),12),rep(c("I"),8),rep(c("M"),8))

assdata<-bind_rows(melted_pre,melted_post) 

enablers_Plev<- assdata %>% select(-Persoon) %>% 
  group_by(phase,enabler, comp, plev) %>% summarise(modv=getmode(value)) 
# distinct(enablers_Plev, enabler,comp)


enablers_PlevNested<-enablers_Plev %>% nest(-enabler)

 
# enablers_Plev<- enablers_Plev %>% mutate(plev2=paste(enabler,comp,"_",plev))

ggplot(enablers_Plev, aes(x=interaction(enabler,comp, plev), y=modv, colour = phase, size=phase)) +
 geom_line()+
  scale_color_manual(values=c("red","blue")) +
  scale_size_manual(values=c(2,1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
   # facet_grid(. ~ enabler )

# .....
# enablers_Clev<- enablers_Plev  %>% group_by(phase,enabler, comp) %>% 
#   summarise( score=sum(modv))
# # 
# ggplot(enablers_Clev, aes(x=comp, y=score, colour = phase, size=phase)) +
#   geom_point() +
#   scale_color_manual(values=c("red","blue")) +
#   scale_size_manual(values=c(5,4))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))




#postFa<- postpem %>% summarise_all(funs(min(., na.rm = TRUE)))
#pretFa<- prepem %>% summarise_all(funs(min(., na.rm = TRUE)))