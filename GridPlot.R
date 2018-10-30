library(tidyverse)
library(reshape2)

postpem <- read_csv('PEMMPST.csv')
prepem  <- read_csv('PEMMPR.csv') 
melted_post<-melt(postpem, id.vars="Persoon", value.name = "postv")  %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                               comp=str_replace(str_sub(variable,2,10), plev ,"") )  
melted_pre<-melt(prepem, id.vars="Persoon", value.name = "prev")   %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                               comp=str_replace(str_sub(variable,2,10), plev ,"") )
assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = 'Persoon'))  %>% select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)
# prepare data

enablers_Plev<- assdata %>% select(-Persoon) %>% 
  group_by(phase,enabler, comp, plev) %>% summarise(modv=getmode(value)) 
# distinct(enablers_Plev, enabler,comp)
nestByEnablerComp <- assdata  %>% select(-Persoon,-variable) %>% nest(-enabler,-comp)

ggplot(nestByEnablerComp$data[[1]], aes(x=plev, y=value, fill=phase))+  geom_violin()
ggplot(nestByEnablerComp$data[[1]], aes(x=plev, y=value, fill=phase)) + geom_bar(stat="identity")

# enablers_PlevNested<-enablers_Plev %>% nest(-enabler)

 
# enablers_Plev<- enablers_Plev %>% mutate(plev2=paste(enabler,comp,"_",plev))

# ggplot(enablers_Plev, aes(x=interaction(enabler,comp, plev), y=modv, colour = phase, size=phase)) +
#  geom_line()+
#   scale_color_manual(values=c("red","blue")) +
#   scale_size_manual(values=c(2,1))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
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