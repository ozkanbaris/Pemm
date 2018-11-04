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

