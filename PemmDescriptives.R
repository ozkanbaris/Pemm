library(ggplot2)
source("LoadPemmData.R")
library("VIM")
library("dplyr")
library("ggplot2")
library("gridExtra")

eNames<-c("Design","Performer","Owner","Infrastructure","Metrics")
cNames<-c( "Purpose", "Context", "Documentation","Knowledge", "Skills", "Behavior", "Identity", "Activities", 
           "Authority","IS", "HRS","Definition", "Uses")
ccodes<- c("PU","CO","DO","KN","SK","BR","ID","AC","AU","IS","HR","DE","US")

lookupC<-c(
      PU = "Purpose",
       CO = "Context",
       DO = "Documentation",
       KN = "Knowledge",
       SK = "Skills",
       BR = "Behavior",
       ID = "Identity",
       AC = "Activities",
       AU = "Authority",
       IS = "IS",
       HR = "HRS",
       DE = "Definition",
       US = "Uses"
)

allflowsGR<-allflows %>% group_by(prev,postv) %>% summarise(freq=n())

# pointPlot at P levels
pointPlotP<-ggplot( allflowsGR, aes(prev, postv, size=freq)) +
  geom_point(alpha=0.3, shape=21, stroke = 1.5 )+
  scale_size(range = c(1, 20))+
  geom_abline(intercept = 0, slope = 1, colour="blue") 
  
pointPlotP


mv<- allflowsPP %>%  select(comp,plev,medpre, medpost) %>% unnest() 
mv<-melt(mv) %>% rename(Rating=value)
labelM<-c(
  prev = "Distribution of Zero Ratings",
  postv = "Post-implementation Maturity Level Map"
  
)





library(reshape)
allflowszero <- reshape2::melt(allflows, measure.vars= c("postv","prev")) %>% filter(value==0)

allflowsm<- allflowszero%>% mutate(comp = factor(comp,levels = c("AU","DE","HR","AC","US","DO","IS","ID","KN","CO","PU","BR","SK")))

allflowssmp<- allflowsm %>%group_by(enabler,comp,plev) %>% summarise(freq=n()/2)

p<- ggplot(allflowssmp,aes(x=comp,y=plev,fill=freq))+labs(y= 'Process Condition', x='PEMM Components')
p+geom_tile(colour = "blue")+scale_fill_continuous(low = "#f0f0f0", high = "#636363")
  # facet_wrap(~variable, labeller =labeller(variable=labelM, .multi_line = FALSE))

allflowsm <- reshape2::melt(allflows, measure.vars= c("postv","prev"))%>%
  filter(value==0)%>% group_by(Persoon,enabler,comp) %>% summarise(freq=n())
 

allflowsm <- reshape2::melt(allflows, measure.vars= c("postv","prev")) %>%
  filter(value==0) %>%  group_by(enabler,comp) %>% summarise(freq=n())

