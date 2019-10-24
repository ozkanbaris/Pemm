library(ggplot2)
source("LoadPemmData.R")

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

 
 
