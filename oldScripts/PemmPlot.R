library(ggplot2)
library(ggalluvial)
source("TestResultsPlot.R") 

eNames<-c("Design","Performer","Owner","Infrastructure","Metrics")
cNames<-c( "Purpose", "Context", "Documentation","Knowledge", "Skills", "Behavior", "Identity", "Activities", 
           "Authority","IS", "HRS","Definition", "Uses")
ccodes<- c("PU","CO","DO","KN","SK","BR","ID","AC","AU","IS","HR","DE","US")

testAsString<-function(comp1,plev1){
  pm<-filter(allflowsPP,comp==comp1,plev==plev1)
  pmd<-as.data.frame(pm$data[[1]])
  # pInc<-  sum(pmd$vdiff >0)
  # pDec<-  sum(pmd$vdiff <0)
  medb <- median(pmd$prev[pmd$prev!=0])
  meda <- median(pmd$postv[pmd$postv!=0])
  altLabel<- paste(comp1, plev1,
    " Median(Pre-Post)=", medb,"-", meda,
    "\n W=", pm$wxTest[[1]][2], 
    ",P=" , pm$wxTest[[1]][1] ,
    ",r=", pm$wxTest[[1]][3]
    )
}
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



appender <- function(string) {

  l<-substr(string,3,100)
  l
}

allflowsG<-group_by(allflows,comp,prev,postv) %>% summarise (freq=n())

allflowsG$comp=factor(allflowsGR$comp,levels = c("PU","CO","DO","KN","SK","BR","ID","AC","AU","IS","HR","DE","US"))
allflowsG$plev=factor(allflowsGR$plev, levels = c("P1", "P2", "P3", "P4"))

# allflowsGR<-filter(allflowsGR, comp=="PU", plev=="P1")

# pointPlot at P levels
pointPlotP<-ggplot( allflowsG, aes(prev, postv, size=freq)) +
  geom_point(aes(), alpha=0.4, pch=21, stroke = 1.5)+
  scale_size_continuous(range = c(2,6))+
  geom_abline(intercept = 0, slope = 1, colour="blue") +
  theme_minimal() + 
  theme(axis.title=element_blank(), axis.text =  element_blank()) +
  coord_cartesian(xlim = 0.5:3.5,ylim =0.5:3.5)+
  facet_wrap(. ~ comp, ncol=4,labeller =labeller(comp=lookupC, altP=appender,.multi_line = FALSE))+
  scale_fill_discrete(name="Frequency")+
  ggtitle("Pre-Post BPMS Implementation PEMM Effect Frequency Plot")+
  theme(
     panel.spacing = unit(0, "lines"),
     legend.position="none",
     legend.text = element_text(colour="blue", size=5),
     legend.key.height = unit(2,"line"),
     legend.margin = margin(1, 1, 1, 1),
     strip.background = element_rect(colour = "gray", size=0.5),
     strip.text = element_text(size=5),
     panel.border = element_rect(colour = "grey", fill=NA, size=0.3)
  )


#Aggregation alluvial at C levels with P colored
# dorianGray<-c(  "#fef0d9",  "#fdcc8a",  "#fc8d59",  "#d7301f")
dorianGray<-c("#e66101","#fdb863","#b2abd2","#5e3c99")
dfC <- allflowsG %>%  filter(!(prev ==0 | postv ==0)  )
dfC$prev=factor(dfC$prev, levels = c(3, 2, 1))
dfC$postv=factor(dfC$postv, levels = c(3, 2, 1))
allCAlluv<-ggplot(dfC, aes(y = freq, axis1 = prev, axis2 = postv)) +
  geom_flow(aes(fill=plev), width = 1/8, alpha=0.9) +
  geom_stratum(width = 1/8, alpha=0.8)+
  geom_text(stat = "stratum", label.strata = TRUE,  size = 3) +
  scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
  scale_fill_manual(name = "", values=dorianGray) +
  theme_minimal() + 
  theme(axis.title=element_blank(), axis.text = element_blank(), legend.position = c(0.5, 0.1), legend.direction = "horizontal") +
  theme(
    # legend.position = c(1,4),
     panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    axis.text.x = element_text(size = 10))+
    facet_wrap(  ~comp, ncol=3,labeller  =labeller(comp=lookupC,multi_line = FALSE),scales="free_y", 
                 strip.position="left")+
  theme(
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face="plain", size=9,lineheight=2.5)
    # panel.border = element_rect(colour = "red", fill=NA, size=1)
  )

unlink("pointPlotP.pdf")
unlink("allCAlluv.pdf")
ggsave("pointPlotP.pdf", plot=pointPlotP, width = 20, height = 20, units = "cm")
ggsave("allCAlluv.pdf", plot=allCAlluv, width = 20, height = 20, units = "cm")


distinct(filter(group_by(allflows, comp, plev),all(vdiff<=0)), comp, plev)


mv<- allflowsPP %>%  select(comp,plev,medpre, medpost) %>% unnest() %>% mutate(medpre=medpre, medpost= medpost)
mv<-melt(mv) %>% rename(Rating=value)
labelM<-c(
  medpre = "Pre-implementation Maturity Level Map",
  medpost = "Post-implementation Maturity Level Map"
  
)
p<- ggplot(mv,aes(x=comp,y=plev,fill=Rating))+labs(y= 'Process Maturity Level', x='PEMM Component', )
p+geom_tile(colour = "blue")+
  scale_fill_continuous(low = "#e6e6e6", high = "#636363",name="Rating\nMedian", breaks=1:3)+
   theme(legend.text=element_text(size=12), legend.title=element_text(size=12),
         axis.text.x = element_text(size=14,angle = 90, hjust = 1, vjust = 0.5))+
  scale_x_discrete(labels = lookupC)+
  theme(    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  facet_wrap(~variable, labeller =labeller(variable=labelM, .multi_line = FALSE))+
  theme(strip.text.x = element_text(size = 18))
  



dfC <- allflows %>%  filter(!(prev ==0 | postv ==0)  )
dfC$prev=factor(dfC$prev, levels = c(3, 2, 1))
dfC$postv=factor(dfC$postv, levels = c(3, 2, 1))

dfC1 <- dfC %>%  filter(postv!= prev)  %>% group_by(comp) %>% summarise (count=n()) %>% arrange()


dfC2 <- dfC %>%  filter(postv== prev)  %>% group_by(comp) %>% summarise (count=n()) %>% arrange()

t<-inner_join(dfC1,dfC2,by='comp') %>% mutate(rr=count.x/count.y)


ggplot(dfC, aes(y = 1, axis1 = prev, axis2 = postv)) +
  geom_flow(aes(fill=plev), width = 1/14, alpha=0.5) +
  geom_stratum(width = 1/14)+
  geom_text(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) 
  
 
 
