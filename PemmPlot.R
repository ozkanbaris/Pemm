library(gridExtra)
library(grid)
library(ggplotify)
library(ggplot2)
library(ggalluvial)
source("LoadPemm.R")
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

allflowsG <- allflows %>% summarise(freq = n())
allflowsG<-arrange(allflowsG,comp,plev,prev,postv)
allflowsGR<- allflowsG%>% mutate(altP=testAsString(comp, plev))
allflowsGR$comp=factor(allflowsGR$comp,levels = c("PU","CO","DO","KN","SK","BR","ID","AC","AU","IS","HR","DE","US"))
allflowsGR$plev=factor(allflowsGR$plev, levels = c("P1", "P2", "P3", "P4"))
# allflowsGR<-filter(allflowsGR, comp=="PU", plev=="P1")

# pointPlot at P levels
pointPlotP<-ggplot( allflowsGR, aes(prev, postv, size=freq)) +
  geom_point(alpha=0.4, shape=21, stroke = 1.5, color="#FF335E" )+
  scale_size_continuous(range = c(2,6))+
  geom_abline(intercept = 0, slope = 1, colour="blue") +
  theme_minimal() + 
  theme(axis.title=element_blank(), axis.text =  element_blank()) +
  coord_cartesian(xlim = 0.5:3.5,ylim =0.5:3.5)+
  facet_wrap(comp ~ altP, ncol=4,labeller =labeller(comp=lookupC, altP=appender,.multi_line = FALSE))+
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


# #Aggregation alluvial at P levels
# allPAlluv<-ggplot(dfC, aes(y = freq, axis1 = prev, axis2 = postv)) +
#   geom_flow(aes(fill=prev), width = 1/14, alpha=0.5) +
#   geom_stratum(width = 1/14)+
#   geom_text(stat = "stratum", label.strata = TRUE,  size = 1) +
#   scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
#   theme_grey() + scale_colour_grey()+
#   theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
#   theme(
#     legend.position = "none",
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(),
#     axis.text.y = element_text(size= 5),
#     axis.ticks  = element_blank(),
#     axis.text.x = element_text(size = 5)
#   )+
#   facet_wrap(  ~comp+plev, ncol=4,strip.position="right",
#              labeller = label_wrap_gen(multi_line=FALSE),scales="free_y")+
#   theme(
#     panel.margin = unit(0, "lines"),
#     strip.background = element_blank(),
#     strip.text = element_text(face="bold", size=6,lineheight=5.0),
#     panel.border = element_rect(colour = rgb(1.0, 0, 0, 0.5), fill=NA, size=1)
#     
#   )

# # header
# grobs<-list(
#   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 5)), textGrob("En")),
#   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 5)), textGrob("comp")),
#   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 5)), textGrob("Comphere")),
#   grobTree(rectGrob(gp=gpar(fill=4, alpha=0.5)), textGrob(levs[1])),
#   grobTree(rectGrob(gp=gpar(fill=5, alpha=0.5)),textGrob(levs[2])),
#   grobTree(rectGrob(gp=gpar(fill=6, alpha=0.5)), textGrob(levs[3])),
#   grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(levs[4])))
# 
# 
# grobs<-c(grobs,list(allCAlluv,pointPlotP, textGrob("CNAME")))
# 
# 
# lay <- rbind(
#   c(1,2,3,4,5,6,7),
#   c(1,10,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9)
#   
# )
# 
# grid.arrange(grobs = grobs,
#              layout_matrix = lay,
#              widths = unit(c(1, 1, 10, 10,10, 10,10),
#                c("null", "null", "null", "null", "null", "null", "null")
#              ))



#One Comp Test
# dfC <- allflows %>%  filter(enabler == "D", comp == "P")%>% ungroup() %>% select(plev,prev,postv,freq) %>%  filter(!(prev ==0 | postv ==0)  )
# dfC$prev=factor(dfC$prev, levels = c(3, 2, 1))
# dfC$postv=factor(dfC$postv, levels = c(3, 2, 1))
# # levels(dfC$postv) <- rev(levels(dfC$postv))
# # levels(dfC$prev) <- rev(levels(dfC$prev))
# set.seed(564)
# ggplot(dfC, aes(y = freq, axis1 = prev, axis2 = postv)) +
#   geom_alluvium(aes(fill=plev), width = 1/12, alpha=0.3) +
#   geom_stratum(width = 1/12)+
#   geom_text(stat = "stratum", label.strata = TRUE) +
#   scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks  = element_blank(),
#     axis.text.x = element_text(size = 2)
#   )

distinct(filter(group_by(allflows, comp, plev),all(vdiff<=0)), comp, plev)
mv<- allflowsPP %>%  select(comp,plev,medpre, medpost) %>% unnest() %>% mutate(medpre=floor(medpre), medpost= floor(medpost))
mv<-melt(mv) %>% rename(Rating=value)
labelM<-c(
  medpre = "Pre-implementation Maturity Level Map",
  medpost = "Post-implementation Maturity Level Map"
  
)
p<- ggplot(mv,aes(x=comp,y=plev,fill=Rating))+labs(y= 'Maturity Levels', x='PEMM Components')
p+geom_tile(colour = "blue")+scale_fill_continuous(low = "#f0f0f0", high = "#636363", breaks=1:3 )+
  facet_wrap(~variable, labeller =labeller(variable=labelM, .multi_line = FALSE))



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
  
 
 
