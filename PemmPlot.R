library(gridExtra)
library(grid)
library(ggplotify)
library(ggplot2)
library(ggalluvial)
source("LoadPemm.R")
eNames<-c("Design","Performer","Owner","Infrastructure","Metrics")
cNames<-c( "Purpose", "Context", "Documentation","Knowledge", "Skills", "Behavior", "Identity", "Activities", 
           "Authority","IS", "HRS","Definition", "Uses")

allflowsG <- allflows %>% summarise(freq = n())
allflowsG<-arrange(allflowsG,comp,plev,prev,postv)

 mylbler <-  function(x) {
   plev
   comp
   a<-"test"
   
 }
 
 
# pointPlot at P levels
pointPlotP<-ggplot( allflowsG, aes(prev, postv, size=freq )) +
  geom_point(alpha=0.4, shape=21, stroke = 1)+
  scale_size_continuous(range = c(4,8), trans="exp")+
  geom_abline(intercept = 0, slope = 1, colour="blue") +
  theme_minimal() + 
  theme(axis.title=element_blank(), axis.text =  element_blank()) +
  coord_cartesian(xlim = 0.5:3.5,ylim =0.5:3.5)+
  labs(x = "stats here ...")+
  facet_wrap(comp~plev, ncol=4,  labeller = mylbler )+
  theme(
    panel.spacing = unit(0, "lines"),
    legend.position="bottom",
    strip.background = element_blank(),
    strip.placement="inside",
    strip.text = element_text(face="bold", size=6,lineheight=3.0),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )

# dorianGray<-c(  "#fef0d9",  "#fdcc8a",  "#fc8d59",  "#d7301f")
dorianGray<-c("#e66101","#fdb863","#b2abd2","#5e3c99")
#Aggregation alluvial at C levels with P colored
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
    facet_wrap(  ~comp, ncol=3,labeller = label_wrap_gen(multi_line=FALSE),scales="free_y", 
                 strip.position="left")+
  theme(
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    strip.placement="inside",
    strip.text = element_text(face="bold", size=6,lineheight=3.0)
    # panel.border = element_rect(colour = "red", fill=NA, size=1)
  )

#Aggregation alluvial at P levels
allPAlluv<-ggplot(dfC, aes(y = freq, axis1 = prev, axis2 = postv)) +
  geom_flow(aes(fill=prev), width = 1/14, alpha=0.5) +
  geom_stratum(width = 1/14)+
  geom_text(stat = "stratum", label.strata = TRUE,  size = 1) +
  scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
  theme_grey() + scale_colour_grey()+
  theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size= 5),
    axis.ticks  = element_blank(),
    axis.text.x = element_text(size = 5)
  )+
  facet_wrap(  ~comp+plev, ncol=4,strip.position="right",
             labeller = label_wrap_gen(multi_line=FALSE),scales="free_y")+
  theme(
    panel.margin = unit(0, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face="bold", size=6,lineheight=5.0),
    panel.border = element_rect(colour = rgb(1.0, 0, 0, 0.5), fill=NA, size=1)
    
  )

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