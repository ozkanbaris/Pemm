library(ggplot2)
library(ggalluvial)
source("LoadPemmData.R")

freqs <- allflows %>% group_by(comp, plev, prev,postv) %>% summarise(freq=n()) %>% filter(prev!=0 && postv!=0)
freqs2 <- allflows %>% group_by(comp, prev,postv) %>% summarise(freq=n()) %>% filter(prev!=0 && postv!=0)


fx<- freqs2 %>% mutate(lan=paste(comp,prev,postv))%>%melt ( measure.vars = c("prev","postv"), variable.name = "time" )
fx$value <-as.factor(fx$value)

dorianGray<-c("#e66101","#fdb863","#b2abd2","#5e3c99")
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

e1<-c("PU","CO","DO")

e2<-c("KN","SK","BR")

e3<-c("ID","AC","AU")

e4<-c("IS","HR","PU")

e5<-c("DE","US","PU")


p5<-ggplot(filter(fx, comp %in% e5),
       aes(x = time, stratum = value, alluvium = lan,
           y = freq,
           fill = value, label = value)) +
  geom_flow(width = 1/8, alpha=0.9) +
  geom_stratum(width = 1/8, alpha=0.8)+
  geom_text(stat = "stratum", label.strata = TRUE,  size = 3) +
  # scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
  scale_fill_manual(name = "", values=dorianGray) +
  theme_minimal() + 
  theme(axis.title=element_blank(), axis.text = element_blank())  +
  theme(
    legend.position = "none",
    # legend.position = c(1,4),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    axis.text.x = element_text(size = 10))+
  facet_wrap(  ~comp, ncol=1,labeller  =labeller(comp=lookupC,multi_line = FALSE),scales="free_y")+
  theme(
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face="plain", size=9,lineheight=2.5)+
      theme(legend.position = "none", axis.title = element_blank())
    # panel.border = element_rect(colour = "red", fill=NA, size=1)
  )

plot_grid(p1, p2,p3,p4,p5)







ggplot(as.data.frame(freqs), aes(y = freq, axis1 = prev, axis2 = postv)) +
  geom_flow(aes(fill=plev), width = 1/8, alpha=0.9) +
  geom_stratum(width = 1/8, alpha=0.8)+
  geom_text(stat = "stratum", label.strata = TRUE,  size = 3) +
  scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
  scale_fill_manual(name = "", values=dorianGray) +
  theme_minimal() + 
  theme(axis.title=element_blank(), axis.text = element_blank())  +
  theme(
    legend.position = "none",
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
    strip.text = element_text(face="plain", size=9,lineheight=2.5)+
    theme(legend.position = "none", axis.title = element_blank())
        # panel.border = element_rect(colour = "red", fill=NA, size=1)
  )
