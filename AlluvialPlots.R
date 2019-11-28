library(ggplot2)
library(ggalluvial)
source("LoadPemmData.R")

freqs <- allflows %>% group_by(enabler,comp,plev,prev,postv) %>% summarise(freq=n()) %>% filter(prev!=0 && postv!=0)
is_alluvia_form(as.data.frame(freqs), axes = 1:5, silent = TRUE)
# freqs$Persoon <- as.factor(freqs$Persoon)


ggplot(as.data.frame(freqs), aes(y = freq,  axis1= prev,  axis2= postv)) +
  geom_alluvium(aes(fill = plev), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Before","After"), expand = c(.05, .05,.05,.05 )) +
  facet_wrap(.~comp, scales = "free_y", ncol = 3)


#  scale_fill_brewer(type = "qual", palette = "Set2") +
  # ggtitle("UC Berkeley admissions and rejections, by sex and department")

fx<-melt (as.data.frame(freqs), id.vars = c("enabler", "comp", "plev","freq"), variable.name = "time" )



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




freq1<-freqs%>% filter(enabler=="D")
freq2<-freqs%>% filter(enabler=="P")
freq3<-freqs%>% filter(enabler=="O")
freq4<-freqs%>% filter(enabler=="I")
freq5<-freqs%>% filter(enabler=="M")

dorianGray<-c("#e66101","#fdb863","#b2abd2","#5e3c99")
ggplot(as.data.frame(freq), aes(y = freq, axis1 = prev, axis2 = postv)) +
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