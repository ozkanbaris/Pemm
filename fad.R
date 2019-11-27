library(cowplot)
library(stringr)
library(tidyverse)
library(reshape2)

cfint <- read_csv('data/cfint.csv') 


meltcf<-cfint %>% select (-Capability) %>% reshape2::melt(id.vars= c("Factor","CID")) 
meltcf$variable=factor(meltcf$variable, levels = c("H", "M", "L", "NI"))

lup<-cfint 
vcids<-lup$CID
cctext<- str_wrap(lup$Capability, width = 10)
names(cctext)<-vcids
lbr<- as_labeller(cctext)

meltcf<-meltcf %>% arrange(CID)

pie1 <- ggplot(data = meltcf , aes(x = "", y = value, fill = variable )) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = ""), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y")+ theme(legend.title = element_blank()) +scale_fill_grey() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 6, face = "bold"),
        panel.grid  = element_blank())+
  facet_wrap(. ~ CID, ncol = 5, labeller=lbr)+
  theme(legend.position = "none", axis.title = element_blank())+
  theme(strip.text.x = element_text(size = 7, face = "bold"))

ggsave('first_page_plots.pdf', pie1, dpi=600)


