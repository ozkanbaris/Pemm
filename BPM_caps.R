library(cowplot)
library(stringr)
library(tidyverse)
library(reshape2)




# meltcf<-cfint %>% select (-Capability) %>% reshape2::melt(id.vars= c("Factor","CID")) 
# meltcf$variable=factor(meltcf$variable, levels = c("H", "M", "L", "N"))
# 
# lup<-cfint 
# vcids<-lup$CID
# cctext<- str_wrap(lup$Capability, width = 10)
# names(cctext)<-vcids
# lbr<- as_labeller(cctext)
# 
# meltcf<-meltcf %>% arrange(CID)
# 
# pie1 <- ggplot(data = meltcf , aes(x = "", y = value, fill = variable )) + 
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = ""), position = position_stack(vjust = 0.5), show.legend = FALSE) +
#   coord_polar(theta = "y")+ theme(legend.title = element_blank()) +scale_fill_grey() +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(size = 6, face = "bold"),
#         panel.grid  = element_blank())+
#   facet_wrap(. ~ CID, ncol = 5, labeller=lbr)+
#   theme(legend.position = "none", axis.title = element_blank())+
#   theme(strip.text.x = element_text(size = 7, face = "bold"))
# 
# ggsave('first_page_plots.pdf', pie1, dpi=600)


cfint <- read_csv('data/cfint2.csv') 
cfint$Rating=factor(cfint$Rating, levels= c("N","L", "M","H"), ordered = TRUE)
cfint$Factor=factor(cfint$Factor, levels = c("STRATEGIC ALIGNMENT", "GOVERNANCE","PEOPLE", "METHODS", "INFORMATION TECHNOLOGY","CULTURE"), ordered=TRUE)
cfint$freq=1
cfint$CID=as.factor(cfint$CID)
# color<-c( "#0072b280", "#D55E0080", "#009e7380", "red")
# ncol<-c("N","L", "M","H")
# names(color)<-ncol
# 


dorianGray<-c("#FFFFFF","#e5e5e5","#cccccc","#999999")

  

p<- ggplot(cfint,aes(x=str_wrap(cfint$Factor,width = 10),y=Row,fill=Rating, label=Capability))+
  theme(    panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text.y =element_blank(),
            axis.text.x =element_text(size=10, face = "bold"),
            legend.position = "none",
            axis.ticks = element_blank())+
            scale_x_discrete(position = "top", expand=c(0,0),limits=str_wrap(c("STRATEGIC ALIGNMENT", "GOVERNANCE", "METHODS", "INFORMATION TECHNOLOGY","PEOPLE","CULTURE"), width =10))+
            scale_y_discrete( expand=c(0,0))+
          geom_tile(color = "black")+ 
          geom_fit_text(reflow = TRUE, grow=FALSE, size=10)+
  scale_fill_manual(name="", values =dorianGray)

p


