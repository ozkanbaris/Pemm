library(gridExtra)
library(grid)
library(ggplotify)
library(ggplot2)
source("LoadPemm.R")

plotCP<- function(df){

  pl<-ggplot( df, aes(prev, postv, size=freq )) +geom_point(alpha=0.2)+
    scale_size_continuous(range = c(1, 5))+
    geom_abline(intercept = 0, slope = 1, colour="#E41A1C") +
    theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
    coord_cartesian(xlim = 0:3,ylim =0:3)
  }


assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = "Persoon")) %>% 
  select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)

allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) %>% filter(!is.na(prev) | !is.na(postv) ) %>% summarise(freq=n())

allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest()

allflowsPP <- allflowsPP%>% mutate(gmodel = map(data, plotCP))

cells<-assdata  %>% group_by(enabler,comp,plev) %>% nest() %>% select (-data)

levs<- as_vector(distinct(cells,plev))

enablers<-  as_vector(distinct(cells,enabler))

# header
grobs<-list(
 rectGrob(gp=gpar(fill=1, alpha=0.5)),
   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 8)), textGrob("En")),
    grobTree(rectGrob(gp=gpar(fill=4, alpha=0.5)), textGrob(levs[1])),
     grobTree(rectGrob(gp=gpar(fill=5, alpha=0.5)),textGrob(levs[2])),
      grobTree(rectGrob(gp=gpar(fill=6, alpha=0.5)), textGrob(levs[3])),
        grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(levs[4])))




for (en in enablers) {

  grobs<-c(grobs,list(grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(en))))
  ecomps<-as_vector(filter(cells, enabler==en) %>% distinct(comp))
  
  # 
 for (enc in ecomps) {
   grobs<-c(grobs,  list(grobTree(rectGrob(gp=gpar(fill=1, alpha=0.5,fontsize = 8)), textGrob(enc))))
   
   encpleve<-as_vector(filter(cells, enabler==en, comp==enc) %>% select(plev))
   for (encp in encpleve) {
     
     pm<-select(filter(allflowsPP, enabler==en,  comp==enc,plev==encp),gmodel)
    grobs<-c(grobs,list(as.grob(pm$gmodel[[1]])))
   }
   
 }
  
}


widths=c()
lay <- rbind(
  c(1,2,2,3,4,5,6),
  c(1,7,c(8:12)),
  c(1,7,c(13:17)),
  c(1,7,c(18:22)),
  c(1,23,c(24:28)),
  c(1,23,c(29:33)),
  c(1,23,c(34:38)),
  c(1,39,c(40:44)),
  c(1,39,c(45:49)),
  c(1,39,c(50:54)),
  c(1,55,c(56:60)),
  c(1,55,c(61:65)),
  c(1,66,c(67:71)),
  c(1,66,c(72:76)))

 grid.arrange(grobs= grobs, layout_matrix = lay, 
              widths = unit(c(1, 1, 1, 4,4,4,4),  c("lines", "null", "null", "null","null","null","null")))
 
 
 d=data.frame(s=c(1,2,3), ic=c(0,2,5))
 
 
df<- allflowsPP %>%  filter(enabler=="D", comp=="P", plev=="P2")





 