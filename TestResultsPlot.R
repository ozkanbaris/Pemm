library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)
library(coin) 
source("LoadPemm.R") 

# Create the function.
getPrevmode <- function(v) {
  nz<-filter(v, prev !=0)  %>% select(prev)
  uniqv <- unique(nz[[1]])
  uniqv[which.max(tabulate(match(nz[[1]], uniqv)))]
}

getPostmode <- function(v) {
  nz<-filter(v, postv !=0) %>% select(postv)
  uniqv <- unique(nz[[1]])
  uniqv[which.max(tabulate(match(nz[[1]], uniqv)))]
}
calWX <- function(df) {
  df<-df %>% filter( !(prev ==0 | postv ==0)  ) 
  r<-wilcoxsign_test (df$prev ~ df$postv, distribution = "exact")
  effect <- abs(statistic(r, type = "standardized"))/sqrt(2*length(df$prev))
  list(round(pvalue(r),3) , round(statistic(r, type = "standardized"),3),round(effect,3))
}

assdata<-inner_join(melted_post, melted_pre,
                    by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = "Persoon"))  %>% 
                    select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)

allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) 
allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest() 
allflowsPP <- allflowsPP %>%  mutate(wxTest = map(data, calWX),
                                     preMode = map(data, getPrevmode), 
                                     postMode=map(data, getPostmode)
                                     )

cells<-assdata  %>% group_by(enabler,comp,plev) %>% nest() %>% select (-data)
enablers<-  as_vector(distinct(cells,enabler))
levs<- as_vector(distinct(cells,plev))



# header
grobs<-list(
  rectGrob(gp=gpar(fill=1, alpha=0.5)),
  grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 8)), textGrob("En")),
  grobTree(rectGrob(gp=gpar(fill=4, alpha=0.5)), textGrob(levs[1])),
  grobTree(rectGrob(gp=gpar(fill=5, alpha=0.5)),textGrob(levs[2])),
  grobTree(rectGrob(gp=gpar(fill=6, alpha=0.5)), textGrob(levs[3])),
  grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(levs[4])))


for (en in enablers) {
  
  grobs<-c(grobs,list(grobTree(rectGrob(gp=gpar(fill=7, alpha=0.2)), textGrob(en))))
  ecomps<-as_vector(filter(cells, enabler==en) %>% distinct(comp))
  
  # 
  for (enc in ecomps) {
    grobs<-c(grobs,  list(grobTree(rectGrob(gp=gpar(fill=1, alpha=0.2,fontsize = 3)), textGrob(enc))))
    
    encpleve<-as_vector(filter(cells, enabler==en, comp==enc) %>% select(plev))
    
    
    for (encp in encpleve) {
      pm<-filter(allflowsPP, enabler==en,  comp==enc,plev==encp)
      
      pmd<-as.data.frame(pm$data[[1]])
      pInc<-  sum(pmd$vdiff >0)
      pDec<-  sum(pmd$vdiff <0)
      meda <- median(pmd$prev[pmd$prev!=0])
      medb <- median(pmd$postv[pmd$postv!=0])
     
      grobs<-c(grobs,list(grobTree(
        rectGrob(gp=gpar(fill=ifelse(pm$wxTest[[1]]< .05,"green","white"), alpha=0.2)), 
        textGrob(
          paste(" Wx.Sig p:Stat:Effect-",
            paste(pm$wxTest[[1]][1], ":", pm$wxTest[[1]][2], ":",pm$wxTest[[1]][3]) ,
            " (+):",
            pInc,
            "(-)",
            pDec,
            "\n medPr-Pst:",
            meda,
          ":",
          medb,
          "mods",
          pm$preMode,
          "-",
          pm$postMode
        ),        gp = gpar(fontface = ifelse((medb != meda), 2, 1),fontsize=7))
        
      )))
        
        }
    }
  }


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
# 
grid.arrange(grobs= grobs, layout_matrix = lay, 
             widths = unit(c(1, 1, 1, 4,4,4,4),  c("null", "null", "null", "null","null","null","null")))



# 
# 
# ##########Calculate PLot Mods#################
# 
# allflowsMods <- allflowsPP %>%  mutate(gmodel = map(data, calWX),
#                                      preMode = map(data, getPrevmode), 
#                                      postMode=map(data, getPostmode) ) %>% mutate(clmmP=map(data, calClmm))
# 
# #aggregation at comp levels
# 
# ggplot( allflows, aes(prev, postv, size=freq , color=plev)) +
#   geom_point(alpha=0.4, position=position_jitter(h=0.02,w=0.02))+
#   scale_size_continuous(range = c(4, 10), trans="exp")+
#   geom_abline(intercept = 0, slope = 1, colour="#E41A1C") +
#   # theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
#   coord_cartesian(xlim = 0:3,ylim =0:3)+
#   facet_wrap( vars(enabler,comp), ncol=2,labeller = label_wrap_gen(multi_line=FALSE))
# 
