library(coin) 
library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)
source("LoadPemm.R") 

# Create the function.
getPrevmode <- function(v) {
  uniqv <- unique(v$prev,na.rm=TRUE)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getPostmode <- function(v) {
  uniqv <- unique(v$postv,na.rm=TRUE)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

calWX <- function(df) {
 
  # res <- wilcoxsign_test (pairedData$prev ~ pairedData$postv, zero.method = c("Pratt"))
  # res
  df<-df %>% filter(!is.na(prev) | !is.na(postv) ) 
  r<-wilcoxsign_test (df$prev ~ df$postv)
  res<-paste(round(pvalue(r),3) ,"-" , statistic(r, "linear"))
}


assdata<-inner_join(melted_post, melted_pre,
                    by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = "Persoon"))  %>% 
                    select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)

allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) 
allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest() 
allflowsPP <- allflowsPP %>%  mutate(gmodel = map(data, calWX), preMode = map(data, getPrevmode), postMode=map(data, getPostmode) )

cells<-assdata  %>% group_by(enabler,comp,plev) %>% nest() %>% select (-data)

# enablers<-  as_vector(top_n (distinct(cells,enabler),1))
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
  
  grobs<-c(grobs,list(grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(en))))
  ecomps<-as_vector(filter(cells, enabler==en) %>% distinct(comp))
  
  # 
  for (enc in ecomps) {
    grobs<-c(grobs,  list(grobTree(rectGrob(gp=gpar(fill=1, alpha=0.5,fontsize = 8)), textGrob(enc))))
    
    encpleve<-as_vector(filter(cells, enabler==en, comp==enc) %>% select(plev))
    
    
    for (encp in encpleve) {
      pFrame<-filter(allflowsPP, enabler==en,  comp==enc,plev==encp)
      pInc<-  sum(pFrame$data[[1]]$vdiff >0, na.rm=TRUE)
      pDec<-  sum(pFrame$data[[1]]$vdiff <0, na.rm=TRUE)
        
      pm<-select(pFrame,gmodel)
      grobs<-c(grobs,list(grobTree(rectGrob(gp=gpar(fill=ifelse(pm$gmodel[[1]]< .05,"green","red"), alpha=0.5,fontsize = 8)), 
                                   textGrob(paste(pm$gmodel[[1]],"\n (+):",pInc, "(-)", pDec, "modPr:", pFrame$preMode,"modPo:", pFrame$preMode)))))
      
      
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
             widths = unit(c(1, 1, 1, 4,4,4,4),  c("lines", "null", "null", "null","null","null","null")))

