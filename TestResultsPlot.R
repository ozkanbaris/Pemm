
if(!require(psych)){install.packages("psych")}
if(!require(ordinal)){install.packages("ordinal")}
if(!require(car)){install.packages("car")}
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}

library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)

library(psych)
library(ordinal)
library(car)
library(RVAideMemoire)
library(coin) 

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

calClmm <- function(dft) {
  
  dat1<-dft  %>% select(-postv)  %>% rename( score = prev)   %>% mutate( Time = "Pre")
  dat2<-dft  %>% select(-prev)  %>% rename( score = postv)    %>% mutate( Time = "Post")
  
  myData <- bind_rows(dat1,dat2)
  myData$score.f <- factor(myData$score, ordered = TRUE)
  myData


  # Anova.clmm(model,type = "II")
  
  # testData$score.f = factor(testData$score, ordered = TRUE)
  # 
  # 
  # 
  # model = clmm(score.f ~ Time + (1|Persoon), data = testData)
  # resClmm<-Anova.clmm(model,type = "II")
  # 
  # 
  # model.fixed = clm(score.f ~ Time, data = testData)
  # effSize<-anova(model,null = model.fixed)
  # 
  # model.clm = clm(score.f ~ Time + Persoon, data = testData)
  # nominal_test(model.clm)
  # scale_test(model.clm)
  
  
  
}


assdata<-inner_join(melted_post, melted_pre,
                    by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = "Persoon"))  %>% 
                    select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)

allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) 
allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest() 
allflowsPP <- allflowsPP %>%  mutate(gmodel = map(data, calWX),
                                     preMode = map(data, getPrevmode), 
                                     postMode=map(data, getPostmode) ) %>% mutate(clmmP=map(data, calClmm))

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
    grobs<-c(grobs,  list(grobTree(rectGrob(gp=gpar(fill=1, alpha=0.2,fontsize = 5)), textGrob(enc))))
    
    encpleve<-as_vector(filter(cells, enabler==en, comp==enc) %>% select(plev))
    
    
    for (encp in encpleve) {
      pm<-filter(allflowsPP, enabler==en,  comp==enc,plev==encp)
      pInc<-  sum(pm$data[[1]]$vdiff >0, na.rm=TRUE)
      pDec<-  sum(pm$data[[1]]$vdiff <0, na.rm=TRUE)
      anovaData<-pm$clmmP[[1]]
      
      model = clmm(score.f ~ Time + (1|Persoon), data = anovaData)
      
      anan<-Anova.clmm(model,type = "II")
        
      
      grobs<-c(grobs,list(grobTree(
        rectGrob(gp=gpar(fill=ifelse(pm$gmodel[[1]]< .05,"green","red"), alpha=0.2,fontsize = 5)), 
        textGrob(paste(" Wx.Sig p-Stat", pm$gmodel[[1]]," (+):",pInc, "(-)", pDec,
                       "\n modPr-Pst:", pm$preMode,"-", pm$postMode, " clmmPS ",round(anan[[3]],4)
                       
                      
                       )
                 
                 
                 ))))
      
      
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

