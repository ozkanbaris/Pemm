library(tidyverse)
library(reshape2)
source("LoadPemmData.R")

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

medos<- function(pmd)median(pmd$prev[pmd$prev!=0])
medos2<- function(pmd)median(pmd$postv[pmd$postv!=0])

allflowsPP <- allflows %>% group_by(enabler,comp,plev) %>% nest() 
allflowsPP <- allflowsPP %>%  mutate(preMode = map(data, getPrevmode), 
                                     postMode=map(data, getPostmode), medpre = map(data,medos), medpost= map(data,medos2) )

      
     
