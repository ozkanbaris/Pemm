library(tidyverse)
library(reshape2)
library(likert)
postpem <- read_csv('PEMMPST.csv')
prepem  <- read_csv('PEMMPR.csv') 

melted_post <-
  melt(postpem, id.vars = "Persoon", value.name = "postv")  %>%
  mutate(
    enabler = str_sub(variable, 1, 1),
    plev = str_sub(variable, 4, 5),
    comp = str_sub(variable, 2, 3)
  )   %>% replace(., is.na(.), 0)
melted_pre <-
  melt(prepem, id.vars = "Persoon", value.name = "prev")   %>%
  mutate(
    enabler = str_sub(variable, 1, 1),
    plev = str_sub(variable, 4, 5),
    comp = str_sub(variable, 2, 3)
  )  %>% replace(., is.na(.), 0)
# 
# 
# postpem<-postpem %>% replace(., is.na(.), 0)
# 
# prepem<-prepem %>% replace(., is.na(.), 0)
# 
# cols <- c(2:53)
# 
# prepem <- lapply(prepem[cols], factor,levels = c("0","1", "2", "3"),ordered = TRUE)
# p1<- likert(as.data.frame(prepem))
# plot(p1, ordered=FALSE)
# 
# 
# postpem <- lapply(postpem[cols], factor,levels = c("0","1", "2", "3"),ordered = TRUE)
# p2<- likert(as.data.frame(postpem))
# plot(p2, ordered=FALSE)



