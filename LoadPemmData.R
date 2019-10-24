library(tidyverse)
library(reshape2)

postpem <- read_csv('data/PEMMPST.csv')
prepem  <- read_csv('data/PEMMPR.csv') 
postpem <- postpem %>% replace(., is.na(.), 0)
prepem  <- prepem %>% replace(., is.na(.), 0)

melted_post <-
  melt(postpem, id.vars = "Persoon", value.name = "postv")  %>%
  mutate(
    enabler = str_sub(variable, 1, 1),
    plev = str_sub(variable, 4, 5),
    comp = str_sub(variable, 2, 3)
  )   
melted_pre <-
  melt(prepem, id.vars = "Persoon", value.name = "prev")   %>%
  mutate(
    enabler = str_sub(variable, 1, 1),
    plev = str_sub(variable, 4, 5),
    comp = str_sub(variable, 2, 3)
  )  

assdata <- inner_join(
  melted_post,
  melted_pre,
  by = c(
    "enabler" = "enabler",
    "plev" = "plev",
    "comp" = "comp",
    "Persoon" = "Persoon"
  )
)  %>%  select(-variable.x,-variable.y) 

allflows <- assdata  %>% group_by(enabler,comp,plev, prev, postv) 
allflows$comp=factor(allflows$comp,levels = c("PU","CO","DO","KN","SK","BR","ID","AC","AU","IS","HR","DE","US"))
allflows$plev=factor(allflows$plev, levels = c("P1", "P2", "P3", "P4"))
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



