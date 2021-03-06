library(tidyverse)
library(reshape2)

postpem <- read_csv('data/PEMMPST.csv')
prepem  <- read_csv('data/PEMMPR.csv') 
postpem <- postpem %>% replace(., is.na(.), 0)
prepem  <- prepem %>% replace(., is.na(.), 0)
cfint <- read_csv('data/cfint.csv')

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

allflows <- assdata 
allflows$comp=factor(allflows$comp,levels = c("PU","CO","DO","KN","SK","BR","ID","AC","AU","IS","HR","DE","US"))
allflows$plev=factor(allflows$plev, levels = c("P1", "P2", "P3", "P4"))


