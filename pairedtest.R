library(coin) 
library(tidyverse)
library(reshape2)

postpem <- read_csv('PEMMPST.csv')
prepem  <- read_csv('PEMMPR.csv') 
melted_post<-melt(postpem, id.vars="Persoon", value.name = "postv")  %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                                 comp=str_replace(str_sub(variable,2,10), plev ,"") )  
melted_pre<-melt(prepem, id.vars="Persoon", value.name = "prev")   %>% mutate( enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                                                               comp=str_replace(str_sub(variable,2,10), plev ,"") )
assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = 'Persoon'))  %>% select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)
# prepare data

# enablers_Plev<- assdata %>% select(-Persoon) %>% 
#   group_by(phase,enabler, comp, plev) %>% summarise(modv=getmode(value)) 

before <- filter(assdata, phase=="prepem", enabler=="D", comp=="P", plev=="P4")  
after <-  filter(assdata, phase=="postpem", enabler=="D", comp=="P", plev=="P4")  
res <- wilcoxsign_test (before$value ~ after$value, zero.method = c("Pratt"))
res <- wilcoxsign_test (before$value ~ after$value, distribution="exact")
res
# resres <- wilcox.test(value ~ phase, data = bind_rows(before,after), paired = TRUE, zero.method="Pratt")


# 
# set.seed(1L)
# cb <- data.frame(group = factor(c("A", "B", "C", "D", "E")), 
#                  WC = runif(100, 0, 100), 
#                  Ana = runif(100, 0, 100), 
#                  Clo = runif(100, 0, 100))
# Code:
#   
#   library(purrr)
# 
# combins <- combn(levels(cb$group), 2)
# 
# params_list <- split(as.vector(combins), rep(1:ncol(combins), each = nrow(combins)))
# 
# model_wc <- map(.x = params_list, 
#                 .f = ~ wilcox.test(formula = WC ~ group, 
#                                    data    = subset(cb, group %in% .x)))
# 
# model_ana <- map(.x = params_list, 
#                  .f = ~ wilcox.test(formula = Ana ~ group, 
#                                     data    = subset(cb, group %in% .x)))
# 
# model_clo <- map(.x = params_list, 
#                  .f = ~ wilcox.test(formula = Clo ~ group, 
#                                     data    = subset(cb, group %in% .x)))
# 
# wilcox_pvals <- do.call(cbind, list(t(data.frame(map(.x = model_wc, .f  = "p.value"))),
#                                     t(data.frame(map(.x = model_ana, .f = "p.value"))),
#                                     t(data.frame(map(.x = model_clo, .f = "p.value")))))
# 
# row.names(wilcox_pvals) <- unlist(map(.x = params_list, .f = ~ paste0(.x, collapse = "")))
# 
# colnames(wilcox_pvals) <- names(cb)[2:4]
# 
# 
# 
# 
# 
