library(tidyverse)
library(reshape2)

library(PairedData)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

postpem <- read_csv('PEMMPST.csv')
prepem  <- read_csv('PEMMPR.csv') 

melted_post<-melt(postpem, id.vars="Persoon")  %>% mutate(phase="postpem", enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                       comp=str_replace(str_sub(variable,2,10), plev ,"") )
melted_pre<-melt(prepem, id.vars="Persoon")   %>% mutate(phase="prepem", enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                     comp=str_replace(str_sub(variable,2,10), plev ,"") )

assdata<-bind_rows(melted_pre,melted_post) 

# enablers_Plev<- assdata %>% select(-Persoon) %>% 
#   group_by(phase,enabler, comp, plev) %>% summarise(modv=getmode(value)) 

filter(assdata, phase="prepem") %>% select(Persoon, variable,value)
filter(assdata, phase="postpem") %>% select(Persoon, variable,value)

pd <- paired(, )
plot(value, type = "profile") + theme_bw()

res <- wilcox.test(weight ~ group, data = my_data, paired = TRUE)


set.seed(1L)
cb <- data.frame(group = factor(c("A", "B", "C", "D", "E")), 
                 WC = runif(100, 0, 100), 
                 Ana = runif(100, 0, 100), 
                 Clo = runif(100, 0, 100))
Code:
  
  library(purrr)

combins <- combn(levels(cb$group), 2)

params_list <- split(as.vector(combins), rep(1:ncol(combins), each = nrow(combins)))

model_wc <- map(.x = params_list, 
                .f = ~ wilcox.test(formula = WC ~ group, 
                                   data    = subset(cb, group %in% .x)))

model_ana <- map(.x = params_list, 
                 .f = ~ wilcox.test(formula = Ana ~ group, 
                                    data    = subset(cb, group %in% .x)))

model_clo <- map(.x = params_list, 
                 .f = ~ wilcox.test(formula = Clo ~ group, 
                                    data    = subset(cb, group %in% .x)))

wilcox_pvals <- do.call(cbind, list(t(data.frame(map(.x = model_wc, .f  = "p.value"))),
                                    t(data.frame(map(.x = model_ana, .f = "p.value"))),
                                    t(data.frame(map(.x = model_clo, .f = "p.value")))))

row.names(wilcox_pvals) <- unlist(map(.x = params_list, .f = ~ paste0(.x, collapse = "")))

colnames(wilcox_pvals) <- names(cb)[2:4]





