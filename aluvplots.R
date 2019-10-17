library(ggplotify)
library(ggplot2)
library(ggalluvial)
source("LoadPemm.R")


freqs <- allflows %>% group_by(enabler,comp,plev,prev,postv) %>% summarise(freq=n()) %>% filter(prev!=0 && postv!00)
is_alluvia_form(as.data.frame(freqs), axes = 1:5, silent = TRUE)


ggplot(as.data.frame(freqs),
       aes(y = freq, axis1 = enabler,  axis3= prev, axis4= postv)) +
  geom_alluvium(aes(fill = comp), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Enabler", "Component", "Before","After"), expand = c(.05, .05,.05,.05 )) +
 # scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")