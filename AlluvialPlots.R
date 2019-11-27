library(ggplot2)
library(ggalluvial)
source("LoadPemmData.R")


freqs <- allflows %>% group_by(enabler,comp,plev,prev,postv) %>% summarise(freq=n()) %>% filter(prev!=0 && postv!=0)
is_alluvia_form(as.data.frame(freqs), axes = 1:5, silent = TRUE)
# freqs$Persoon <- as.factor(freqs$Persoon)


ggplot(as.data.frame(freqs),
       aes(y = freq,  axis2= prev,  axis4= postv)) +
  geom_alluvium(aes(fill = prev), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Component", "Before","After"), expand = c(.05, .05,.05,.05 )) +
  facet_wrap(.~enabler)

#  scale_fill_brewer(type = "qual", palette = "Set2") +
  # ggtitle("UC Berkeley admissions and rejections, by sex and department")

data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = Time, stratum = freqs, alluvium = subject,
           y = freq, fill = response, label = round(a, 3))) +
  geom_lode() + geom_flow() +
  geom_stratum(alpha = 0) +
  geom_text(stat = "stratum")