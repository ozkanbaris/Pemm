library (tidyverse)
library (brms)
source("LoadPemmData.R")


bdata<- bind_rows(melted_pre,melted_post)   %>% 
  select (-variable) %>% reshape2::melt(measure.vars= c("prev","postv")) %>% filter(value!=0)%>% 
  mutate(pl= factor(value, ordered=TRUE, levels= c("1","2","3"))) %>%  select(-pl)
# levels= c("0","1","2","3")))
bdata<-rename(bdata, Assessor = Persoon, Time=variable, Rating=value, Comp=comp )
bdata$Time=recode(bdata$Time, prev="t1", postv="t2")
bdata$Time<- relevel(bdata$Time, ref = "t1")

bdata <- bdata %>% mutate (Assessor=factor(Assessor)) 

ggplot(bdata, aes(Time))+geom_bar(aes(fill = factor(Rating)))+ coord_flip() + theme(legend.position = "top")
# ggplot(data = bdata, aes(x = Rating, fill = ..x..)) +  geom_bar()


prior_ma <- prior(normal(0, 5), class = "b") +prior(normal(0, 5), class = "Intercept")
fitc <- brm(Rating ~ Time+(1|Assessor)+(1|Comp), data = bdata, prior= prior_ma, family=cumulative(link= "logit", threshold="flexible"))
# Props assumpption see brms ordinal guidelines
# fitc <- brm(Rating ~ Time+(1|Assessor)+(1|Comp), data = bdata, prior= prior_ma, family=acat(link= "logit", threshold="flexible"))
# fitc <- brm(Rating ~ cs(Time)+(1|Assessor)+(1|Comp), data = bdata, prior= prior_ma, family=acat(link= "logit", threshold="flexible"))

summary(fitc)
plot(fitc)
plot(marginal_effects(fitc, categorical = TRUE))
# marginal_smooths(fit1)
brms::pp_check(fitc)

coda = brms::posterior_samples(fitc)
newdata = data.frame(Time = levels(bdata$Time))
fit = fitted(   fitc,   newdata = newdata,    re_formula = NA,  summary = TRUE )* 100 
colnames(fit) = c('fit', 'se', 'lwr', 'upr')
df_plot = cbind(newdata, fit)




y<- bdata$Rating
yrep<-


