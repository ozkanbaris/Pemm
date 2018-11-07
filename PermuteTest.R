if(!require(psych)){install.packages("psych")}
if(!require(ordinal)){install.packages("ordinal")}
if(!require(car)){install.packages("car")}
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}


library(psych)
library(ordinal)
library(car)
library(RVAideMemoire)
library(tidyverse)
library(reshape2)

source("LoadPemm.R") 


melted_prem = melted_pre%>% mutate(Time=1)  %>% rename( score = prev)
melted_postm = melted_post%>% mutate(Time=2)  %>% rename(  score = postv)
flatdata = bind_rows(melted_prem,melted_postm) %>% select(-variable)
testData = flatdata %>%  filter(enabler=="D", comp=="C", plev=="P3")

### Create a new variable which is the Likert scores as an ordered factor

testData$score.f = factor(testData$score, ordered = TRUE)



model = clmm(score.f ~ Time + (1|Persoon), data = testData)
resClmm<-Anova.clmm(model,type = "II")


model.fixed = clm(score.f ~ Time, data = testData)
effSize<-anova(model,null = model.fixed)

model.clm = clm(score.f ~ Time + Persoon, data = testData)
nominal_test(model.clm)
scale_test(model.clm)




