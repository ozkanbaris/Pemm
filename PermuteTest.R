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
library(coin)


source("LoadPemm.R") 

#Aggregation PLot at comp levels

assdata<-inner_join(melted_post, melted_pre, by= c("enabler" = "enabler", "plev" = "plev", "comp"="comp", "Persoon" = "Persoon")) %>% 
  select(-variable.x, -variable.y) %>% mutate(vdiff=postv-prev)



dft <- assdata %>%  filter(enabler == "D", comp == "C", plev== "P1") 

dat1<-dft  %>% select(-postv)  %>% rename( score = prev)   %>% mutate( Time = "Pre")
dat2<-dft  %>% select(-prev)  %>% rename( score = postv)    %>% mutate( Time = "Post")
myData <- bind_rows(dat1,dat2) %>% select(Persoon, score, Time)
myData$score <- factor(myData$score , ordered = TRUE)
myData$Persoon <- factor(myData$Persoon)
myData$Time <- factor(myData$Time)

# independence_test(Length ~ Hand,                  data = Data)
symmetry_test(score ~ Time | Persoon,   data = myData,  paired = TRUE)






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




