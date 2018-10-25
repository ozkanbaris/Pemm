library(tidyverse)
library(reshape2)
postpem <- read_csv('PEMMPST.csv')
prepem <- read_csv('PEMMPR.csv')

melted_post<-melt(postpem, id.vars = "Persoon")
melted_pre<-melt(postpem, id.vars = "Persoon")






ggplot(data = melted_post, aes(y=Persoon, x=variable, fill=value)) + 
  geom_tile()
ggplot(data = melted_pre, aes(y=Persoon, x=variable, fill=value)) + 
  geom_tile()
