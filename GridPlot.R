library(tidyverse)
library(reshape2)
postpem <- read_csv('PEMMPST.csv')
prepem <- read_csv('PEMMPR.csv')

melted_post<-melt(postpem, id.vars = "Persoon")

ggplot(data = melted_post, aes(x=Persoon, y=variable, fill=value)) + 
  geom_tile()