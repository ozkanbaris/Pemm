library(tidyverse)
library(reshape2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

postpem <- read_csv('PEMMPST.csv') %>% select(-Persoon)
prepem  <- read_csv('PEMMPR.csv')  %>% select(-Persoon)

melted_post<-melt(postpem, id.vars="Persoon")  %>% mutate(phase="postpem", enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                       comp=str_replace(str_sub(variable,2,10), plev ,"") )
melted_pre<-melt(prepem, id.vars="Persoon")   %>% mutate(phase="prepem", enabler= str_sub(variable,1,1), plev=str_sub(variable,-2,-1), 
                                     comp=str_replace(str_sub(variable,2,10), plev ,"") )

# aTYpes <- c("postpem", "prepem")
# enabler<- c(rep(c("D"),12),rep(c("P"),12),rep(c("O"),12),rep(c("I"),8),rep(c("M"),8))

assdata<-bind_rows(melted_pre,melted_post) 



# set.seed(3)
# ggplot(mass, aes(x=variable, y=value, colour = atype)) + 
#   geom_point(position=position_jitter(h=0.15,w=0.15))

ggplot(mass, aes(x=variable, y=value, colour = atype, size=atype)) +
  geom_point() +
  scale_color_manual(values=c("red","blue")) +
  scale_size_manual(values=c(5,4))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = melted_post, aes(y=Persoon, x=variable, fill=value)) + 
  geom_tile()
ggplot(data = melted_pre, aes(y=Persoon, x=variable, fill=value)) + 
  geom_tile()
posta<- postpem %>% summarise_all(funs(getmode))
preta<- prepem %>% summarise_all(funs(getmode))


#postFa<- postpem %>% summarise_all(funs(min(., na.rm = TRUE)))
#pretFa<- prepem %>% summarise_all(funs(min(., na.rm = TRUE)))