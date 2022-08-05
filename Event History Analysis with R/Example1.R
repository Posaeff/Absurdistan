# install.packages("survival",dependencies = T)
# install.packages("eha", dependencies = T)

library(tidyverse)
library(eha)
data(oldmort)

oldmort$enter[1:20] + oldmort$birthdate[1:20]

############
data(mort)

mort[3:4,]

########

data(infants)

head(infants)

#######

data(swepop)
data(swedeaths)
swedeaths$id<- NULL
swepop$id<- NULL
swe <- merge(swepop, swedeaths, by = c("age","year","sex"))

swe_log <- swe %>% 
  mutate(log_pop= log(pop)) %>% 
  filter(year >= 2007, age >=61)
  
