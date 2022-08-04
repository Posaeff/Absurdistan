load("board games")

ratings<- as.data.frame(tuesdata$ratings)
head(ratings)

details <- as.data.frame(tuesdata$details)


library(tidyverse)
library(patchwork)
library(ggdark)
####################
#head(details)

counts_details <- details %>% 
  group_by(yearpublished) %>% 
  count(name = "counts") %>% 
  ungroup() %>% 
  mutate(counts_cat = as.factor( 
    ifelse(counts >=1 & counts <=100, "1_100",
           ifelse(counts >= 101 & counts <= 250, "101_250", 
                  ifelse(counts >= 251 & counts <=500, "251_500",
                         ifelse(counts >=501 & counts <=1000, "501_1000",
                                ifelse(counts >=1001,">1000" ,"999")))))))

counts_details$counts_cat <-factor(counts_details$counts_cat,
                                   levels = c("1_100",
                                              "101_250",
                                              "251_500",
                                              "501_1000",
                                              ">1000"))


df3 <- counts_details %>% 
  select(yearpublished, counts, counts_cat) %>% 
  filter(yearpublished >= -3500 & yearpublished <= 1975) %>% 
  mutate(yearpublished = as.factor(yearpublished)) %>% 
  mutate(counts = as.numeric(counts))

df4 <- counts_details %>% 
  select(yearpublished, counts, counts_cat) %>% 
  filter(yearpublished >= 1976 & yearpublished <= 2023) %>% 
  mutate(yearpublished = as.factor(yearpublished)) %>%
  mutate(counts = as.numeric(counts))

######

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.all()

myColors <- c("#FFFF00","#FFA54F","#EE7942","#DD5B45","#8B3E2F")
barplot(rep(length(myColors),length(myColors)), col=c(myColors));myColors

######

?scale_fill_manual
p3 <-df3 %>% 
  ggplot(aes(yearpublished, counts, group=1,
             colour = counts_cat)) +
  geom_point(aes(size = counts_cat)) +
  scale_x_discrete(breaks = c(-3500, 0, 1000, 1500, 1750, 1850, 1885,
    1900, 1915, 1930, 1940, 1950, 1960, 1970, 1975))+
  scale_colour_manual(values = myColors)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p4 <- df4 %>% 
  ggplot(aes(yearpublished, counts, group=1, 
             color = counts_cat)) +
  geom_point(aes(size = counts_cat)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_colour_manual(values = myColors)

p3 + dark_theme_gray()
p4 + dark_theme_gray()


p3/p4
