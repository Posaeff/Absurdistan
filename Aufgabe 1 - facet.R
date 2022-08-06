load("board games")

ratings<- as.data.frame(tuesdata$ratings)
head(ratings)

details <- as.data.frame(tuesdata$details)

#install.packages("cowplot")
library(cowplot)
library(tidyverse)
library(patchwork)
library(ggdark)
library(Hmisc)
####################
#head(details)

counts_details <- details %>% 
  group_by(yearpublished) %>% 
  count(name = "counts") %>% 
  ungroup() %>%
  #mutate(yearpublished = as.factor(yearpublished))%>%
  mutate(counts_cat = as.factor( 
    ifelse(counts >=1 & counts <=100, "[1-100]",
           ifelse(counts >= 101 & counts <= 250, "[101-250]", 
                  ifelse(counts >= 251 & counts <=500, "[251-500]",
                         ifelse(counts >=501 & counts <=1000, "[501-1000]",
                                ifelse(counts >=1001,">1000" ,"999"))))))) %>% 
  mutate(year_cat = as.factor(
    ifelse(yearpublished >= -3500 & yearpublished <= 1500,"1",
           ifelse(yearpublished >= 1501 & yearpublished <=1880 ,"2",
                  ifelse(yearpublished >= 1881 & yearpublished <= 1975,"3",
                         ifelse(yearpublished >= 1976 ,"4","999")
                  )))
  ))

counts_details$counts_cat <-factor(counts_details$counts_cat,
                                   levels = c("[1-100]",
                                              "[101-250]",
                                              "[251-500]",
                                              "[501-1000]",
                                              ">1000"))

table(counts_details$year_cat)

#?scale_size
#?rev

p1 <- counts_details %>% 
  ggplot(aes(yearpublished, counts, group=1, 
             colour = counts_cat)) +
  geom_point(aes(size = counts_cat), shape = 20) +
  #scale_x_discrete(breaks = c("-3500", "-2000",
  # "-1000", 
  #"0","1000", "1500")) +
  dark_theme_gray() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_colour_manual(values=myColors) +
  labs(title = "Published board games over the centuries", 
       subtitle = "3500 B.C. -  2023 A.C.") +
  facet_wrap(~year_cat,scales = "free", ncol = 1,
             strip.position = "right") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()) +
  scale_y_continuous(limits = c(0,NA)) +
  
  theme(
    text  = element_text(family = "Castellar"), 
    plot.title = element_text(size = 20,hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 14),
    axis.text = element_text(size = 10))
    
p1


