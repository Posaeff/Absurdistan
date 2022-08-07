load("board games")

ratings<- as.data.frame(tuesdata$ratings)
#head(ratings)

details <- as.data.frame(tuesdata$details)

#install.packages("cowplot")
library(cowplot)
library(tidyverse)
library(patchwork)
library(ggdark)
library(Hmisc)
library(extrafont)
####################
#head(details)

counts_details <- details %>% 
  group_by(yearpublished) %>% 
  count(name = "counts") %>% 
  ungroup() %>%

  mutate(counts_cat = as.factor( 
    ifelse(counts >=1 & counts <=100, "[1-100]",
           ifelse(counts >= 101 & counts <= 250, "[101-250]", 
                  ifelse(counts >= 251 & counts <=500, "[251-500]",
                         ifelse(counts >=501 & counts <=1000, "[501-1000]",
                                ifelse(counts >=1001,">1000" ,"999"))))))) %>% 
  mutate(year_cat = as.factor(
    ifelse(yearpublished >= -3500 & yearpublished <= 1500,"1",
           ifelse(yearpublished >= 1501 & yearpublished <=1880,"2",
                  ifelse(yearpublished >= 1881 & yearpublished <= 1975,"3",
                         ifelse(yearpublished >= 1976 ,"4","999")))))) 

#?add_row
counts_details$counts_cat <-factor(counts_details$counts_cat,
                                   levels = c("[1-100]",
                                              "[101-250]",
                                              "[251-500]",
                                              "[501-1000]",
                                              ">1000"))
#install.packages("data.table")
#library(data.table)
#fehlende Werte nachtragen

counts_details$show <- "show"

yearpublished <- c(1501,1880)
counts <- c(0,0)
counts_cat <- c("[1-100]","[1-100]")
year_cat <- c("2","2")
show <- c("invisible","invisible")

temp<- data.frame(yearpublished, counts,counts_cat,year_cat,show)

temp$counts_cat <- as.factor(temp$counts_cat)
temp$year_cat <- as.factor(temp$year_cat)

counts_details<- bind_rows(counts_details,temp)

counts_details <- counts_details %>% 
  mutate(yearpublished = as.factor(yearpublished))

####

#?reorder
p1 <- counts_details %>% 
  ggplot(aes(yearpublished, counts, group=1, 
             colour = counts_cat)) +
  geom_point(aes(size = counts_cat), shape = 20) +
  scale_x_discrete(breaks = c("-3500", "1500",
  "1501","1880","1881","1975","1976","2023")) +
  scale_y_continuous(limits=c(1,NA)) +
  dark_theme_gray() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_colour_manual(values=myColors) +
  labs(title = "Published board games over the centuries", 
       subtitle = "3500 B.C. -  2023 A.C.") +
  facet_wrap(~year_cat,scales = "free",ncol = 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  
  theme(
    text  = element_text(family = "Castellar"), 
    plot.title = element_text(size = 20,hjust = 0.5, family = "Castellar"),
    plot.subtitle = element_text(hjust = 0.5, family = "Castellar"),
    plot.caption = element_text(size = 14, family = "Castellar" ),
    axis.text = element_text(size = 10, family = "Candara"),
    legend.text = element_text(family = "Candara"))
 

p1

?facet_wrap()

