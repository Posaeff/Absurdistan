load("BoardGames/Data/board games")

ratings<- as.data.frame(tuesdata$ratings)
#head(ratings)

details <- as.data.frame(tuesdata$details)

#install.packages("cowplot")
install.packages("ggExtra")
library(ggExtra)
library(cowplot)
library(tidyverse)
library(patchwork)
library(ggdark)
library(Hmisc)
library(extrafont)
loadfonts(device = "win")
####################
#head(details)

counts_details <- details %>% 
  group_by(yearpublished) %>% 
  count(name = "counts") %>% 
  ungroup() %>%
  
  complete(yearpublished = -3600:2023) %>% 
  mutate_all(~replace(.,is.na(.),0)) %>% 

  mutate(counts_cat = as.factor( 
    ifelse(counts >=0 & counts <=100, "1-100",
           ifelse(counts >= 101 & counts <= 250, "101-250", 
                  ifelse(counts >= 251 & counts <=500, "251-500",
                         ifelse(counts >=501 & counts <=1000, "501-1000",
                                ifelse(counts >=1001,">1000" ,"999"))))))) %>% 
  mutate(year_cat = as.factor(
    ifelse(yearpublished >= -3600 & yearpublished <= 1400,"1",
           ifelse(yearpublished >= 1401 & yearpublished <=1880,"2",
                  ifelse(yearpublished >= 1881 & yearpublished <= 1975,"3",
                         ifelse(yearpublished >= 1976 ,"4","999")))))) 

#?add_row
counts_details$counts_cat <-factor(counts_details$counts_cat,
                                   levels = c("1-100",
                                              "101-250",
                                              "251-500",
                                              "501-1000",
                                              ">1000"))


#library(norm)
#fehlende Werte nachtragen
###### calculate perc yearpublished

quants <- counts_details %>% 
  group_by(year_cat)%>% 
  summarise_at(vars(yearpublished),
               list(min=min,
                    Q1=~quantile(.,probs = 0.25),
                    median=median,
                    Q3=~quantile(.,probs=0.75),
                    max=max))


#######fehlende Werte nachtragen
# yearpublished <- c(1501,1880,-2250,575,1188,1798,1705,1846)
# counts <- c(0,0,0,0,0,0,0,0)
# counts_cat <- c("[1-100]","[1-100]","[1-100]","[1-100]","[1-100]","[1-100]","[1-100]","[1-100]")
# year_cat <- c("2","2","1","1","1","2","2","2")
# 
# temp<- data.frame(yearpublished, counts,counts_cat,year_cat)
# 
# temp$counts_cat <- as.factor(temp$counts_cat)
# temp$year_cat <- as.factor(temp$year_cat)
# 
# counts_details<- bind_rows(counts_details,temp)

#in factor umformatieren
counts_details <- counts_details %>% 
  distinct(yearpublished, .keep_all = T)%>% 
  mutate(yearpublished = as.factor(yearpublished))

####

#?reorder
p1 <- counts_details %>% 
  ggplot(aes(yearpublished, counts, group=1, 
             colour = counts_cat)) +
  geom_point(aes(size = counts_cat), shape = 20) +
  scale_x_discrete(breaks = c("-3600","-2350","-1100","150","1400",
  "1401","1520","1640","1760","1880",
  "1881","1905","1928","1952","1975",
  "1976","1988","2000","2011","2023")) +
  scale_y_continuous(limits=c(1,NA)) +
  dark_theme_gray() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_colour_manual(values=myColors) +
  labs(title = "Published board games over the centuries", 
       subtitle = "3600 B.C. -  2023 A.C.",
       caption = "#TidyTuesday - data: board games (2022-01-25)") +
  facet_wrap(~year_cat,scales = "free",ncol = 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.caption = element_text(vjust = -2.5, hjust = 1)) +
  
  
  theme(
    text  = element_text(family = "Castellar"), 
    plot.title = element_text(size = 20,hjust = 0.5, family = "Castellar",color = "azure2"),
    plot.subtitle = element_text(hjust = 0.5, family = "Castellar",color = "azure2"),
    plot.caption = element_text(size = 8, family = "Candera",color = "azure2" ),
    axis.text = element_text(size = 8, family = "Castellar",color = "azure2"),
    legend.text = element_text(family = "Castellar",color="azure2"))
 

p1 + removeGrid(x = TRUE,y=F)

?facet_wrap()
 



  
