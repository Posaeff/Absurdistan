load("board games")

ratings<- as.data.frame(tuesdata$ratings)
head(ratings)

details <- as.data.frame(tuesdata$details)

#install.packages("cowplot")
library(cowplot)
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
    ifelse(counts >=1 & counts <=100, "[1-100]",
           ifelse(counts >= 101 & counts <= 250, "[101-250]", 
                  ifelse(counts >= 251 & counts <=500, "[251-500]",
                         ifelse(counts >=501 & counts <=1000, "[501-1000]",
                                ifelse(counts >=1001,">1000" ,"999")))))))

counts_details$counts_cat <-factor(counts_details$counts_cat,
                                   levels = c("[1-100]",
                                              "[101-250]",
                                              "[251-500]",
                                              "[501-1000]",
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


p3 <-df3 %>% 
  ggplot(aes(yearpublished, counts, group=1,
             colour = counts_cat)) +
  geom_point(aes(size = df4$counts_cat)) +
  scale_x_discrete(breaks = c(-3500, 0, 1000, 1500, 1750, 1850, 1885,
    1900, 1915, 1930, 1940, 1950, 1960, 1970, 1975))+
  scale_y_continuous(breaks = seq(from= 0, to= 200 , by= 25)) +
  scale_colour_manual(values = myColors) +
  labs(y="n = published games")


p4 <- df4 %>% 
  ggplot(aes(yearpublished, counts, group=1, 
             color = counts_cat)) +
  geom_point(aes(size = counts_cat)) +
  scale_colour_manual(values = myColors) +
  scale_y_continuous(breaks = seq(from= 0, to= 1500 , by= 250)) +
  labs(y="n = published games")

p3<- p3 + dark_theme_gray() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        legend.position = "none")

p4 <-p4 + dark_theme_gray() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
axis.title.x = element_blank(),
legend.title = element_blank(),
legend.position = "none"


)

legend <- get_legend(p4)

p_0L <- plot_grid(p3 + theme(axis.title.y = element_blank()), 
                p4 + theme(axis.title.y = element_blank()),
                
                   ncol = 1,
                   labels = "auto")

p_0L


p <- plot_grid(p_0L, legend, ncol = 1)

p 
p3/p4
