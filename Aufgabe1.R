library(tidyverse)
library(patchwork)
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



table(counts_details$counts_cat)

df <- counts_details %>% 
  select(yearpublished, counts, counts_cat) %>% 
  filter(yearpublished > -3500 & yearpublished <= 1900) %>% 
  mutate(yearpublished = as.factor(yearpublished)) %>% 
  mutate(counts = as.numeric(counts))



str(df)

df2 <- counts_details %>% 
  select(yearpublished, counts, counts_cat) %>% 
  filter(yearpublished >= 1901 & yearpublished <= 1945) %>% 
  mutate(yearpublished = as.factor(yearpublished)) %>%
  mutate(counts = as.numeric(counts))
  






p1 <-df %>% 
  ggplot(aes(yearpublished, counts, group=1,
             color = counts_cat)) +
  geom_point(aes(size = counts_cat)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p2 <- df2 %>% 
  ggplot(aes(yearpublished, counts, group=1, 
             color = counts_cat)) +
  geom_point(aes(size = counts_cat)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p1/p2
