library(tidyverse)
library(patchwork)
####################
head(details)

counts_details <- details %>% 
  group_by(yearpublished) %>% 
  count(name = "counts")

table(counts_details$counts)

df <- details %>% 
  select(primary, yearpublished) %>% 
  filter(yearpublished > -3500 & yearpublished <= 1900) %>% 
  mutate(yearpublished = as.factor(yearpublished)) %>% 
  group_by(yearpublished) %>% 
count(name = "counts") %>% 
  ungroup() %>% 
mutate(counts = as.numeric(counts)) %>% 
mutate(counts_cat = case_when(
  counts >=1 ~ "counts_1-10",
  counts == 184 ~ "counts_11-rest"))

str(df)

df2 <- details %>% 
  select(primary, yearpublished) %>% 
  filter(yearpublished >= 1901 & yearpublished <= 1945) %>% 
  mutate(yearpublished = as.factor(yearpublished)) %>% 
  group_by(yearpublished) %>% 
  count(name = "counts") %>% 




p1 <- df %>% 
  ggplot(aes(yearpublished, counts, group=1)) +
  geom_point(size=df$counts) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p2 <- df2 %>% 
  ggplot(aes(yearpublished, counts, group=1)) +
  geom_point(size = df2$counts) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


