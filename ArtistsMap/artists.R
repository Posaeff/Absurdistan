artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')


artist_eu <- artists %>%
  filter(artist_nationality %in% c("Austrian", "Austrian-American", "Belgian", "British", "Czech", "Danish-American", "Danish-French", "Dutch", "Dutch-American",
         "French", "German", "German-American", "German-French", "Hungarian", "Hungarian-American", "Hungarian-French", "Italian", 
         "Italian-American", "Latvian", "Russian", "Russian-French", "Scottish", "Norwegian", "Spanish", "Swedish", "Swiss", "Swiss-French", "Swiss-German")) %>% 
  distinct(artist_name, .keep_all = T) %>% 
  select(artist_name, artist_nationality)




