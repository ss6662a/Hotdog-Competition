
library(tidyverse)
library(readxl)


dog <- read_excel("data/hotdog_tracker.xlsx") %>% 
  janitor::clean_names()



dog %>% 
  filter(person == "Ike") %>% 
  mutate(cum = cumsum(number_of_dogs)) %>% 
  ggplot(aes(x = date, y = cum)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Number of cummulative hot dogs",
    title = "Ike's 2026 Hot Dogs (so far)"
  )






