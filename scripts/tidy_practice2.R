library(ggplot2) # To load the diamonds dataset
library(dplyr)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(1:2)

diamonds %>% 
  arrange(cut, carat, desc(price))

diamonds %>% 
  select(color, clarity, x:z) %>% 
  head(n = 5)

diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

diamonds %>% 
  select(x:z, everything()) %>% 
  head(n = 5)

diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head

diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)

diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))

library(lubridate)
mdy("4/3/2022")

bday <- dmy("14/10/1979")
month(bday)
year_test = bday %>% year 
