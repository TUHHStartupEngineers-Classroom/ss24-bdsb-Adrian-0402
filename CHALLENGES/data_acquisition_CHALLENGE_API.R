#Spotify API

library(spotifyr) #install via devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(lubridate)

# can't use get_spotify_access_token() since function will prompt to website with "Illegal scope"
# only way to bypass this broken API is to avoid certain scopes
auth_object <- get_spotify_authorization_code(scope = scopes()[c(7,8,9,10,14,15)])


top_tracks_tbl <- get_my_top_artists_or_tracks(
  type = "tracks",
  limit = 20,
  offset = 0,
  time_range = "medium_term",
  authorization = auth_object,
  include_meta_info = FALSE
) %>% as_tibble()


top_tracks_wrangled_tbl <- top_tracks_tbl %>%
  mutate(duration_min = duration((duration_ms / 1000),"seconds")) %>% 
  select(artists,duration_min,name,popularity) %>% 
  rename(track_name = name) %>%
  unnest(artists) %>% 
  rename(artists = name) %>%
  select(artists,duration_min,track_name,popularity) %>%
  group_by(duration_min,track_name,popularity) %>% 
  summarize(artists = paste(unique(artists), collapse = ", ")) %>%
  select(artists,track_name,duration_min,popularity) %>% 
  arrange(desc(popularity))

top_tracks_wrangled_tbl %>% 
  head(n = 10)


# A tibble: 10 × 4
# Groups:   duration_min, track_name [10]
#  artists                      track_name         duration_min             popularity
#  <chr>                        <chr>              <Duration>                    <int>
#1 Kenya Grace                  Strangers          172.964s (~2.88 minutes)         88
#2 Dynoro, Gigi D'Agostino      In My Mind         184.56s (~3.08 minutes)          80
#3 Vicetone                     Walk Thru Fire     194.482s (~3.24 minutes)         66
#4 deadmau5, Rob Swire          Ghosts 'n' Stuff   328.253s (~5.47 minutes)         56
#5 Vini Vici, Ranji, Halflives  Everyday Rockstars 209.375s (~3.49 minutes)         41
#6 Aether, Sizzle Bird, Veela   Raccoon City       226.925s (~3.78 minutes)         38
#7 Bensley, Skyelle             All I Wanted       244.137s (~4.07 minutes)         35
#8 Draper, Laura Brehm          Pressure           318.537s (~5.31 minutes)         35
#9 Tut Tut Child, Danyka Nadeau Breathe            275.121s (~4.59 minutes)         31
#10 Nomyn, Veela                Be Honest          223.655s (~3.73 minutes)         30


  
  


