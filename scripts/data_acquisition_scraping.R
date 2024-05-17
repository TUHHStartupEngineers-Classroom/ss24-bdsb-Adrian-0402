library(rvest)
library(stringr)
library(tidyverse)
library(jsonlite)
library(purrr)

# get the URL for the wikipedia page with all S&P 500 symbols
url1 <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest

sp_500 <- url1 %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_elements(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>%
  .[[1]] %>% 
  as_tibble()

url2  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url2 %>% 
  read_html()

rank <- html %>%   
  html_elements(css = ".cli-title .ipc-title__text") %>%  
  html_text() %>%   
  parse_number()

title <- html %>% 
  html_elements(css = ".cli-title .ipc-title__text") %>%  
  html_text() %>% 
  str_remove("^\\d*\\. ") # Remove numbers, dot and space at the beginning

rating <- html %>% 
  html_elements(css = ".ratingGroup--imdb-rating") %>% 
  html_text() %>% 
  str_extract("^\\d\\.\\d") %>% 
  as.numeric()

imbd_tbl <- tibble(rank,title,rating)


bike_data_lst <- fromJSON("~/GitHub/ss24-bdsb-Adrian-0402/source_data/bike_data.json")

bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")
## "Stealth"     "aero silver"




  