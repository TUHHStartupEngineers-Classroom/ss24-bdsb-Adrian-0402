# WEBSCRAPING ----

# 1.0 LIBRARIES ----
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs

# 1.2 COLLECT PRODUCT CATEGORIES ----

url_home <- "https://www.canyon.com/en-de/"
#xopen(url_home)

html_home <- read_html(url_home)

bike_categories_chr <- html_home %>% 
  html_elements(css = ".header__navBarPreloadItem--level2") %>% 
  html_attr('href') %>% 
  str_subset("sale|outlet|gear|customer-service",negate = TRUE) |> 
  str_c("https://www.canyon.com", ... = _)

# 2.0 COLLECT BIKE DATA ----

bike_category_url <- bike_categories_chr[1]
html_bike_category <- bike_category_url %>% 
  read_html()

bike_url_chr <- html_bike_category %>% 
  html_elements(css = ".productTileDefault__productName") %>% 
  html_attr('href') %>% 
  str_remove(pattern = "\\?.*")

# 2.2 Wrap it into a function ----

get_bike_urls <- function(url) {
  
  html_bike_category <- read_html(url)
  
  bike_url_chr <- html_bike_category %>% 
    html_elements(css = ".productTileDefault__productName") %>% 
    html_attr('href') %>% 
    str_remove(pattern = "\\?.*")
  
  return(bike_url_chr)
}

bike_urls_chr <- map(bike_categories_chr,get_bike_urls) %>% 
  flatten_chr() %>% 
  unique()

# 3.1 Subsetting ----

bike_urls_tbl <- bike_urls_chr %>% 
  
  tibble::as_tibble_col(column_name = "url") %>% 
  tidyr::separate_wider_regex(cols = url, patterns = c(".*en-de/", family   = "[^/]*", "/",
                                                       category = "[^/]*", "/",
                                                       model    = "[^/]*", "/",
                                                       material = "[^/]*",
                                                       ".*"), cols_remove = F)

bike_urls_endurace_tbl <- bike_urls_tbl |>
  filter(model == "endurace")

# 3.2 Get name and prices for 1 bike ----

html_bike_model <- read_html(bike_urls_endurace_tbl$url[1])

bike_model <- html_bike_model %>% 
  html_elements(css = ".xlt-pdpName") %>% 
  html_text() %>% 
  str_squish()
  
bike_price <- html_bike_model %>% 
  html_elements(css = ".productDescription__priceSale") %>% 
  html_text() %>% 
  parse_number() %>% 
  str_remove("\\.")

# 3.3 Make a function ----

get_model_data <- function(url) {
  
  html_bike_model <- read_html(url)
  
  bike_model <- html_bike_model %>% 
    html_elements(css = ".xlt-pdpName") %>% 
    html_text() %>% 
    str_squish()
  
  bike_price <- html_bike_model %>% 
    html_elements(css = ".productDescription__priceSale") %>% 
    html_text() %>% 
    parse_number() %>% 
    str_remove("\\.")
  
  bike_data <- tibble(url   = url,
                      model = bike_model,
                      price = bike_price)
  
  return(bike_data)
}

# 3.4 Test functions ----

bike_model_data_tbl <- get_model_data(url = bike_urls_endurace_tbl$url[1])

bike_model_data_tbl <- bike_urls_endurace_tbl$url %>% map_dfr(get_model_data)

bike_model_data_joined_tbl <- bike_urls_endurace_tbl %>% 
  left_join(bike_model_data_tbl, by = join_by("url"))