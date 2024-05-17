# Webscraping ----

# 1.0 Libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping

# 1.1 Collect product categories ----

url_home <- "https://www.radon-bikes.de/en/"
html_home <- read_html(url_home)

bike_categories_chr <- html_home %>% 
  html_elements(css = ".megamenu__item a") %>% 
  html_attr('href') %>% 
  str_subset("wear", negate = TRUE) |>
  str_c("https://www.radon-bikes.de",  ... = _) %>% 
  paste("bikegrid/", sep = "")
  

# 2.0 Collect bike data ----

bike_category_url <- bike_categories_chr[2]
html_bike_category <- bike_category_url %>% 
  read_html()

bike_url_chr <- html_bike_category %>% 
  html_elements(css = ".m-bikegrid__info a") %>% 
  html_attr('href') |>
  str_c("https://www.radon-bikes.de",  ... = _)
  

# 2.1 Make a function ----

get_bike_urls <- function(url) {
  
  html_bike_category <- read_html(url)
  
  bike_url_chr <- html_bike_category %>% 
    html_elements(css = ".m-bikegrid__info a") %>% 
    html_attr('href') |>
    str_c("https://www.radon-bikes.de",  ... = _)
  
  return(bike_url_chr)
}

# Get all bike urls
bike_urls_chr <- map(bike_categories_chr,get_bike_urls)

# 3.1 Subsetting ----

# Unnest the mess into one column
bike_urls_tbl <- bike_urls_chr %>% 
  as_tibble_col(column_name = "url") %>% 
  unnest(cols = c("url")) %>% 
  unique() %>% 
  tidyr::separate_wider_regex(cols = url, patterns = c(".*de/en/", family   = "[^/]*", "/",
                                                       category = "[^/]*", "/",
                                                       model    = "[^/]*", "/",
                                                       ".*"), cols_remove = F)

# Select one category to analyze
bike_urls_fullsuspension_tbl <- bike_urls_tbl %>% 
  filter(category == "fullsuspension")

# 3.2 Get different data for 1 bike ----

html_bike_model <- read_html(bike_urls_fullsuspension_tbl$url[1])

bike_model <- html_bike_model %>% 
  html_elements(css = ".m-bikedetail__overlays-top .a-heading--medium") %>% 
  html_text() %>% 
  str_squish()

bike_price <- html_bike_model %>% 
  html_elements(css = ".m-bikedetail__price--active") %>% 
  html_text() %>% 
  .[1] %>% 
  parse_number() %>% 
  round()

bike_weight <- html_bike_model %>% 
  html_elements(css = ".spec-42 .m-feature-list__description") %>% 
  html_text() %>% 
  str_extract("[0-9]*[.,]*[0-9]*")  %>% 
  str_replace("\\,","\\.") %>% 
  as.numeric()

# 3.3 Make a function ----

get_model_data <- function(url) {
  
  html_bike_model <- read_html(url)
  
  bike_model <- html_bike_model %>% 
    html_elements(css = ".m-bikedetail__overlays-top .a-heading--medium") %>% 
    html_text() %>% 
    str_squish()
  
  bike_price <- html_bike_model %>% 
    html_elements(css = ".m-bikedetail__price--active") %>% 
    html_text() %>% 
    .[1] %>% 
    parse_number() %>% 
    round()
  
  bike_weight <- html_bike_model %>% 
    html_elements(css = ".spec-42 .m-feature-list__description") %>% 
    html_text() %>% 
    str_extract("[0-9]*[.,]*[0-9]*")  %>% 
    str_replace("\\,","\\.") %>% 
    as.numeric()
  
  bike_data <- tibble(url   = url,
                      model = bike_model,
                      price = bike_price,
                      weight = bike_weight)
  
  return(bike_data)
}

# 3.4 Test functions ----

# For one bike
# bike_model_data_tbl <- get_model_data(url = bike_urls_fullsuspension_tbl$url[1])

# For one category
bike_model_data_tbl <- bike_urls_fullsuspension_tbl$url %>% map_dfr(get_model_data)

# For ALL categories
# bike_model_data_tbl <- bike_urls_tbl$url %>% map_dfr(get_model_data)

# 3.5 Join Data ----

# For one category
bike_model_data_joined_tbl <- bike_urls_fullsuspension_tbl %>% 
  left_join(bike_model_data_tbl, by = join_by("url"))

# For ALL categories
# bike_model_data_joined_tbl <- bike_urls_tbl %>% 
#  left_join(bike_model_data_tbl, by = join_by("url"))

# 4 Print for category "fullsuspension"

bike_model_data_joined_tbl %>% 
  head(n = 10)

# A tibble: 10 × 7
#   family       category       model.x        url                        model.y price weight
#   <chr>        <chr>          <chr>          <chr>                      <chr>   <dbl>  <dbl>
#1  mountainbike fullsuspension skeen-trail-al https://www.radon-bikes.d… SKEEN …  1428   14.4
#2  mountainbike fullsuspension skeen-trail-al https://www.radon-bikes.d… SKEEN …  1680   14.4
#3  mountainbike fullsuspension skeen-trail    https://www.radon-bikes.d… SKEEN …  1848   13.4
#4  mountainbike fullsuspension skeen-trail    https://www.radon-bikes.d… SKEEN …  2184   13.2
#5  mountainbike fullsuspension skeen-trail    https://www.radon-bikes.d… SKEEN …  2688   13.8
#6  mountainbike fullsuspension slide-trail-al https://www.radon-bikes.d… SLIDE …  1848   14.9
#7  mountainbike fullsuspension slide-trail-al https://www.radon-bikes.d… SLIDE …  2268   15.0
#8  mountainbike fullsuspension slide-trail-al https://www.radon-bikes.d… SLIDE …  1512   14.7
#9  mountainbike fullsuspension slide-trail-al https://www.radon-bikes.d… SLIDE …  1764   15.1
#10 mountainbike fullsuspension slide-trail    https://www.radon-bikes.d… SLIDE …  4033   14.0