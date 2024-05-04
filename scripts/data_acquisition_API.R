library(RSQLite)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "content/01_journal/02_data_acquisition_files/02_chinook/Chinook_Sqlite.sqlite")
dbListTables(con)
tbl(con, "Album")
album_tbl <- tbl(con, "Album") %>% collect()
dbDisconnect(con)
con

library(glue)
name <- "Fred"
glue('My name is {name}.')

library(httr)
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
rawToChar(resp$content)

resp %>% .$content %>% rawToChar() %>% fromJSON()


