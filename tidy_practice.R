diamonds2 %>% head(n = 5) # head(diamonds2, n = 5)
diamonds2 %>% pivot_longer(
    cols = c("2008","2009"),
    names_to = "year",
    values_to = "price",
) %>% head(n = 5)   # head(pivot_longer(diamonds2))

diamonds3 <- readRDS("diamonds3.rds")
diamonds3 %>% pivot_wider(
  names_from = "dimension",
  values_from = "measurement"
) %>% head (n = 5)

diamonds4 <- readRDS("diamonds4.rds")
diamonds4 %>% separate(
  col = "dim",
  into = c("x","y","z"),
  sep = "/",
  convert = TRUE
)

diamonds5 <- readRDS("diamonds5.rds")
diamonds5 %>% unite(
  col = clarity,
  c(clarity_prefix,clarity_suffix),
  sep = ""
)

