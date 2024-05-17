# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)
library(readxl)

# 2.0 Importing Files ----

bikes_tbl <- read_excel("~/GitHub/ss24-bdsb-Adrian-0402/content/01_journal/01_tidyverse_files/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("~/GitHub/ss24-bdsb-Adrian-0402/content/01_journal/01_tidyverse_files/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("~/GitHub/ss24-bdsb-Adrian-0402/content/01_journal/01_tidyverse_files/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----

glimpse(bikes_tbl)
glimpse(orderlines_tbl)

# 4.0 Joining Data ----

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate( col = location,
            into = c("city","state"),
            sep = ", ") %>% 
  mutate(total.price = price * quantity) %>% 
  select(-...1, -gender) %>%
  rename(bikeshop = name) %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----

library(lubridate)

# 6.1 Sales by State ----

# Step 1 - Manipulate

sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(state, total_price) %>% 
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize

sales_by_state_tbl %>%
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text)) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title    = "Revenue by state",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

library(lubridate)

# Step 1 - Manipulate

sales_by_state_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state, order_date, total_price) %>% 
  mutate(year = year(order_date)) %>% 
  group_by(year, state) %>% 
  summarize(sales = sum(total_price)) %>% 
  ungroup() %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize

sales_by_state_year_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    scale = 0.000001,
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = "m €")) +
  labs(
    title = "Revenue by year and state",
    fill = "States" # Changes the legend name
  )

# 7.0 Writing Files ----

library(writexl)

# 7.1 Excel ----

bike_orderlines_wrangled_tbl %>%
  write_xlsx("~/GitHub/ss24-bdsb-Adrian-0402/content/01_journal/01_tidyverse_files/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----

bike_orderlines_wrangled_tbl %>%
  write_csv("~/GitHub/ss24-bdsb-Adrian-0402/content/01_journal/01_tidyverse_files/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>%
  write_rds("~/GitHub/ss24-bdsb-Adrian-0402/content/01_journal/01_tidyverse_files/01_bike_sales/02_wrangled_data/bike_orderlines.rds")