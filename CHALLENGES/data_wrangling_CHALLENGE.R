# 1.0 LIBRARIES ----

# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# 2.0 DATA IMPORT ----
# Data is from April 22, 2024
# Import files used:
# g_assignee_disambiguated: Disambiguated assignee data for granted patents.
# g_patent: Data on granted patents.
# g_uspc_at_issue: USPC classification data for all granted patents at issue.

# 2.1 Assignee Data ----

col_types_1 <- list(
  patent_id = col_character(),
  assignee_sequence = col_skip(),
  assignee_id = col_character(),
  disambig_assignee_individual_name_first = col_character(),
  disambig_assignee_individual_name_last = col_character(),
  disambig_assignee_organization = col_character(),
  assignee_type = col_skip(),
  location_id = col_skip()
)

assignee_data <- vroom(
  file       = "~/GitHub/ss24-bdsb-Adrian-0402/source_data/wrangling_data/full_data/g_assignee_disambiguated.tsv", 
  delim      = "\t", 
  col_types  = col_types_1,
  na         = c("", "NA", "NULL")
)

# 2.2 Patent Data ----

col_types_2 <- list(
  patent_id = col_character(),
  patent_type = col_skip(),
  patent_date = col_date("%Y-%m-%d"),
  patent_title = col_character(),
  patent_abstract = col_skip(),
  wipo_kind = col_skip(),
  num_claims = col_skip(),
  withdrawn = col_skip(),
  filename = col_skip()
)

patent_data <- vroom(
  file       = "~/GitHub/ss24-bdsb-Adrian-0402/source_data/wrangling_data/full_data/g_patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_2,
  na         = c("", "NA", "NULL")
)

# 2.3 USPC Data ----

col_types_3 <- list(
  patent_id = col_character(),
  uspc_sequence = col_double(),
  uspc_mainclass_id = col_character(),
  uspc_mainclass_title = col_character(),
  uspc_subclass_id = col_skip(),    #ignore since we only look at mainclasses
  uspc_subclass_title = col_skip()
)

uspc_data <- vroom(
  file       = "~/GitHub/ss24-bdsb-Adrian-0402/source_data/wrangling_data/full_data/g_uspc_at_issue.tsv", 
  delim      = "\t", 
  col_types  = col_types_3,
  na         = c("", "NA", "NULL")
)

# 3.0 TRANSFORM DATA ----

setDT(assignee_data)
setDT(patent_data)
setDT(uspc_data)

# 4.0 DATA WRANGLING ----

# 4.1 Joining / Merging Data ----

combined_patent_data <- merge(assignee_data,patent_data,
                              by = "patent_id",
                              all.x = TRUE,
                              all.y = FALSE)
# free space
rm(patent_data)

combined_uspc_data <- merge(assignee_data,uspc_data,
                            by = "patent_id",
                            all.x = TRUE,
                            all.y = FALSE)
# Remove uncountable rows and free space
combined_uspc_data <- combined_uspc_data[!is.na(uspc_mainclass_id)]
rm(uspc_data)

# 4.2 Set keys

setkey(assignee_data, "assignee_id")
setkey(combined_patent_data, "assignee_id")
setkey(combined_uspc_data, "uspc_mainclass_id")

# 5.0 QUESTIONS ----

# 5.1 Question 1: Patent Dominance: What US company / corporation has the most patents? 
#     List the 10 US companies with the most assigned/granted patents

question_1_dt <- assignee_data[, .N , by = .(assignee_id,
                                          disambig_assignee_individual_name_first,
                                          disambig_assignee_individual_name_last,
                                          disambig_assignee_organization)][order(-N)] %>% 
  setnames("disambig_assignee_individual_name_first","first_name") %>% 
  setnames("disambig_assignee_individual_name_last","last_name") %>% 
  setnames("disambig_assignee_organization","company") %>% 
  head(n = 10)

# 5.2 Question 2: Recent patent activity: What US company had the most patents
#     granted in 2019? List the top 10 companies with the most new granted 
#     patents for 2019.

question_2_dt <- 
  combined_patent_data[year(patent_date) == 2019, .N, by = .(assignee_id,
                                                             disambig_assignee_individual_name_first,
                                                             disambig_assignee_individual_name_last,
                                                             disambig_assignee_organization)][order(-N)] %>% 
  setnames("disambig_assignee_individual_name_first","first_name") %>% 
  setnames("disambig_assignee_individual_name_last","last_name") %>% 
  setnames("disambig_assignee_organization","company") %>% 
  head(n = 10)

# 5.3 Question 3: Innovation in Tech: What is the most innovative tech sector? 
#     For the top 10 companies (worldwide) with the most patents, 
#     what are the top 5 USPTO tech main classes?

# Note top 10 companies first
compare_company = question_1_dt$assignee_id

# Subset all the patents for only the top 10 companies
# Allow counting multiple uspc mainclasses for a patent, since subclasses would differ
question_3_dt <- 
  combined_uspc_data[assignee_id %in% compare_company, .N, by = uspc_mainclass_id][order(-N)] %>% 
  head(n = 5)

# 6 PRINT SOLUTIONS

# IBM has the most patents!
question_1_dt

# Samsung had the most newly granted patents in 2019!
question_2_dt

# Top 5 main classes : 257, 438, 370, 430, 365 (considering only the top 10)
question_3_dt