library(readr)
library(dplyr)

files <- list.files(pattern = "*.csv", path = "../data/3-avaliacao-humana/raw/")
dff <- lapply(files, read_csv) %>% bind_rows()
