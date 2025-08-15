library(tidyverse)
library(arrow)

donations <- readxl::read_excel("donations_full.xls")

donations |> write_parquet("donations_full.parquet")

tenders <- readxl::read_excel("main_all.xlsx")

tenders |> 
  write_parquet("tenders.parquet")

