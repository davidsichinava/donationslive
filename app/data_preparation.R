library(arrow)
library(tidyverse)

tenders <- read_parquet("../../tenders.parquet")
donations <- read_parquet("../../donations_full.parquet")
relations <- read_rds("../../relations.rds")
individuals <- read_rds("../../individuals.rds")

individuals |> 
  select(id, idNumber, name, nationality) |>
  set_names(
    c(
      "id", "idCode", "name", "nationality"
    )
  ) -> individuals_short

tenders |> 
  select(
     ID, `შემსყიდველი`, `ბიდინგის დამთავრების დრო`, `მთავარი Cpv`, `მიმწოდებლის Id`, `მიმწოდებლის სახელი`, `საბოლოო ღირებულება`, `ვალუტა`
  ) |> 
  set_names(
    c("tender_id", "buyer", "bid_end_time", "main_cpv", "supplier_id", "supplier_name", "final_value", "currency")
  ) -> tenders_short

bind_cols(
  relations$individual,
  relations$nodes$idCode,
  relations$nodes$label
) |> 
set_names(
  "id", "idCode", "name_of_the_entity"
) -> relations_short

donations |>
  select(
    2:6
  ) |> 
  set_names(
    c(
      "date", "name", "idCode", "amount", "recipient"
    )
  ) -> donations_short

## Dataset

# Subset tenders that were won by individuals associated with donations

# All individuals from the donations list

all_ids <- donations_short |> select(name, idCode) |> distinct() |> tibble() 

# Attach person numbers from the individuals dataset, needs for merging with relations

all_ids |>
  mutate(
    personal_id = idCode
  ) |> 
  left_join(
    individuals_short |> select(id, idCode),
    by = "idCode"
  ) |>
  left_join(
    relations_short,
    by = "id"
  ) |> 
  select(
    name, personal_id, idCode.x, idCode.y
  ) |> 
  pivot_longer(
    cols = c(idCode.x, idCode.y),
    values_to = "associated_entities",
    names_to = "idType"
  ) |> 
  filter(!is.na(associated_entities)) |> 
  select(
    -idType
  ) |> 
  distinct() -> all_ids_expanded

# Merge with tenders

# Exchange rates

readxl::read_excel("../data_setup/data.xlsx") |> 
  select(
    c(1, 2, 6)
  ) |> 
  slice(4:n()) |> 
  set_names(
    c(
      "end_of_month", "usd", "eur"
    )
  ) |> 
  filter(!is.na(usd)) |> 
  type_convert() -> exchange_rates # NBG

tenders_short |>
  filter(
    supplier_id %in% all_ids_expanded$associated_entities
  ) |>
  # This yields multiple join matches, which I'm not sure how to handle, so I will just keep all of them
  left_join(
    all_ids_expanded,
    by = c("supplier_id" = "associated_entities")
  ) |> 
  mutate(
    final_value = as.numeric(final_value),
    # character: year-month
    date = substr(bid_end_time, 1, 7),
  ) |>
  left_join(
    exchange_rates,
    by = c("date" = "end_of_month")
  ) |>
  mutate(
    final_value = case_when(
      currency == "GEL" ~ final_value,
      currency == "USD" ~ final_value * usd,
      currency == "EUR" ~ final_value * eur,
      TRUE ~ NA_real_
    ),
    currency = "GEL"
  ) |>
  select(
    tender_id, buyer, bid_end_time, main_cpv, supplier_id, supplier_name, final_value, currency, name, personal_id
  ) -> tenders_donations

# write_parquet(tenders_donations, "tenders_donations.parquet")
write_rds(tenders_donations, "tenders_donations.rds")

# (donations_short |> 
#   filter(recipient != "ევროპული საქართველო - მოძრაობა თავისუფლებისთვის") |>
#   filter(recipient != "ერთიანი ნაციონალური მოძრაობა")) |> 
#   filter(recipient != "ლელო საქართველოსთვის") |> 
#   write_parquet("donations_short.parquet")
(donations_short |> 
  filter(recipient != "ევროპული საქართველო - მოძრაობა თავისუფლებისთვის") |>
  filter(recipient != "ერთიანი ნაციონალური მოძრაობა")) |> 
  filter(recipient != "ლელო საქართველოსთვის") |> 
  write_rds("donations_short.rds")

# write_parquet(individuals_short, "individuals_short.parquet")
write_rds(individuals_short, "individuals_short.rds")

