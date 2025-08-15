library(tidyverse)
library(httr)
library(jsonlite)
library(tibble)

### ჩამოვტვირთოთ companyinfo.ge-ზე განთავსებული ყველა ინდივიდის მონაცემები, პირადი ნომრის კოდთან დასაკავშირებლად

individuals_url <- "https://api.companyinfo.ge/api/people/search?name=&idNumber=&address=&nationality=&page="

pages <- c(1:16866)

# Function to fetch company data from the API

get_individual_list_url <- function(
  url = individuals_url,
  page,
  timeout_s   = 15,
  retry_times = 5,
  retry_pause = 0.2
) {
  res <- RETRY(
    "GET", paste0(url, page),
    times = retry_times,
    pause_base = retry_pause,
    accept_json(),
    timeout(timeout_s)
  )
  stop_for_status(res)

  txt <- content(res, as = "text", encoding = "UTF-8")

  parsed <- tryCatch(fromJSON(txt, simplifyVector = TRUE),
                     error = function(e) stop("JSON parse failed: ", e$message))
  # If payload includes $items, return that; otherwise return the whole thing as a tibble.
  out <- parsed$items %||% parsed
  as_tibble(out)
}

# quietly continue on errors, add a polite delay, and keep a progress bar

safe_get_individual <- purrr::possibly(get_individual_list_url, otherwise = tibble(), quiet = FALSE)

individuals <- map_dfr(pages, ~{
  df <- safe_get_individual(page = .x)
  # Sys.sleep(0.25)             # throttle a bit
  if (nrow(df)) mutate(df, page = .x) else df
}, .progress = TRUE)

# Save the data to an RDS file

individuals |> 
  write_rds("individuals.rds")

# read_rds("../individuals.rds") -> individuals

# ინდივიდებთან დაკავშირებული კომპანიების მონაცემები

# donations <- readxl::read_excel("donattions_full")
donations <- readxl::read_excel("../donations_full.xls")

donations |> 
  mutate(
    # first four digits of თარიღი
    year = substr(თარიღი, 1, 4)
  ) |> 
  type_convert() |> 
  filter(year > 2012) |> 
  select(
    c(3, 4, 5, 6, 8)
  ) |>
  set_names(
    c(
      "name", "id", "amount", "party", "year"
    )
  ) -> donations_cleaned

donations_cleaned |> 
  filter(
# ოთხი ძირითდი პარტია
#     str_detect(party, "კოალიცია ცვლილებისთვის|ნაციონალური მოძრაობა|ევროპული საქართველო|ლელო|გახარია")
  ) |> 
  group_by(
    id, party
  ) |> 
  count() -> id_parties

# companyinfo.ge-ზე განთავსებული ინდივიდების მონაცემები
individuals |> 
  filter(
    idNumber %in% unique(id_parties$id) # ამოვიღოთ მხოლოდ ის ინდივიდები, რომლებიც ოპოზიციის შემომწირველები არიან
  ) |>
  select(id) -> individuals_filtered



# ჯერ წავიკითხოთ პოლიტიკური შემოწირულობების მონაცემები, რათა ამოვიღოთ ოპოზიციის შემომწირველების ნომრები


rel_url <- "https://api.companyinfo.ge/api/relationship-graph?person="

get_rel_url_list <- function(
    url = rel_url,
    individual,
    timeout_s   = 15,
    retry_times = 5,
    retry_pause = 0.2
) {
  res <- RETRY(
    "GET", paste0(url, individual, "&level=1"),
    times = retry_times,
    pause_base = retry_pause,
    accept_json(),
    timeout(timeout_s)
  )
  stop_for_status(res)
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  
  parsed <- tryCatch(fromJSON(txt, simplifyVector = TRUE)|> as_tibble() |> mutate(individual := individual),
                     error = function(e) stop("JSON parse failed: ", e$message))
  # If payload includes $items, return that; otherwise return the whole thing as a tibble.
  out <- parsed %||% parsed
  as_tibble(out)
}

# quietly continue on errors, add a polite delay, and keep a progress bar

safe_get_individual <- purrr::possibly(get_rel_url_list, otherwise = tibble(), quiet = FALSE)

individual_all <- individuals_filtered$id |> unname() |> unlist()

relations <- map_dfr(individual_all, ~{
  df <- safe_get_individual(individual = .x)
  # Sys.sleep(0.25)             # throttle a bit
  if (nrow(df)) mutate(df, individual = .x) else df
}, .progress = TRUE)


write_rds(relations, "relations.rds")
