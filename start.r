library(dotenv)
library(tidyverse)
library(janitor)
library(lubridate)
library(httr)
library(jsonlite)
library(vctrs)

alert <- function (payload) {
    POST(Sys.getenv("SLACK_WEBHOOK_URL"), body = payload)
}

donations_previously <- read_csv("donations.csv")

donations <- GET("http://search.electoralcommission.org.uk/api/csv/Donations") |>
    content(col_types = cols(.default = "c")) |>
    clean_names()

donations_new <- donations |>
    filter(!is.na(company_registration_number)) |>
    filter(!(ec_ref %in% donations_previously$ec_ref))

if (nrow(donations_new) == 0) {
    message("No new donations found!")
    quit()
}

source("check-insolvency.r")

donations |>
    select(ec_ref) |>
    write_csv("donations.csv")
