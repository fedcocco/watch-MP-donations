get_company_details <- function (number) {
    message(str_glue("Getting details for company ", number, "..."))
    str_glue("https://api.company-information.service.gov.uk/company/", str_pad(str_replace_all(number, "[^A-Za-z0-9]", ""), 8, pad = "0")) |>
        slowly(GET)(authenticate(Sys.getenv("COMPANIES_HOUSE_API_KEY"), "")) |>
        content() |>
        unlist() |>
        as_tibble_row(.name_repair = ~ vec_as_names(..., repair = "unique", quiet = TRUE)) |>
        rowwise() |>
        mutate(status = ifelse("company_status" %in% names(across()), company_status, NA_character_)) |>
        select(status)
}

donations_from_inactive <- donations_new |>
    mutate(map_df(company_registration_number, get_company_details)) |>
    filter(status != "active")

donations_from_inactive |>
    mutate(url = str_glue("http://search.electoralcommission.org.uk/English/Donations/", ec_ref)) |>
    select(donor_name, status, regulated_entity_name, value, donation_type, accepted_date, url) |>
    rowwise() |>
    mutate(payload = across() |> unbox() |> toJSON()) |>
    with(walk(payload, alert))
