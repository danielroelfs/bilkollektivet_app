### BILKOLLEKTIVET COST BREAKDOWN (SERVER) ########################

#-- Libraries -------------------------

library(tidyverse)
library(rvest)
library(DBI)

#-- Functions -------------------------

scrape_prices <- function() {
  #' Scrape the prices from bilkollektivet

  raw_list <- "https://bilkollektivet.no/priser/" |>
    read_html() |>
    html_nodes("div.fl-col") |>
    html_text() |>
    str_remove_all("\\n|\\t") |>
    `[`(x = _, j = seq(36, 49)) |>
    as_tibble()

  get_unparsed <- raw_list |>
    filter(str_detect(value, "Toyota Proace Verso9")) |>
    mutate(value = str_extract(value, "(Toyota Proace Verso9).*"))

  pricelist <- raw_list |>
    bind_rows(get_unparsed) |>
    separate(
      col = value,
      into = c("cartype", "prices"),
      sep = "Timepris:"
    ) |>
    mutate(
      cartype = str_trim(cartype),
      cartype = str_replace_all(cartype, "SUV", "Suv")
    ) |>
    filter(nchar(cartype) > 0) |>
    separate(
      col = cartype,
      into = c("carname", "category"),
      sep = "(?<=[a-z|0-9|L|Y|S])(?=[A-Z|9|E])"
    ) |>
    separate(
      col = prices,
      into = c("price_hour", "price_day", "price_week", "price_km"),
      sep = ":"
    ) |>
    mutate(
      across(everything(), str_trim),
      across(everything(), ~ str_replace_all(.x, "Suv", "SUV")),
      price_week = str_remove_all(price_week, "1 uke:"),
      across(
        price_hour:last_col(),
        ~ parse_number(.x, locale = locale(
          decimal_mark = ",",
          grouping_mark = " "
        ))
      ),
      car = ifelse(
        str_detect(category, "Budsjett"),
        yes = str_glue("{carname} ({category})"),
        no = carname
      )
    )

  return(pricelist)
}

image_links <- function() {
  #' Scrape car with links to the images

  carlabels <- tribble(
    ~car, ~linkname,
    "Toyota Yaris (Budsjettklasse)", "https://bilkollektivet.no/content/uploads/2022/08/Yaris_600x250.png",
    "Opel Corsa-e", "https://bilkollektivet.no/content/uploads/2021/05/Opel-e-Corsa.png",
    "Toyota Yaris", "https://bilkollektivet.no/content/uploads/2022/08/Yaris22_600x250.png",
    "Mazda MX5", "https://bilkollektivet.no/content/uploads/2019/09/MX5-250x600-1-768x335.png",
    "Toyota Yaris Cross", "https://bilkollektivet.no/content/uploads/2022/08/BK_YarisCross22.png",
    "MG ZS", "https://bilkollektivet.no/content/uploads/2022/12/MG-ZS-side-1-768x293.png",
    "Toyota Corolla", "https://bilkollektivet.no/content/uploads/2019/09/Corolla_STV_600x250.png",
    "Tesla Model 3", "https://bilkollektivet.no/content/uploads/2019/09/Tesla3-250x600.png",
    "Tesla Model Y", "https://bilkollektivet.no/content/uploads/2021/08/TeslaY-250x600-1.png",
    "Corolla Cross", "https://bilkollektivet.no/content/uploads/2022/12/CorollaCross-side_takboks-768x432.jpg",
    "Toyota Rav4", "https://bilkollektivet.no/content/uploads/2019/09/Toyota-Rav4_250x600.png",
    "Toyota Proace EL", "https://bilkollektivet.no/content/uploads/2019/09/Toyota-Proace-L2_695x250.png",
    "Toyota Proace", "https://bilkollektivet.no/content/uploads/2019/09/Toyota-Proace-L2_695x250.png",
    "Toyota Proace Verso", "https://bilkollektivet.no/content/uploads/2019/09/Proace-verso_600x250.png",
  )

  return(carlabels)
}

subscription_info <- function() {
  #' Get the data associated with different subscriptions
  data_sub <- tribble(
    ~type, ~cost, ~start_price, ~time_discount,
    "Klikk og kjør", 0, 35, 0,
    "Privat", 149, 0, 0.05,
    "Privat+", 1250, 0, 0.4
  )

  return(data_sub)
}

create_db_connection <- function() {
  #' Create a database connection

  conn <- dbConnect(RSQLite::SQLite(), "./data/bilkollektivet.sqlite")

  return(conn)
}

save_to_db <- function(data, table, quiet = FALSE) {
  #' Save data frame to the database

  conn <- create_db_connection()

  dbWriteTable(conn, name = table, value = data, row.names = FALSE, overwrite = TRUE)

  if (!quiet) {
    print(str_glue("Saved to {table}"))
  }
}

#-- Main -------------------------

pricelist <- scrape_prices()
carlabels <- image_links()
subscriptions <- subscription_info()

save_to_db(pricelist, table = "pricelist")
save_to_db(carlabels, table = "carlabels")
save_to_db(subscriptions, table = "subscriptions")
