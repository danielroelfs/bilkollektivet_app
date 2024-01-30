### BILKOLLEKTIVET COST BREAKDOWN (SERVER) ########################

#-- Libraries -------------------------

library(tidyverse)
library(rvest)
library(DBI)
library(dotenv)

#-- Functions -------------------------

scrape_prices <- function() {
  #' Scrape the prices from bilkollektivet

  pricelist <- "https://bilkollektivet.no/priser/" |>
    read_html() |>
    html_nodes("div.fl-col") |>
    html_text() |>
    str_remove_all("\\n|\\t") |>
    {
      \(.) .[seq(19, 31)]
    }() |>
    as_tibble() |>
    separate(
      col = value,
      into = c("cartype", "prices"),
      sep = "Start"
    ) |>
    mutate(cartype = str_trim(cartype)) |>
    filter(nchar(cartype) > 0) |>
    separate(
      col = cartype,
      into = c("carname", "category"),
      sep = "(?<=[a-z|0-9|L|Y])(?=[A-Z|9])"
    ) |>
    separate(
      col = prices,
      into = c(NA, "price_start", "price_hour", "price_day", "price_week", "price_km"),
      sep = ":"
    ) |>
    mutate(
      across(everything(), str_trim),
      price_week = str_remove_all(price_week, "1 uke:"),
      across(
        price_start:last_col(),
        ~ parse_number(.x, locale = locale(
          decimal_mark = ",",
          grouping_mark = " "
        ))
      ),
      car = ifelse(str_detect(category, "Budsjett"),
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
    "Toyota Yaris Cross", "https://bilkollektivet.no/content/uploads/2019/09/Ampera-250x600.png",
    "Toyota Corolla", "https://bilkollektivet.no/content/uploads/2019/09/Corolla_STV_600x250.png",
    "Toyota Proace EL", "https://bilkollektivet.no/content/uploads/2019/09/Toyota-Proace-L2_695x250.png",
    "Toyota Proace", "https://bilkollektivet.no/content/uploads/2019/09/Toyota-Proace-L2_695x250.png",
    "Toyota Proace Verso", "https://bilkollektivet.no/content/uploads/2019/09/Proace-verso_600x250.png",
    "Mazda MX5", "https://bilkollektivet.no/content/uploads/2019/09/MX5-250x600-1-768x335.png",
    "Tesla Model 3", "https://bilkollektivet.no/content/uploads/2019/09/Tesla3-250x600.png",
    "Tesla Model Y", "https://bilkollektivet.no/content/uploads/2021/08/TeslaY-250x600-1.png",
    "Toyota Rav4", "https://bilkollektivet.no/content/uploads/2019/09/Toyota-Rav4_250x600.png",
  )

  return(carlabels)
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

save_to_db(pricelist, table = "pricelist")
save_to_db(carlabels, table = "carlabels")
