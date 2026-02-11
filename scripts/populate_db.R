### BILKOLLEKTIVET COST BREAKDOWN (SERVER) ########################

#-- Libraries -------------------------

library(tidyverse)
library(rvest)
library(DBI)

#-- Functions -------------------------

scrape_prices <- function() {
  #' Scrape the prices from bilkollektivet

  raw_list <- "https://bilkollektivet.no/biler/" |>
    read_html() |>
    html_nodes("div.flip-card") |>
    html_text() |>
    str_remove_all("\\n") |>
    str_trim() |>
    as_tibble()

  pricelist = raw_list |>
    mutate(value = str_split(value, "    ")) |>
    mutate(
      selected = map(value, ~ .x[c(1, 2, 34, 33, 46, 45)]),
      price_km = map(value, ~ keep(.x, str_detect, "pr. km")),
    ) |>
    unnest_wider(
      selected,
      names_sep = "_"
    ) |>
    mutate(
      across(is.character, str_trim),
      across(is.character, ~ str_remove(.x, ",-")),
      across(is.character, ~ if_else(.x == "", NA, .x)),
      price_hour = coalesce(selected_3, selected_4),
      price_hour = as.numeric(price_hour),
      price_day = coalesce(selected_5, selected_6),
      price_day = str_remove_all(price_day, " "),
      price_day = parse_number(price_day, locale = locale(grouping_mark = " ")),
      price_km = str_remove(price_km, "Drivstoff og slitasje pr. km: "),
      price_km = str_remove(price_km, "Slitasje pr. km: "),
      price_km = str_trim(price_km),
      price_km = parse_number(
        price_km,
        locale = locale(decimal_mark = ",", grouping_mark = " ")
      )
    ) |>
    select(carname = selected_1, category = selected_2, starts_with("price")) |>
    mutate(
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
    ~car                            , ~linkname                                                                                                                                             ,
    "Toyota Yaris (Budsjettklasse)" , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/3e81b91a-1bb4-4c41-9c99-d6e9b0e2b84a/YarisBudsjett.png?content-type=image%2Fpng" ,
    "Opel Corsa-e"                  , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/2807c76a-f43a-4b30-82ba-0a3eb9d73ed8/OpelCorsa.png?content-type=image%2Fpng"     ,
    "Toyota Yaris"                  , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/62fb9651-6ecb-427e-9f05-d6b66e82e00a/Yaris.png?content-type=image%2Fpng"         ,
    "Mazda MX5"                     , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/f368fd0d-a8b2-477c-b6f9-8ac55990fea2/Mazda-MX-5.png?content-type=image%2Fpng"    ,
    "Toyota Yaris Cross"            , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/1f30a917-fe9b-43b9-b8dd-52536c8e1685/YarisCross.png?content-type=image%2Fpng"    ,
    "MG ZS"                         , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/e21b4379-0f66-4b11-b894-2e5e53422e97/MG-ZS.png?content-type=image%2Fpng"         ,
    "Toyota Corolla"                , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/8b44827c-f966-4b9c-b74f-b209f797eee5/Corolla.png?content-type=image%2Fpng"       ,
    "Tesla Model 3"                 , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/705c4794-902d-4c32-acfd-be8a26bdb2f4/Tesla-mod-3.png?content-type=image%2Fpng"   ,
    "Tesla Model Y"                 , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/7670e022-5445-45b7-843b-929de8dba453/Tesla-mod-Y.png?content-type=image%2Fpng"   ,
    "Corolla Cross"                 , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/17ae19d2-2f25-4700-b92e-be1d0d901dea/CorollaCross.png?content-type=image%2Fpng"  ,
    "Toyota Rav4"                   , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/510a0ea8-9b02-49c4-b16f-f4345d4f2007/RAV4.png?content-type=image%2Fpng"          ,
    "Opel Vivaro EL"                , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/5daad8e8-d0f8-4d66-a52e-4a354a8963f8/Proace-Vivaro.png?content-type=image%2Fpng" ,
    "Opel Vivaro"                   , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/5daad8e8-d0f8-4d66-a52e-4a354a8963f8/Proace-Vivaro.png?content-type=image%2Fpng" ,
    "Toyota Proace EL"              , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/5daad8e8-d0f8-4d66-a52e-4a354a8963f8/Proace-Vivaro.png?content-type=image%2Fpng" ,
    "Toyota Proace"                 , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/5daad8e8-d0f8-4d66-a52e-4a354a8963f8/Proace-Vivaro.png?content-type=image%2Fpng" ,
    "Toyota Proace Verso"           , "https://images.squarespace-cdn.com/content/67d04c1f88e98e18f1f5b0bb/75723a31-4401-4361-9418-94596a3fefe7/Proace-verso.png?content-type=image%2Fpng"  ,
  )

  return(carlabels)
}

subscription_info <- function() {
  #' Get the data associated with different subscriptions
  data_sub <- tribble(
    ~type           , ~cost , ~start_price , ~time_discount ,
    "Klikk og kjør" ,     0 ,           35 , 0              ,
    "Privat"        ,   149 ,            0 , 0.05           ,
    "Privat+"       ,  1250 ,            0 , 0.4
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

  dbWriteTable(
    conn,
    name = table,
    value = data,
    row.names = FALSE,
    overwrite = TRUE
  )

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
