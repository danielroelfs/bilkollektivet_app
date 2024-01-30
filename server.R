### BILKOLLEKTIVET COST BREAKDOWN (SERVER) ########################

#-- Libraries -------------------------

library(tidyverse)
library(shiny)
library(rvest)
library(gt)

#-- Define server -------------------------

shinyServer(function(input, output) {
  # Get variables
  dist <- reactive({
    dist <- input$dist
  })

  n_days <- reactive({
    n_days <- input$n_days
  })

  n_hours <- reactive({
    n_hours <- input$n_hours
  })

  n_pass <- reactive({
    n_pass <- input$n_pass
  })

  car <- reactive({
    car <- input$car
  })

  insurance <- reactive({
    insurance <- input$insurance
  })

  # Input base prices
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

  # Show cartype as title
  output$title <- renderText({
    car()
  })

  # Show base prices
  output$baseprices <- render_gt({
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

    url <- carlabels |>
      filter(car == car()) |>
      pull(linkname)

    pricelist |>
      select(-price_start) |>
      select(car, category, starts_with("price")) |>
      filter(car == car()) |>
      gt() |>
      tab_options(
        table.width = pct(80),
        column_labels.font.weight = "bold",
      ) |>
      tab_style(
        style = list(
          cell_borders(
            sides = c("top", "bottom"),
            color = "white",
            weight = px(2)
          )
        ), locations = list(
          cells_body(
            columns = everything()
          )
        )
      ) |>
      cols_label(
        car = "",
        category = "Class",
        price_hour = "Timepris",
        price_day = "Døgnpris",
        price_week = "Ukepris",
        price_km = "Per kilometer"
      ) |>
      text_transform(
        locations = cells_body(car),
        fn = function(x) {
          web_image(
            url = url,
            height = 80
          )
        }
      ) |>
      fmt_currency(
        columns = starts_with("price"),
        currency = "NOK",
        sep_mark = " ",
        dec_mark = ",",
        use_subunits = TRUE,
        incl_space = TRUE,
      ) |>
      fmt_currency(
        columns = c("price_hour", "price_day", "price_week"),
        currency = "NOK",
        sep_mark = " ",
        pattern = "{x},-",
        use_subunits = FALSE,
        incl_space = TRUE
      ) |>
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels(
            columns = gt::everything()
          )
        )
      )
  })

  # Calculate breakdown
  output$breakdown <- render_gt({
    prices <- pricelist |>
      filter(car == car())

    # Calculate kilometer price
    dist_km <- dist()
    price_km <- prices$price_km * dist_km

    # Calculate time price
    days_price <- prices$price_day * n_days()
    hours_price <- prices$price_hour * n_hours()

    if (n_days() < 7) {
      discount <- 0
      discount_day <- 0
    } else if (n_days() >= 7 & n_days() < 14) {
      discount <- 0.20
      discount_day <- prices$price_day * discount * -1
    } else if (n_days() >= 14) {
      discount <- 0.30
      discount_day <- prices$price_day * discount * -1
    }

    if (input$insurance) {
      ins_cost <- (n_hours() * 14) + (n_days() * 96)
      if (n_days() >= 1) {
        ins_unit <- n_days()
        ins_cost_base <- 96
      } else {
        ins_unit <- n_hours()
        ins_cost_base <- 14
      }
    } else {
      ins_cost <- 0
      ins_unit <- 0
      ins_cost_base <- 0
    }

    discount_text <- pct(discount * 100)
    discount_price <- ifelse(discount > 0, days_price * discount * -1, 0)

    # Create breakdown table
    breakdown_table <- tribble(
      ~Item, ~Units, ~Baseprice, ~Price,
      "Days", n_days(), prices$price_day, days_price,
      "Discount", discount, discount_day, discount_price,
      "Hours", n_hours(), prices$price_hour, hours_price,
      "Distance", dist_km, prices$price_km, price_km,
      "Additional insurance", ins_unit, ins_cost_base, ins_cost
    )

    breakdown_table |>
      gt(rowname_col = "Item") |>
      tab_options(
        table.width = pct(80),
        grand_summary_row.background.color = "#8A29BE",
        column_labels.font.weight = "bold"
      ) |>
      fmt(
        columns = c(Units),
        rows = contains("Discount"),
        fns = function(x) pct(x * 100)
      ) |>
      fmt_currency(
        columns = c(Baseprice),
        currency = "NOK",
        sep_mark = " ",
        dec_mark = ",",
        incl_space = TRUE
      ) |>
      fmt_currency(
        columns = c(Price),
        currency = "NOK",
        use_subunits = FALSE,
        sep_mark = " ",
        dec_mark = ",",
        incl_space = TRUE,
        pattern = "{x},-"
      ) |>
      tab_header(
        title = md("**How much will my trip cost?**")
      ) |>
      grand_summary_rows(
        columns = c(Price),
        fns = list(Total = ~ sum(., na.rm = TRUE)),
        fmt = ~ fmt_currency(
          .,
          currency = "NOK",
          use_subunits = FALSE,
          sep_mark = " ",
          dec_mark = ",",
          incl_space = TRUE
        )
      ) |>
      tab_footnote(
        footnote = md("Discount of 20% on rental periods of 7 days or longer, 30% for 14 days or longer"),
        locations = cells_body(columns = "Units", rows = contains("Discount"))
      ) |>
      tab_footnote(
        footnote = md("Additional insurance is 14 NOK per hour or 96 NOK per day"),
        locations = cells_body(columns = "Units", rows = contains("Additional insurance"))
      ) |>
      tab_footnote(
        footnote = md("Tolls included, fuel included only for non-electric vehicles"),
        locations = cells_grand_summary(
          columns = c(Price)
        )
      ) |>
      cols_align(
        align = "center",
        columns = c(Units)
      ) |>
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels(
            columns = everything()
          )
        )
      ) |>
      tab_style(
        style = list(
          cell_borders(
            sides = "left",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(Price)
          )
        )
      )
  })

  # Calculate individual price
  output$per_passenger <- render_gt({
    prices <- pricelist |>
      filter(car == car())

    # Calculate kilometer price
    dist_km <- dist()
    price_km <- prices$price_km * dist()

    # Calculate time price
    days_price <- prices$price_day * n_days()
    hours_price <- prices$price_hour * n_hours()

    # Calculate insurance price
    if (input$insurance) {
      ins_cost <- (n_hours() * 14) + (n_days() * 96)
    } else {
      ins_cost <- 0
    }

    if (n_days() < 7) {
      discount <- 0
    } else if (n_days() >= 7 & n_days() < 14) {
      discount <- 0.20
    } else if (n_days() >= 14) {
      discount <- 0.30
    }

    discount_price <- ifelse(discount > 0, days_price * discount * -1, 0)

    total_price <- sum(days_price, discount_price, hours_price, price_km, ins_cost)
    passenger_price <- total_price / n_pass()

    if (n_pass() == 1) {
      tribble(
        ~text, ~Price,
        "Total price is equal to individual price", total_price,
      ) |>
        gt() |>
        cols_label(text = "") |>
        tab_options(
          table.width = pct(50),
          column_labels.font.weight = "bold"
        ) |>
        fmt_currency(
          columns = c(Price),
          currency = "NOK",
          use_subunits = FALSE,
          sep_mark = " ",
          dec_mark = ",",
          pattern = "{x}",
          incl_space = TRUE
        ) |>
        tab_header(
          title = md("**Not splitting the costs**")
        ) |>
        tab_style(
          style = list(
            cell_borders(
              sides = "bottom",
              color = "black",
              weight = px(3)
            )
          ),
          locations = list(
            cells_column_labels(
              columns = everything()
            )
          )
        )
    } else {
      tribble(
        ~text, ~Price,
        "Total price for trip", total_price,
        "Price per passenger", passenger_price,
      ) |>
        gt() |>
        cols_label(text = "") |>
        tab_options(
          table.width = pct(50),
          column_labels.font.weight = "bold"
        ) |>
        fmt_currency(
          columns = c(Price),
          currency = "NOK",
          use_subunits = FALSE,
          sep_mark = ".",
          dec_mark = ",",
          pattern = "{x}",
          incl_space = TRUE
        ) |>
        tab_header(
          title = md(sprintf("**Splitting the cost between %s people**", n_pass()))
        ) |>
        tab_style(
          style = list(
            cell_borders(
              sides = "bottom",
              color = "black",
              weight = px(3)
            )
          ),
          locations = list(
            cells_column_labels(
              columns = everything()
            )
          )
        )
    } # end of if-else
  })
})
