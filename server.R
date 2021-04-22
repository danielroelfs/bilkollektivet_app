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
    
    cartype <- reactive({
        cartype <- input$cartype
        cartype <- case_when(str_detect(cartype, "Tesla|Jaguar") ~ str_glue("{cartype} (el)"),
                             TRUE ~ cartype)
    })
    
    insurance <- reactive({
        insurance <- input$insurance
    })
    
    # Input base prices
    htmltable_vector <- "https://bilkollektivet.no/priser/" %>%
        read_html() %>%
        html_nodes("table") %>%
        html_text() %>%
        str_replace_all(., "\\t","") %>%
        str_split(., "\\n") %>%
        .[[1]] %>%
        str_trim(., side = "both") %>%
        .[. != ""] %>%
        .[4:length(.)] %>%
        str_replace_all(., ",-",".0") %>%
        str_replace_all(., ",", ".") %>%
        .[. != 0]
    
    htmltable_matrix <- matrix(htmltable_vector, ncol = length(htmltable_vector)/10, byrow = TRUE)
    pricelist <- as_tibble(htmltable_matrix, .name_repair = "unique") %>%
        janitor::clean_names() %>% 
        rename(cartype = x1,
               price_hour = x2,
               price_day = x3,
               price_km = x4,
               read_more = x5)
    
    pricelist <- pricelist %>%
        mutate(across(starts_with("price"), parse_number))
    
    # Show cartype as title
    output$title <- renderText({
        cartype()
    })
    
    # Show base prices
    output$baseprices <- render_gt({
        carlabels <- tribble(
            ~cartype, ~linkname,
            "Liten elbil", "smabil",
            "Småbil", "smabil",
            "Mellomklasse elbil", "elbil",
            "Stasjonsvogn", "stasjonsvogn",
            "Tesla Model 3 (el)", "elbil",
            "Jaguar Ipace (el)", "elbil-premium",
            "7-seter", "9seter",
            "SUV 4x4", "suv",
            "Elektrisk varebil", "elvarebil",
            "9-seter", "9seter",
            "Varebil", "varebil"
        )
        
        url <- carlabels %>%
            filter(cartype == cartype()) %>%
            pull(linkname) %>%
            sprintf("https://bilkollektivet.no/content/uploads/2019/09/%s.png",.)
        
        pricelist %>%
            filter(cartype == cartype()) %>%
            select(-read_more) %>%
            gt() %>%
            tab_options(
                table.width = pct(80),
                column_labels.font.weight = "bold",
            ) %>%
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
            ) %>%
            cols_label(
                cartype = "",
                price_hour = "Timepris",
                price_day = "Døgnpris",
                price_km = "Per kilometer"
            ) %>%
            text_transform(
                locations = cells_body(vars(cartype)),
                fn = function(x) {
                    web_image(
                        url = url,
                        height = 80
                    )
                }
            ) %>%
            fmt_currency(
                columns = starts_with("price"), 
                currency = "NOK",
                use_subunits = TRUE, 
                incl_space = TRUE
            ) %>%
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
        prices <- pricelist %>%
            filter(cartype == cartype())
        
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
            ins_cost <- (n_hours() * 11) + (n_days() * 77)
            if (n_days() >= 1) {
                ins_unit <- n_days()
                ins_cost_base <- 77
            } else {
                ins_unit <- n_hours()
                ins_cost_base <- 11
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
        
        breakdown_table %>%
            gt(rowname_col = "Item") %>%
            tab_options(
                table.width = pct(80),
                grand_summary_row.background.color = "#8A29BE", 
                column_labels.font.weight = "bold"
            ) %>%
            fmt_missing(
                columns = everything(),
                missing_text = ""
            ) %>%
            fmt(
                columns = vars(Units),
                rows = contains("Discount"),
                fns = function(x) pct(x * 100)
            ) %>%
            fmt_currency(
                columns = vars(Baseprice), 
                currency = "NOK",
                incl_space = TRUE
            ) %>%
            fmt_currency(
                columns = vars(Price), 
                currency = "NOK", 
                use_subunits = FALSE,
                sep_mark = ".",
                dec_mark = ",",
                pattern = "{x},-",
                incl_space = TRUE
            ) %>%
            tab_header(
                title = md("**How much will my trip cost?**")
            ) %>%
            grand_summary_rows(
                columns = vars(Price),
                fns = list(Total = ~sum(., na.rm = TRUE)),
                formatter = fmt_currency,
                currency = "NOK", 
                use_subunits = FALSE,
                sep_mark = ".",
                dec_mark = ",",
                pattern = "{x},-",
                incl_space = TRUE
            ) %>%
            tab_footnote(
                footnote =  md("Discount of 20% on rental periods of 7 days or longer, 30% for 14 days or longer"),
                locations = cells_body(columns = "Units", rows = contains("Discount"))
            ) %>%
            tab_footnote(
                footnote =  md("Additional insurance is 11 NOK per hour or 77 NOK per day"),
                locations = cells_body(columns = "Units", rows = contains("Additional insurance"))
            ) %>%
            tab_footnote(
                footnote =  md("Fuel and tolls included"),
                locations = cells_grand_summary(
                    columns = vars(Price)
                )
            ) %>%
            cols_align(
                align = "center",
                columns = vars(Units)
            ) %>%
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
            ) %>%
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
                        columns = vars(Price)
                    )
                )
            )
    })
    
    # Calculate individual price
    output$per_passenger <- render_gt({
        
        prices <- pricelist %>%
            filter(cartype == cartype())
        
        # Calculate kilometer price 
        dist_km <- dist()
        price_km <- prices$price_km * dist()
        
        # Calculate time price
        days_price <- prices$price_day * n_days()
        hours_price <- prices$price_hour * n_hours()
        
        if (n_days() < 7) {
            discount <- 0
        } else if (n_days() >= 7 & n_days() < 14) {
            discount <- 0.20
        } else if (n_days() >= 14) {
            discount <- 0.30
        }
        
        discount_price <- ifelse(discount > 0, days_price * discount * -1, 0)
        
        total_price <- sum(days_price, discount_price, hours_price, price_km)
        passenger_price <- total_price / n_pass()
        
        if (n_pass() == 1) {
            
            tribble(
                ~text, ~Price,
                "Total price is equal to individual price", total_price,
            ) %>%
                gt() %>%
                cols_label(text = "") %>%
                tab_options(
                    table.width = pct(50),
                    column_labels.font.weight = "bold"
                ) %>%
                fmt_currency(
                    columns = vars(Price), 
                    currency = "NOK", 
                    use_subunits = TRUE,
                    sep_mark = ".",
                    dec_mark = ",",
                    incl_space = TRUE
                ) %>%
                tab_header(
                    title = md("**Not splitting the costs**")
                ) %>%
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
            ) %>%
                gt() %>%
                cols_label(text = "") %>%
                tab_options(
                    table.width = pct(50),
                    column_labels.font.weight = "bold"
                ) %>%
                fmt_currency(
                    columns = vars(Price), 
                    currency = "NOK", 
                    use_subunits = TRUE,
                    sep_mark = ".",
                    dec_mark = ",",
                    incl_space = TRUE
                ) %>%
                tab_header(
                    title = md(sprintf("**Splitting the cost between %s people**", n_pass()))
                ) %>%
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
