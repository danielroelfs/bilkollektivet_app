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
        .[5:length(.)] %>%
        str_replace_all(., ",-",".0") %>%
        str_replace_all(., ",", ".")
    
    pricelist <- tibble(.rows = length(htmltable_vector)/6)
    pricelist$cartype <- htmltable_vector[seq(1,length(htmltable_vector),6)]
    pricelist$price_km <- htmltable_vector[seq(2,length(htmltable_vector),6)]
    pricelist$price_200km <- htmltable_vector[seq(3,length(htmltable_vector),6)]
    pricelist$price_hour <- htmltable_vector[seq(4,length(htmltable_vector),6)]
    pricelist$price_day <- htmltable_vector[seq(5,length(htmltable_vector),6)]
    
    pricelist <- pricelist %>%
        mutate_at(vars(starts_with("price")), parse_number)
    
    # Show base prices
    output$baseprices <- render_gt({
        carlabels <- tribble(
            ~cartype, ~linkname,
            "Mellomklasse elbil", "elbil",
            "Premium elbil", "elbil-premium",
            "Elektrisk lastesykkel", "elkassesykkel",
            "Elektrisk varebil", "elvarebil",
            "Småbil", "smabil",
            "Mellomklasse", "mellomklasse",
            "Stasjonsvogn", "stasjonsvogn",
            "7-seter", "9seter",
            "SUV 4x4", "suv",
            "9-seter", "9seter",
            "Varebil", "varebil",
            "Stor varebil", "varebil_stor"
        )
        
        url <- carlabels %>%
            filter(cartype == cartype()) %>%
            pull(linkname) %>%
            sprintf("https://bilkollektivet.no/content/uploads/2019/09/%s.png",.)
        
        pricelist %>%
            filter(cartype == cartype()) %>%
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
                price_km = "Per km (< 200km)",
                price_200km = "Per km (> 200km)",
                price_hour = "Timepris",
                price_day = "Døgnpris"
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
        
        start_price <- 20
        
        # Calculate kilometer price 
        if (dist() <= 200) {
            dist_km <- dist()
            dist_200km <- 0
        } else {
            dist_km <- 200
            dist_200km <- dist() - 200
        }
        price_km <- prices$price_km * dist_km
        price_200km <- prices$price_200km * dist_200km
        
        # Calculate time price
        days_price <- prices$price_day * n_days()
        hours_price <- prices$price_hour * n_hours()
        
        # Create breakdown table
        breakdown_table <- tribble(
            ~Item, ~Units, ~Baseprice, ~Price,
            "Start price", NA, NA, start_price,
            "Days",n_days(), prices$price_day, days_price,
            "Hours",n_hours(), prices$price_hour, hours_price,
            "Distance (< 200km)", dist_km, prices$price_km, price_km,
            "Distance (> 200km)", dist_200km,prices$price_200km, price_200km,
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
                footnote =  md("Fuel and tolls included"),
                locations = cells_grand_summary(
                    columns = vars(Price)
                )
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
        
        start_price <- 20
        
        # Calculate kilometer price 
        if (dist() <= 200) {
            dist_km <- dist()
            dist_200km <- 0
        } else {
            dist_km <- 200
            dist_200km <- dist() - 200
        }
        price_km <- prices$price_km * dist_km
        price_200km <- prices$price_200km * dist_200km
        
        # Calculate time price
        days_price <- prices$price_day * n_days()
        hours_price <- prices$price_hour * n_hours()
        
        # Create breakdown table
        breakdown_table <- tribble(
            ~Item, ~Units, ~Baseprice, ~Price,
            "Start price", NA, NA, start_price,
            "Days",n_days(), prices$price_day, days_price,
            "Hours",n_hours(), prices$price_hour, hours_price,
            "Distance (< 200km)", dist_km, prices$price_km, price_km,
            "Distance (> 200km)", dist_200km,prices$price_200km, price_200km,
        )
        
        total_price <- sum(start_price, days_price, hours_price, price_km, price_200km)
        passenger_price <- total_price / n_pass()
        
        if (n_pass() == 1) {
            
            tribble(
                ~` `, ~Price,
                "Total price is equal to individual price", total_price,
            ) %>%
                gt() %>%
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
                ~` `, ~Price,
                "Total price for trip", total_price,
                "Price per passenger", passenger_price,
            ) %>%
                gt() %>%
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
