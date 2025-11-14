
exchange_rate_map <-
  exchange_rates %>%
  filter(!is.na(`nominal exchange rate`))

predict_currency = function(exchange_rate, currency_year) {
    exchange_rate_map %>%
    filter(year == currency_year) %>%
    mutate(
        abs_log_difference = abs(
            log(1 / `nominal exchange rate`) - 
            log(exchange_rate)
        )
    ) %>%
    arrange(abs_log_difference) %>%
    slice(1) %>%
    .$country
}

test = 
    raw_ratios %>%
    select(
        currency_year,
        tufts_currency_country,
        local_price_of_life_final,
        historical_price_of_life
    ) %>%
    mutate(
        price_ratio = historical_price_of_life / local_price_of_life_final
    ) %>%
    filter(!is.na(price_ratio)) %>%
    slice(1:200) %>%
    rowwise() %>%
    mutate(
        predicted_currency_country = predict_currency(price_ratio, currency_year)
    ) %>%
    ungroup()

currency_year = 1996
exchange_rate = 0.6454
predict_currency(1, 1998)