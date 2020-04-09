library(readr)
library(dplyr)
library(ggplot2)
url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

covid = read_csv(url) %>%
    mutate(date = lubridate::ymd(date))

erie =
    covid %>% 
    filter(
        county %in% c("Erie"),
        state == "New York"
    ) %>% 
    mutate(
        new_cases = c(diff(cases + deaths), NA)
    ) %>%
    head(-1)

ggplot(erie, aes(x = date, y = log10(1 + new_cases))) +
    geom_point() +
    geom_smooth()

ny = covid %>%
    filter(state == "New York") %>%
    group_by(date) %>%
    summarize(cases = sum(cases)) %>%
    mutate(new_cases = c(0, diff(cases)))

out.lm <- lm(new_cases ~ date, data = ny)
o <- segmented(out.lm)

dat2 = data.frame(x = xx, y = broken.line(o)$fit)

ggplot(ny, aes(x = date, y = log10(1 + new_cases))) + geom_point() + 
    geom_smooth()

hopkins = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
csv = 
    read_csv(hopkins) %>%
    tidyr::pivot_longer(-(1:4), names_to = "date", values_to = "cases") %>%
    mutate(
        country = `Country/Region`,
        date = lubridate::mdy(date)
    ) %>%
    group_by(country, date) %>% 
    summarize(cases = sum(cases))

csv %>% 
    group_by(country) %>% 
    summarize(cases = max(cases)) %>% 
    arrange(desc(cases))

csv %>%
    filter(country %in% "US") %>%
    mutate(new_cases = c(diff(cases), NA)) %>%
    filter(new_cases > 1) %>%
    head(-1) %>%
    ggplot(aes(x = date, y = new_cases)) +
      geom_point() + geom_smooth(method="loess", formula = y ~ x) +
      scale_y_log10()
