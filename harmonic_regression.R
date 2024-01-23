library(tidyverse)
library(fpp3)
library(gganimate)

df <-
  read_csv(
    "https://raw.githubusercontent.com/deepakshf/Time-series-analysis-on-Air-passenger-Dataset/main/AirPassengers.csv"
  )

df |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month) |>
  rename(Passengers = `#Passengers`) |>
  mutate(rn = row_number()) -> df

viz_df <- tibble()

for (i in 1:6) {
  df <- df |>
    mutate("sin_x{i}" := sin(2 * pi * i * rn / 12))
  train <- df |>
    filter(Month < yearmonth("1957-01"))
  
  test <- df |>
    filter(Month >= yearmonth("1957-01"))
  
  fit_train <- train |>
    model(TSLM(Passengers ~ . - Passengers))
  
  fit_train |>
    forecast(test) |>
    select(Month, Passengers, .mean) |>
    mutate(k = i) |>
    as_tibble() -> tmp_viz_df
  
  viz_df <- bind_rows(viz_df, tmp_viz_df)
}

viz_df |>
  as_tsibble(index = Month, key = k) |>
  mutate(k = as.integer(k)) |>
  ggplot(aes(x = Month, y = .mean, color = "test_pred")) +
  geom_line() +
  autolayer(train, color = "cornflowerblue") +
  geom_point(data = train,
             aes(x = Month, y = Passengers, colour = "train"),
             size = 0.5) +
  autolayer(test, color = "tan", alpha = 0.9) +
  geom_point(
    data = test,
    aes(x = Month, y = Passengers,
        colour = "test_actual"),
    alpha = 0.5,
    size = 0.5
  ) +
  scale_colour_manual(
    name = "split",
    values = c(
      train = "cornflowerblue",
      test_pred = "salmon",
      test_actual = "tan"
    )
  ) +
  theme(text = element_text(size = 14, family = "Courier")) +
labs(
  title = "forecasting passengers with k = {closest_state}",
  x = "month", y = "passengers"
) +
transition_states(k) +
enter_appear() -> p

animate(p, renderer = gifski_renderer())
anim_save("./harmonic_regression.gif", path = ".")
