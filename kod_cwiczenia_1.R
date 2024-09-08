library(tidymodels)
library(parsnip)

# Helper packages
library(readr)       # import danych
library(broom.mixed) # konwersja 
library(dotwhisker)  # wizualizacja

colnames(airquality) <- tolower(colnames(airquality))

air <-
  airquality |>
  as_tibble() |>
  na.omit() |> 
  select(-day) |> 
  mutate(month = factor(month)) 

air <- air |> 
  mutate(
    solar_r_cat = cut(solar.r, breaks = 3, labels = c(0, 1, 2)),
    ozone_cat = cut(ozone, breaks = 3, labels = c(0, 1, 2))
  ) 
air |> 
  ggplot(aes(
    x = wind,
    y = temp,
    col = solar_r_cat
  )) +
  geom_smooth(method = lm, se = F) + facet_wrap(ozone_cat ~ month)

lm_mod <- 
  linear_reg() |> 
  set_engine("lm")


lm_fit <-  
  lm_mod |>
  fit(temp ~ wind * month, data = air)
lm_fit  

print(lm_fit, digits = 6)


lm_fit$fit |> summary()
lm_fit |> tidy()
lm_fit |> tidy(conf.int = T)
lm_fit |> 
  tidy() |> 
  dwplot(vline = geom_vline(xintercept = 0, color = "grey50", linetype = 2), 
         dot_args = list(size = 2, color = "black"), 
         whisker_args = list(color = "black")) +
  theme_bw()

# statystycznie istotne są zmienne z pvalue < 0.05

# install.packages("caret")
# library(caret)
# 
# dmy <- dummyVars(" ~month", data=air)
# new_cat <- data.frame(predict(dmy, newdata=air))
# 
# new_cat <- data.frame(air, new_cat)
# 
# lm_fit <- lm_mod |>
#   fit(temp ~ wind * (month.7 + month.8 + month.9) * ozone_cat, data = new_cat)
# lm_fit
# 
# 
# print(lm_fit, digits = 6)
# 
# 
# lm_fit$fit |> summary()
# lm_fit |> tidy()
# lm_fit |> tidy(conf.int = T)
# lm_fit |>
#   tidy() |>
#   dwplot(vline = geom_vline(xintercept = 0, color = "grey50", linetype = 2),
#          dot_args = list(size = 2, color = "black"),
#          whisker_args = list(color = "black")) +
#   theme_bw()

new_points <- expand.grid(wind = seq(5,45,5),
                          month = c("5","6","7","8","9"))
mean_pred <- predict(object = lm_fit, new_data = new_points)
conf_pred <- predict(object = lm_fit, new_data = new_points, type = "conf_int")

# Łączenie danych
lm_pred <- 
  new_points |> 
  bind_cols(mean_pred) |> 
  bind_cols(conf_pred)

# WYkres danych

lm_pred |>
  ggplot(aes(x = wind,
             y = .pred)) +
  geom_point() +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = 0.2) +
  facet_wrap(~ month) +
  theme_bw() +
  labs(y = "temp")
