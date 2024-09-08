library(tidymodels) 
library(skimr) 
library(GGally) 
library(openair) 
tidymodels_prefer()

air <- 
  mydata |> 
  selectByDate(year = 2002)
air |> skim()

air <- 
  air |> na.omit()

# set.seed(222)
# air[sample(1:nrow(air), size = 300, replace = F),] |> 
#   select(nox, no2) |> 
#   ggpairs()

library(ggpubr)
# wykres regresji liniowej, do sprawdzenia danych 
set.seed(222)
# air[sample(1:nrow(air), size = 300, replace = F),] |> 
#   select(nox, no2) |> 
#   ggplot(aes(nox, no2)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = T, formula = y ~ x) + 
#   stat_cor(label.x = 10, label.y = 80) + 
#   stat_regline_equation(label.x = 10, label.y = 82) +
#   theme_bw()
# 
# 
# air |>    
#   ggplot(aes(date, o3)) +     
#   geom_line() +     
#   theme_bw()
# 
# air |> 
#   pull(o3) |> 
#   range()  


air <- 
  air |>  
  mutate(ozone = cut(
  o3,
  breaks = c(-0.1, 10, 53),
  labels = c("Niskie", "Wysokie")
  ))

# air |> count(ozone)

# 
# air |> 
#   select(o3, nox) |> 
#   ggpairs()
# 
# air|> 
#   select(o3, no2) |> 
#   ggpairs()
# 
# air|> 
#   select(o3, wd) |> 
#   ggpairs()
# 
# air|> 
#   select(o3, pm10) |> 
#   ggpairs()
# 
# air|> 
#   select(o3, pm25) |> 
#   ggpairs()
# 
# air|> 
#   select(o3, so2) |> 
#   ggpairs()
# 
# air|> 
#   select(o3, co) |> 
#   ggpairs()
# 
# air|> 
#   select(o3, ws) |> 
#   ggpairs()

# air |> 
#   mutate(month = as.numeric(format(date, "%m"))) |> 
#   group_by(month) |> 
#   summarise(mean_o3 = mean(o3, na.rm = TRUE)) |> 
#   summarise(correlation = cor(month, mean_o3)) |> 
#   pull(correlation)

air |> 
  select_if(is.numeric) |> 
  cor(use = "complete.obs") |> as.data.frame() |> select(o3)


set.seed(222)
data_split <- initial_split(data=air, prop = 3/4, strata = ozone)
train_data <- training(data_split)
test_data <- testing(data_split)

ozone_recipe <- 
  recipe(ozone ~ ., data = train_data) |> 
  update_role(o3, wd, date, pm10, pm25, so2, co, no2, new_role = "ID") |> 
  step_BoxCox(ws, nox, no2) |>  # daje ten sam efet co metoda Yeo-Johnson
  # step_normalize(ws, nox, no2) |> # nie pomaga 
  step_date(date, features = c("month")) |> 
  step_time(date, features = c("hour")) |>
  step_mutate(date_hour = as.factor(date_hour)) |>  
  step_dummy(all_nominal_predictors()) |> 
  step_zv()


ozone_recipe |> summary()
ozone_recipe |> 
  prep() |> 
  bake(new_data = train_data) |> 
  head(10) |> 
  DT::datatable()

lr_mod <-  
  logistic_reg() |> 
  set_engine("glm")

ozone_work <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(ozone_recipe)

ozone_fit <- 
  ozone_work |> 
  fit(data=train_data)

ozone_fit |> 
  extract_fit_parsnip() |> 
  tidy()

ozone_fit |> 
  predict(test_data, type="prob")

pred_test <- 
  augment(ozone_fit, test_data) |>
  select(-ws,
         -wd,
         -o3,
         -nox,
         -no2,
         -pm10,
         -pm25,
         -so2,
         -co,
         -date)
pred_test
pred_test  |> 
  roc_curve(truth = ozone, .pred_Niskie) |> 
  autoplot()

pred_test |> count(ozone)
pred_test |> count(.pred_class)
