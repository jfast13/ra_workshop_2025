library(tidyverse)
library(epiextractr)
library(epidatatools)


basic_data <- load_basic(2023, year, emp, wbhao, basicwgt, age, female, discwork, unemp)

universe <- basic_data %>%
  filter(age >= 16) %>%
  mutate(across(c(wbhao, female), ~as_factor(.x)),
         wgt = basicwgt/12)
class(universe  %>%  group_by(wbhao) %>% ungroup())

universe %>%
  summarise(n = sum(basicwgt/12), .by = wbhao) %>%
  mutate(share = n/sum(n)) %>%
  write_csv("./output/shares_wbhao.csv")


######## Function #########
crosstab_fun <- function(x ) {
  
  universe %>%
    summarise(
      across(c(discwork, unemp, emp), 
             list(shares = ~sum(.x * wgt, na.rm = TRUE),
                  obs = ~sum(.x , na.rm = TRUE))),
      .by = {{x}} 
    ) %>%
    mutate(across(ends_with("shares"), ~ .x  / sum(.x ))) %>%
    pivot_longer(
      cols = -{{x}},
      names_to = c("measure", "names"), 
      names_sep = "_",
      values_to = "value"
    ) %>% 
    pivot_wider(id_cols= c({{ x }}, measure), names_from = names, values_from= value)
  
}

crosstab_fun(wbhao)



df<- map(.x = c(wbhao, female), .f = ~crosstab_fun(.x)) %>%  
  reduce(bind_rows) %>% 
  unite(col = "demographic", c(wbhao, female), na.rm = TRUE)










  
