library(tidyverse)
library(svglite)
library(readxl)
library(janitor)
library(GGally)

source("setup-viz.R")

# read data -----------------------------------------------------

dat <- 
  read_excel(
    "data/240614_carcount_ptal_population_poi.xlsx"
  ) %>% 
  clean_names()


vie_e_municipi <- 
  read_excel(
    "data/VIA LIBERA!!!_Max.xlsx",
    sheet = "VIA LIBERA_municipi"
  ) %>% 
  clean_names() %>% 
  select(name, municipio, perc_municipio)

# summarise buffer ---------------------------------------------

dat_buffer <- 
  dat %>% 
  summarise(
    total_car_number_per_100m = total_car_number_per_100m %>% 
      mean(na.rm = T),
    sum_of_population_in_100m_buffer = sum_of_population_in_100m_buffer %>% 
      mean(na.rm = T),
    weight = n(),
    .by = name
  ) 

dat_buffer %>%  
  uncount(weight) %>% 
  drop_na() %>% 
  {
    cor(
      .$total_car_number_per_100m,
      .$sum_of_population_in_100m_buffer,
      method = "spearman"
    )
  }

dat_buffer %>% 
  uncount(weight) %>%
  filter(total_car_number_per_100m < 25) %>% 
  ggplot() +
  aes(x = sum_of_population_in_100m_buffer,
      y = total_car_number_per_100m) +
  geom_point(shape = 1,
             alpha = .1)

correlazione_municipi <- 
  dat_buffer %>% 
  left_join(vie_e_municipi) %>% 
  mutate(weight = weight*perc_municipio %>% round()) %>% 
  drop_na() %>% 
  uncount(weight) %>% 
  mutate(municipio = glue::glue("Municipio {municipio}")) %>% 
  ggplot() +
  aes(x = sum_of_population_in_100m_buffer,
      y = total_car_number_per_100m) +
  geom_point(shape = 1,
             alpha = .1) +
  geom_text(
    data = . %>% 
      summarize(
        cor = cor(
          total_car_number_per_100m,
          sum_of_population_in_100m_buffer,
          method = "spearman"
        ) %>% 
          round(2),
        .by = municipio
      ),
      aes(
        x = 4000,
        y = 90,
        label = paste("ρ =", cor)
      ),
    size = font_size/(1.5*size_scale)
  ) +
  scale_y_log10() +
  facet_wrap(facets = "municipio")

ggsave(
  "viz/correlazione-municipi.png",
  plot = correlazione_municipi,
  width = 20,
  height = 20,
  units = "cm"  
)

ggsave(
  "viz/correlazione-municipi.svg",
  plot = correlazione_municipi,
  width = 20,
  height = 20,
  units = "cm"  
)


# summarise density ---------------------------------------------

# dat_pop <- 
#   dat %>% 
#   summarise(
#     total_car_number_per_100m = total_car_number_per_100m %>% 
#       mean(na.rm = T),
#     average_denisty_of_population_per_100m2 = average_denisty_of_population_per_100m2 %>% 
#       mean(na.rm = T),
#     weight = n(),
#     .by = name
#   ) 
# 
# dat_pop %>%  
#   uncount(weight) %>% 
#   drop_na() %>% 
#   {
#     cor(
#       .$total_car_number_per_100m,
#       .$average_denisty_of_population_per_100m2,
#       method = "spearman"
#     )
#   }
# 
# dat_pop %>% 
#   uncount(weight) %>%
#   filter(total_car_number_per_100m < 25) %>% 
#   ggplot() +
#   aes(x = average_denisty_of_population_per_100m2,
#       y = total_car_number_per_100m) +
#   geom_point(shape = 1,
#              alpha = .1)
# 
# dat_pop %>% 
#   left_join(vie_e_municipi) %>% 
#   mutate(weight = weight*perc_municipio %>% round()) %>% 
#   drop_na() %>% 
#   uncount(weight) %>% 
#   filter(average_denisty_of_population_per_100m2 < 5) %>% 
#   ggplot() +
#   aes(x = average_denisty_of_population_per_100m2,
#       y = total_car_number_per_100m) +
#   geom_point(shape = 1,
#              alpha = .1) +
#   scale_y_log10() +
#   facet_wrap(facets = "municipio")
# 

# old -----------------------------------------------------------


dat_cors <- 
  dat %>% 
  group_by(name) %>% 
  summarise_all(~mean(., na.rm = T)) %>% 
  ungroup()

dat_cors %>% 
  select(-name, -id) %>% 
  pivot_longer(
    -`total car number per 100m`
  ) %>% 
  ggplot() +
  aes(
    x = value,
    y = `total car number per 100m`
  ) +
  geom_point(
    shape = 1,
    size = 2
  ) +
  geom_smooth(se = FALSE,
              size = .5) +
  facet_wrap(
    facets = "name",
    scales = "free_x",
    labeller = label_wrap_gen()
  ) +
  theme(
    strip.text = element_text(
      size = font_size
    ),
    panel.spacing = unit(1, "cm"),
    axis.title = element_text(
      size = font_size
    ),
    axis.text = element_text(
      size = font_size
    ),
    axis.ticks = element_line(
      linewidth = line_size
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linewidth = line_size/4,
      colour = "black",
    )
  )

dat_cors %>% 
  select(`total car number per 100m`, `average denisty of population per 100m2`) %>% 
  ggplot() +
  aes(x = `average denisty of population per 100m2`,
      y = `total car number per 100m`) +
  geom_point(
    shape = 1
  ) +
  scale_x_log10()

dat_cors %>% 
  # filter(`total car number per 100m` > 0) %>% 
  select(`total car number per 100m`, `average denisty of population per 100m2`) %>% 
  filter(`average denisty of population per 100m2` <= 10) %>% 
  ggplot() +
  aes(x = `average denisty of population per 100m2`,
      y = `total car number per 100m`) +
  geom_point(
    shape = 1
  ) +
  scale_y_log10() 

dat_cors %>% 
  # filter(`total car number per 100m` > 0) %>% 
  select(`total car number per 100m`, `average denisty of population per 100m2`) %>% 
  filter(`average denisty of population per 100m2` <= 10) %>%
  ggplot() +
  aes(x = `average denisty of population per 100m2`,
      y = `total car number per 100m`) +
  geom_point(
    shape = 1
  ) +
  scale_y_log10() 

dat_cors %>% 
  # filter(`total car number per 100m` > 0) %>% 
  select(`total car number per 100m`, `sum of population in 100m buffer`) %>% 
  # filter(`average denisty of population per 100m2` <= 10) %>% 
  ggplot() +
  aes(x =`sum of population in 100m buffer`,
      y = `total car number per 100m`) +
  geom_point(
    shape = 1
  ) +
  scale_x_log10() +
  scale_y_log10()

dat_cors %>% 
  filter(`total car number per 100m` > 0) %>%
  select(`total car number per 100m`, `average denisty of population per 100m2`) %>% 
  filter(`average denisty of population per 100m2` <= 10) %>%
  drop_na() %>% 
  {
    cor(.[,1], .[,2], method = "spearman") 
  }

dat_cors %>% 
  # filter(`total car number per 100m` > 0) %>%
  select(`total car number per 100m`, `sum of population in 100m buffer`) %>% 
  # filter(`average denisty of population per 100m2` <= 10) %>%
  drop_na() %>% 
  {
    cor(.[,1], .[,2], method = "spearman") 
  }

dat_cors_muni <- 
  dat_cors %>% 
  clean_names() %>% 
  left_join(vie_e_municipi) %>% 
  select(name, municipio, perc_municipio, total_car_number_per_100m, sum_of_population_in_100m_buffer)
