library(tidyverse)
library(svglite)
library(readxl)
library(janitor)
library(GGally)

source("setup-viz.R")

dat_cors <- 
  read_excel(
    "data/240614_carcount_ptal_population_poi.xlsx"
  )


dat_cors %>%
  select(-name, -id) %>% 
  ggpairs(mapping = aes(alpha = .1))

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
  select(`total car number per 100m`, `average denisty of population per 100m2`) %>% 
  filter(`average denisty of population per 100m2` <= 10) %>% 
  ggplot() +
  aes(x = `average denisty of population per 100m2`,
      y = `total car number per 100m`) +
  geom_point(
    shape = 1
  ) +
  scale_y_log10() 
