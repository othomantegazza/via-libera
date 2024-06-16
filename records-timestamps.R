library(tidyverse)
library(svglite)
library(readxl)
library(janitor)

source("setup-viz.R")

map_date <- as_date("2024-05-16")

timestamps <- 
  read_excel("data/via-libera-records-timestamps.xlsx")%>% 
  janitor::clean_names() %>% 
  rename(stamp = ora_italiana_inserimento) 
  
day(timestamps$stamp) <- 
  yday(map_date)
year(timestamps$stamp) <- 
       year(map_date)

timestamps <- 
  timestamps %>% 
  mutate(
    stamp = case_when(
      hour(stamp) < 15 ~ stamp + days(1),
      TRUE ~ stamp
    )
  )

p_timestamps <-
  timestamps %>% 
  mutate(stamp = stamp %>% floor_date(unit = "5 minutes")) %>% 
  rowwise() %>% 
  mutate(
    n_auto = sum(
      auto_su_careggiata,
      auto_su_marciapiede,
      auto_su_verde,
      na.rm = T
    )
  ) %>%
  ungroup() %>% 
  arrange(desc(n_auto)) %>% 
  mutate(n = 1:n(), .by = stamp) %>%
  ggplot() +
  aes(x = stamp, y = n, fill = n_auto) +
  geom_point(
    shape = 21,
    size = 1.6
  ) +
  labs(x = "Orario inserimento conteggio auto",
       y = "Conteggi inseriti",
       fill = "Auto contate") +
  scale_y_continuous(
    expand = expansion(mult = c(0, .02)),
    limits = c(0, NA)
  ) +
  scale_fill_viridis_c(
    direction = -1,
    option = "D",
    trans = scales::sqrt_trans(),
    breaks = c(1, 100, 200, 300, 400),
    guide = guide_legend(
      override.aes = list(size = 4)
    )
  ) +
  theme(
    axis.line.x = element_line(
      linewidth = line_size
    ),
    axis.title = element_text(size = font_size,
                              hjust = 1),
    axis.text = element_text(size = font_size),
    axis.ticks = element_line(
      linewidth = line_size
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linewidth = line_size/2,
      colour = "black",
      linetype = "11"
    ),
    legend.text = element_text(size = font_size),
    legend.title = element_text(size = font_size),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(.2, "cm"),
  )

ggsave(
  filename = "viz/monitoraggio_timestamps.svg",
  plot = p_timestamps,
  height = 27, width = 20,
  unit = "cm"
)

ggsave(
  filename = "viz/monitoraggio_timestamps.png",
  plot = p_timestamps,
  height = 27, width = 20,
  unit = "cm"
)
  
