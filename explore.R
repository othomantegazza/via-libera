library(tidyverse)
library(svglite)
library(readxl)
library(janitor)

source("setup-viz.R")

# read and clean the data ---------------------------------------

dat <- 
  read_excel(
    "data/VIA LIBERA!!!_Max.xlsx",
    sheet = "VIA LIBERA_municipi"
  ) %>% 
  clean_names()

d_long <- 
  dat %>% 
  select(-perc_municipio, -lunghezza_totale, -x8) %>% 
  rename(nome_via = name) %>% 
  pivot_longer(
    cols = c(
      auto_su_marciapiede_n,
      auto_su_carreggiata_n,
      auto_su_verde_n),
    names_to = "posizione",
    values_to = "n"
  ) %>% 
  mutate(
    posizione = posizione %>%
      str_remove("auto_su_") %>%
      str_remove("_n"),
    n_per_km = n/lenght_km
  )

d_long %>% 
  ggplot() +
  aes(x = n_per_km,
      y = posizione,
      size = n_per_km) +
  geom_jitter(
    shape = 0,
    width = 0,
    height = .3
  )

municipi_ordered <- 
  d_long %>% 
  summarise(
    n = n %>% sum(na.rm = T),
    length_km = lenght_km %>% sum(na.rm = T),
    .by = municipio
  ) %>% 
  mutate(n_by_km = n/length_km) %>% 
  arrange(n_by_km)

# common part of the plot ---------------------------------------

common_part <- 
  list(
    geom_col(
      colour = "black",
      size = line_size
    ) ,
    scale_x_continuous(
      expand = expansion(mult = c(0, .05)),
      position = "top"
    ) ,
    scale_y_discrete( 
      labels = ~paste("Municipio", .),
      expand = expansion(.07)
    ) ,
    scale_fill_viridis_d(
      option = "A",
      direction = -1
    ) , 
    theme(
      axis.line.y = element_line(
        linewidth = line_size
      ),
      axis.title = element_text(size = font_size,
                                hjust = 1),
      axis.text = element_text(size = font_size),
      axis.ticks = element_line(
        linewidth = line_size
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        linewidth = line_size,
        colour = "black",
        linetype = "11"
      ),
      legend.text = element_text(size = font_size),
      legend.title = element_text(size = font_size),
      legend.position = "bottom"
    )
  )


p_tot <- 
  d_long %>% 
  summarise(
    n = n %>% sum(na.rm = T),
    length_km = lenght_km %>% sum(na.rm = T),
    .by = c(municipio, posizione) 
  ) %>% 
  ggplot() +
  aes(
    x = n,
    y = municipio %>%
      as.factor() %>% 
      fct_rev(),
    fill = posizione %>% 
      factor(
        levels = c("carreggiata", "marciapiede", "verde")
      ) %>% 
      fct_rev()
  ) +
  common_part +
  labs(
    x = "Automobili in Sosta Illegale [n]",
    y = "",
    fill = "Posizione:"
  ) 

ggsave(filename = "viz/tot-macchine.svg", 
       plot = p_tot, 
       units = "cm",
       height = 20, width = 20)

ggsave(filename = "viz/tot-macchine.png", 
       plot = p_tot, 
       units = "cm",
       height = 20, width = 20)


p_by_km <- 
  d_long %>% 
  summarise(
    n = n %>% sum(na.rm = T),
    length_km = lenght_km %>% sum(na.rm = T),
    .by = c(municipio, posizione) 
  ) %>% 
  ggplot() +
  aes(x = n/length_km,
      y = municipio %>% 
        as.factor() %>% 
        factor(
          levels = municipi_ordered$municipio
        ),
      fill = posizione %>% 
        factor(
          levels = c("carreggiata", "marciapiede", "verde")
        ) %>% 
        fct_rev()
  ) +
  common_part +
  labs(x = "Automobili in Sosta Illegale per Km di Strada [n]",
       y = "",
       fill = "Posizione:") 
  
   
ggsave(filename = "viz/macchine-per-km.svg", 
       plot = p_by_km, 
       units = "cm",
       height = 20, width = 20)


ggsave(filename = "viz/macchine-per-km.png", 
       units = "cm",
       height = 20, width = 20)


