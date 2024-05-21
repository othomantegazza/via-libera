library(tidyverse)
library(ggbeeswarm)

theme_set(theme_minimal())

dat <-
  read_csv(
    "data/VIA LIBERA!!!_Max.xlsx - VIA LIBERA_municipi.csv"
  ) %>% 
  janitor::clean_names()


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
  filter(n_per_km > 0,
         n_per_km < 1000) %>% 
  ggplot() +
  aes(x = n_per_km,
      y = posizione,
      size = n_per_km) +
  geom_jitter(
    shape = 0,
    width = 0,
    height = .3
  )
  # geom_quasirandom(
  #   shape = 1
  # )

d_long %>% 
  uncount(n) %>% 
  filter(n_per_km > 0,
         n_per_km < 1000) %>%
  ggplot() +
  aes(x = n_per_km,
      fill = posizione) +
  geom_histogram(
    binwidth = 10,
    colour = "black"
  )

municipi_ordered <- 
  d_long %>% 
  summarise(
    n = n %>% sum(na.rm = T),
    length_km = lenght_km %>% sum(na.rm = T),
    .by = municipio
  )

municipi_ordered <- %>% 
  arrange(desc(n))


line_size <- .6
font_size <- 14
d_long %>% 
  summarise(
    n = n %>% sum(na.rm = T),
    length_km = lenght_km %>% sum(na.rm = T),
    .by = c(municipio, posizione) 
  ) %>% 
  ggplot() +
  aes(x = n,
      y = municipio %>%
        as.factor() %>% 
        fct_rev(),
        # factor(
        #   levels = municipi_ordered$municipio
        # ) %>% 
        # fct_rev(),
      fill = posizione %>% 
        factor(
          levels = c("carreggiata", "marciapiede", "verde")
        ) %>% 
        fct_rev()
  ) +
  geom_col(
    colour = "black",
    size = line_size
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .05)),
    position = "top"
  ) +
  scale_y_discrete( 
    labels = ~paste("Municipio", .),
    expand = expansion(.07)
  ) +
  scale_fill_viridis_d(
    option = "A",
    direction = -1
    ) +
  labs(x = "Automobili in Sosta Illegale [n]",
       y = "",
       fill = "Posizione:") +
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
        # factor(
        #   levels = municipi_ordered$municipio
        # ) %>% 
        fct_rev(),
      fill = posizione %>% 
        factor(
          levels = c("carreggiata", "marciapiede", "verde")
        ) %>% 
        fct_rev()
  ) +
  geom_col(
    colour = "black",
    size = line_size
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .05)),
    position = "top"
  ) +
  scale_y_discrete( 
    labels = ~paste("Municipio", .),
    expand = expansion(.07)
  ) +
  scale_fill_viridis_d(
    option = "A",
    direction = -1
    ) +
  labs(x = "Automobili in Sosta Illegale per Km di Strada [n]",
       y = "",
       fill = "Posizione:") +
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
   


