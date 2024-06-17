library(tidyverse)
library(svglite)
library(readxl)
library(janitor)

source("setup-viz.R")


# common part ---------------------------------------------------


common_part <- 
  list(
    geom_col(fill = "white",
             colour = "black",
             width = 1,
             size = line_size),
    scale_x_continuous(
      expand = expansion(mult = c(0, .05)),
      position = "top"
    ) ,
    # scale_y_discrete( 
    #   labels = ~paste("Municipio", .),
    #   expand = expansion(.07)
    # ) ,
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
    
dat_1 <- 
  d_long %>% 
  filter(municipio == 1)


# strade peggiori -----------------------------------------------

dat_1 %>% 
  filter(posizione == "marciapiede") %>% 
  arrange(desc(n_per_km)) %>% 
  slice(1:20) %>% 
  ggplot() +
  aes(
    x = n_per_km,
    y = nome_via %>% as_factor() %>% fct_rev()
  ) +
  common_part +
  labs(
    x = "Automobili in Sosta Illegale per Km di Strada [n]",
    y = ""
  ) +
  geom_text(
    aes(label = after_stat(x) %>% round()),
    size = font_size/size_scale,
    hjust = 1.1
  )

