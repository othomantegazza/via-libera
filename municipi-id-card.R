library(tidyverse)
library(svglite)
library(readxl)
library(janitor)
library(patchwork)

source("setup-viz.R")


# common part ---------------------------------------------------


common_part <- 
  list(
    geom_col(fill = "white",
             colour = "black",
             width = 1,
             size = line_size),
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


# strade peggiori -----------------------------------------------


plot_position <- function(data,
                          posizione,
                          max_n_per_km) {
  
  p <- 
    data %>% 
    arrange(desc(n_per_km)) %>% 
    slice(1:15) %>% 
    ggplot() +
    aes(
      x = n_per_km/10,
      y = nome_via %>% as_factor() %>% fct_rev()
    ) +
    common_part +
    labs(
      x = "Automobili in Sosta Illegale ogni 100 metri di Strada [n]",
      y = ""
    ) +
    geom_label(
      aes(
        label = after_stat(x) %>% 
          round() %>% 
          as.character() %>%
          str_pad(side = "both",
                  width = 1),
        hjust = after_stat(x) %>% 
          {
            case_when(. < max_n_per_km/100 ~ 0,
                      TRUE ~ 1)
          }
      ),
      size = font_size/size_scale,
      label.size = 0,
      label.padding = unit(.2, "lines"),
      # hjust = 1,
      fill = "#00000000"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, .1)),
      limits = c(0, max_n_per_km/10),
      position = "top"
    ) 
  
  return(p)
}

plot_worst_streets <- function(
    data,
    municipio
) {
  data <- 
    data %>% 
    filter(municipio == {{municipio}}) 
  
  max_n <- data$n %>% max(na.rm = T)
  max_n_per_km <- data$n_per_km %>% max(na.rm = T)
  
  d_nested <- 
    data %>% 
    nest(.by = posizione) %>% 
    mutate(max_n_per_km = max_n_per_km)
  
  ps <- 
    d_nested %>% 
    pmap(plot_position)
    

  p_out <- ps[[1]] + ps[[2]] + ps[[3]]
  
  return(p_out)  
}
  

plot_worst_streets(
  d_long,
  1
)
