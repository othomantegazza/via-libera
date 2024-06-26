library(tidyverse)
library(svglite)
library(readxl)
library(janitor)
library(patchwork)
library(glue)

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
      axis.text = element_text(size = font_size,
                               colour = "black"),
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

plot_position_scaled <- function(data,
                          posizione,
                          max_n_per_km) {
  
  p <- 
    data %>% 
    arrange(desc(n_per_km)) %>% 
    slice(1:20) %>% 
    ggplot() +
    aes(
      x = n_per_km/10,
      y = nome_via %>% as_factor() %>% fct_rev()
    ) +
    common_part +
    labs(
      x = paste("Automobili su", posizione),
      y = ""
    ) +
    geom_label(
      aes(
        label = after_stat(x) %>% 
          round() %>% 
          as.character() %>%
          str_pad(side = "both",
                  width = 1)
        ),
      size = font_size/size_scale,
      label.size = 0,
      label.padding = unit(.2, "lines"),
      hjust = 0,
      fill = "#00000000"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, .2)),
      limits = c(0, max_n_per_km/10),
      position = "top"
    ) +
    theme(
      axis.title = element_text(
        size = font_size*1.3,
        hjust = 1,
        colour = "black"
        )
    )
  
  return(p)
}

plot_position_raw <- function(data,
                              posizione,
                              max_n) {
  
  p <- 
    data %>% 
    arrange(desc(n)) %>% 
    slice(1:20) %>%
    ggplot() +
    aes(
      x = n,
      y = nome_via %>% as.factor() %>% fct_rev()
    ) +
    common_part +
    labs(
      x = paste("Automobili su", posizione),
      y = ""
    ) +
    geom_label(
      aes(
        label = after_stat(x) %>% 
          round() %>% 
          as.character() %>%
          str_pad(side = "both",
                  width = 1)
      ),
      size = font_size/size_scale,
      label.size = 0,
      label.padding = unit(.2, "lines"),
      hjust = 0,
      fill = "#00000000"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, .1)),
      limits = c(0, max_n),
      position = "top"
    ) +
    theme(
      axis.title = element_text(
        size = font_size*1.3,
        hjust = 1,
        colour = "black"
      )
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
  
  ps_scaled <- 
    d_nested %>% 
    pmap(plot_position_scaled)
  
  ps_raw <- 
    data %>% 
    nest(.by = posizione) %>% 
    mutate(max_n = max_n) %>%
    pmap(plot_position_raw)
    

  p_out_scaled <- ps_scaled[[1]] + ps_scaled[[2]] + ps_scaled[[3]]
  
  p_out_raw <- ps_raw[[1]] + ps_raw[[2]] + ps_raw[[3]]
  
  p_out_scaled <- 
    p_out_scaled +
    plot_annotation(
      subtitle = "Automobili in Sosta Illegale ogni 100 metri di Strada [n]",
      title = paste("Municipio", municipio),
      theme = theme(
        plot.title = element_text(size = font_size*2),
        plot.subtitle = element_text(size = font_size*1.3)
      )
    )
  
  ggsave(
    filename = glue("viz/municipi-macchine-per-100m/municipio-{municipio}-macchine-per-100-metri.png"),
    plot = p_out_scaled,
    width = 40,
    height = 18,
    units = "cm"
  )
  
  ggsave(
    filename = glue("viz/municipi-macchine-per-100m/municipio-{municipio}-macchine-per-100-metri.svg"),
    plot = p_out_scaled,
    width = 40,
    height = 18,
    units = "cm"
  )
  
  p_out_raw <- 
    p_out_raw +
    plot_annotation(
      subtitle = "Automobili in Sosta Illegale [n]",
      title = paste("Municipio", municipio),
      theme = theme(
        plot.title = element_text(size = font_size*2),
        plot.subtitle = element_text(size = font_size*1.3)
      )
    )
  
  ggsave(
    filename = glue("viz/municipi-macchine-per-via/municipio-{municipio}-macchine-per-via.png"),
    plot = p_out_raw,
    width = 40,
    height = 18,
    units = "cm"
  )
  
  ggsave(
    filename = glue("viz/municipi-macchine-per-via/municipio-{municipio}-macchine-per-via.svg"),
    plot = p_out_raw,
    width = 40,
    height = 18,
    units = "cm"
  )
  
  
  # return(p_out)  
}
  
d_long %>% 
  nest(.by = municipio) %>% 
  pmap(plot_worst_streets)
