library(tidyverse)
library(svglite)
library(readxl)
library(janitor)

source("setup-viz.R")

map_date <- as_date("2024-05-16")

iscrizioni <-
  "data/Copy of PER OTHO - GRAFICO ISCRIZIONI.xlsx" %>% 
  read_excel() %>% 
  janitor::clean_names() %>% 
  rename(mezzo = organizzare_tutte_le_squadre_e_ottimizzare_le_necessita_di_ognuna_non_e_semplice_ma_se_indichi_qui_la_tua_modalita_preferita_per_muoverti_faremo_un_tentativo_di_aggiungerti_a_qualche_squadra_gia_esistente_la_mappatura_sara_giovedi_16_maggio_e_durera_circa_unora_nella_fascia_dalle_18_alle_22_le_squadre_si_auto_organizzano_e_decidono_quando_farla)
  

p_iscrizioni <-
iscrizioni %>% 
  mutate(
    informazioni_cronologiche = informazioni_cronologiche %>% 
      floor_date("day")
  ) %>% 
  filter(informazioni_cronologiche <= map_date) %>% 
  arrange(informazioni_cronologiche, mezzo %>% as.factor() %>% fct_rev()) %>% 
  mutate(count = 1:n(),
         .by = informazioni_cronologiche) %>% 
  ggplot() +
  aes(x = informazioni_cronologiche,
      y = count,
      fill = mezzo) +
  geom_point(
    shape = 21,
    size = 1.8,
    stroke= line_size
    ) +
  scale_y_continuous(
    expand = expansion(c(0, .1)),
    limits = c(0, NA)
    ) +
  scale_fill_manual(values = c("#00ff0b", "#ff00f2")) +
  labs(x = "Giorno iscrizione",
       y = "Numero iscrizioni",
       fill = "Mezzo di preferenza") +
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
      linewidth = line_size/4,
      colour = "black",
    ),
    legend.text = element_text(size = font_size),
    legend.title = element_text(size = font_size),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(.2, "cm"),
  )

ggsave(
  filename = "viz/iscrizioni.svg",
  plot = p_iscrizioni,
  height = 20, width = 18,
  unit = "cm"
)

ggsave(
  filename = "viz/iscrizioni.png",
  plot = p_iscrizioni,
  height = 20, width = 18,
  unit = "cm"
)
