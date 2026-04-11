library(ggplot2)
library(dplyr)
library(scales)
library(svglite)
library(showtext)


font_add_google("Montserrat", "Montserrat")
showtext_auto()

# Datos
meses_cortos <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Ene", "Feb", "Mar", "Abr")
anios <- c(rep("2025", 12), rep("2026", 4))
meses_id <- paste(meses_cortos, anios, 1:16)

valores <- c(721801, 656328, 730409, 812411, 759997, 785317,
             793083, 822738, 778720, 873902, 781757, 824870,
             709838, 953334, 854656, 102923)

df <- data.frame(
  Mes      = factor(meses_id, levels = meses_id),
  MesCorto = meses_cortos,
  Anio     = anios,
  Valor    = valores,
  Id       = seq_along(valores)
)

# Etiquetas_globos
df_labels <- df %>%
  mutate(
    etiqueta = case_when(
      Id == 1             ~ "Primero",
      Id == max(Id)       ~ "Ultimo",
      Valor == max(Valor) ~ "Maximo",
      Valor == 656328 ~ "Minimo",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(etiqueta)) %>%
 
  mutate(nudge_y = case_when(
    Id == 1 ~ 65000, #Forzar Burbuja arriba
    Valor > mean(df$Valor) ~ 65000, 
    TRUE ~ -65000                   
  ))

# Gráfica
p <- ggplot(df, aes(x = Mes, y = Valor, group = 1)) +
  
  geom_smooth(method = "lm", se = FALSE, color = "#b88645", linetype = "dashed", linewidth = 1.2) +
  geom_line(data = filter(df, Id <= 15), color = "#125f4a", linewidth = 1.8) +
  geom_line(data = filter(df, Id >= 15), color = "#125f4a", linewidth = 1.8, linetype = "dashed") +
  geom_point(color = "#125f4a", size = 2.8) +
  
  geom_label(data = df_labels,
             aes(label = scales::comma(Valor), y = Valor + nudge_y),
             fill     = "#125f4a", color = "white",
             family   = "Montserrat", fontface = "bold",
             label.padding = unit(0.30, "lines"),
             label.r       = unit(0.50, "lines"),
             size     = 5.5, vjust = 0.5) +
  
  scale_y_continuous(
    breaks = seq(0, 1000000, by = 200000), 
    labels = scales::comma, 
    expand = c(0, 0)
  ) +
  
  scale_x_discrete(labels = df$MesCorto, expand = expansion(add = c(1.3, 1.0))) +
  
  # Años
  annotate("text",
           x = 1:16,
           y = -65000, 
           label = df$Anio,
           family = "Montserrat",
           size = 3.8,
           color = "#666666") +
  
  coord_cartesian(ylim = c(0, 1100000), clip = "off") +
  
  labs(
    title = NULL, x = NULL,
    y = expression(paste(bold("Volumen de Importación")))
  ) +
  
  theme_minimal(base_family = "Montserrat") +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.4),
    
    axis.line = element_line(color = "#cccccc", linewidth = 1),
    
    axis.text.x  = element_text(size = 11, color = "#666666", face = "bold", margin = margin(b = 20)),
    axis.text.y  = element_text(size = 13, color = "#666666", face = "bold"),
    axis.title.y = element_text(size = 16, color = "#666666", margin = margin(r = 8)),
    
    panel.background  = element_rect(fill = "transparent", color = NA),
    plot.background   = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.margin = margin(t = 20, r = 15, b = 15, l = 5)
  )

# 
ggsave(
  filename = "grafica_16_2_tendencia.svg",
  plot     = p,
  device   = "svg",
  bg       = "transparent",
  width    = 10,
  height   = 5.5,
  units    = "in"
)

showtext_opts(dpi = 300)

ggsave(
  filename = "grafica_16_2_tendencia.png", 
  plot     = p,
  device   = "png",
  bg       = "white", 
  width    = 10,
  height   = 5.5,
  units    = "in",
  dpi      = 300      
)



