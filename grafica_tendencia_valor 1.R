library(ggplot2)
library(dplyr)
library(scales)
library(svglite) # Necesario para la exportación a SVG

# 1. Crear los datos (se mantiene igual)
meses <- c("Ene\n2025", "Feb\n2025", "Mar\n2025", "Abr\n2025", "May\n2025", "Jun\n2025",
           "Jul\n2025", "Ago\n2025", "Sep\n2025", "Oct\n2025", "Nov\n2025", "Dic\n2025",
           "Ene\n2026", "Feb\n2026", "Mar\n2026", "Abr\n2026")

valores <- c(721801, 656328, 730409, 812411, 759997, 785317,
             793083, 822738, 778720, 873902, 781757, 824870,
             709838, 953334, 854656, 102923)

df <- data.frame(
  Mes = factor(meses, levels = meses),
  Valor = valores,
  Id = 1:length(meses)
)

# 2. Filtrar dinámicamente (se mantiene igual)
df_labels <- df %>%
  mutate(
    etiqueta = case_when(
      Id == 1 ~ "Primero",
      Id == max(Id) ~ "Ultimo",
      Valor == max(Valor) ~ "Maximo",
      Valor == 656328 ~ "Minimo",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(etiqueta)) %>%
  mutate(nudge_y = ifelse(Valor > mean(df$Valor), 50000, -50000)) 

# 3. Construir la gráfica y guardarla en un objeto 'p'
p <- ggplot(df, aes(x = Mes, y = Valor, group = 1)) +
  geom_smooth(method = "lm", se = FALSE, color = "#b88645", 
              linetype = "dashed", linewidth = 1) +
  geom_line(color = "#125f4a", linewidth = 1.2) +
  geom_point(color = "#125f4a", size = 2.5) +
  
  geom_label(data = df_labels,
             aes(label = comma(Valor), y = Valor + nudge_y),
             fill = "#125f4a", color = "white", fontface = "bold",
             label.padding = unit(0.3, "lines"), label.r = unit(0.5, "lines"),
             size = 4, vjust = 0.5) +
  
  scale_y_continuous(
    breaks = seq(100000, 1000000, length.out = 4), 
    labels = comma_format(),
    limits = c(50000, 1050000) 
  ) +
  
  labs(
    title = "Tendencia del valor total de importaciones temporales",
    x = NULL, y = "Volumen de importación"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#8a203b"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e0e0e0"),
    axis.text.x = element_text(size = 9, color = "#666666", lineheight = 0.8),
    axis.text.y = element_text(size = 10, color = "#666666"),
    axis.title.y = element_text(size = 11, color = "#333333", margin = margin(r = 10)),
    
    # --- MODIFICACIONES PARA FONDO TRANSPARENTE ---
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

# 4. Exportar la imagen a SVG con fondo transparente
ggsave(
  filename = "importaciones_tendencia.svg", 
  plot = p, 
  device = "svg", 
  bg = "transparent", # Obliga a ggsave a respetar la transparencia
  width = 10,         # Ancho en pulgadas (ajusta según necesites)
  height = 6,         # Alto en pulgadas (ajusta según necesites)
  units = "in"
)