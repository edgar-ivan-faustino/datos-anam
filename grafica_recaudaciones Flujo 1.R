# Instalar los paquetes si no los tienes: install.packages(c("ggplot2", "dplyr", "scales"))
library(ggplot2)
library(dplyr)
library(scales)

# 1. Crear los datos
meses <- c("Ene\n2025", "Feb\n2025", "Mar\n2025", "Abr\n2025", "May\n2025", "Jun\n2025",
           "Jul\n2025", "Ago\n2025", "Sep\n2025", "Oct\n2025", "Nov\n2025", "Dic\n2025",
           "Ene\n2026", "Feb\n2026", "Mar\n2026", "Abr\n2026")

valores <- c(80648, 86860, 79777, 90004, 82183, 79569,
             84751, 78964, 80463, 85820, 75363, 82850,
             69207, 65521, 77492, 18031)

df <- data.frame(
  Mes = factor(meses, levels = meses),
  Valor = valores,
  Id = 1:length(meses)
)

# 2. Filtrar dinámicamente los puntos que llevarán etiqueta
df_labels <- df %>%
  mutate(
    etiqueta = case_when(
      Id == 1 ~ "Primero",
      Id == max(Id) ~ "Ultimo",
      Valor == max(Valor) ~ "Maximo",
      Valor == 65521 ~ "Minimo",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(etiqueta)) %>%
  # Ajuste de posición vertical
  mutate(nudge_y = ifelse(Valor > mean(df$Valor), 4000, -4000)) # Aumenté un poco el margen

# 3. Construir la gráfica
p <- ggplot(df, aes(x = Mes, y = Valor, group = 1)) +
  
  geom_smooth(method = "lm", se = FALSE, color = "#b88645", 
              linetype = "dashed", linewidth = 1) +
  
  geom_line(color = "#125f4a", linewidth = 1.2) +
  geom_point(color = "#125f4a", size = 2.5) +
  
  geom_label(data = df_labels,
             aes(label = comma(Valor), y = Valor + nudge_y),
             fill = "#125f4a",
             color = "white",
             fontface = "bold",
             label.padding = unit(0.3, "lines"),
             label.r = unit(0.5, "lines"),
             size = 4,
             vjust = 0.5) +
  
  # --- AQUÍ ESTÁ LA CORRECCIÓN ---
  scale_y_continuous(
    # Nuevos cortes proporcionales para abarcar el 18,000
    breaks = seq(15000, 90000, length.out = 4), 
    labels = comma_format(),
    # Ampliamos los límites para que el 18,031 y su etiqueta quepan perfectamente
    limits = c(10000, 98000) 
  ) +
  # -------------------------------

labs(
  title = "Recaudación en flujo de efectivo de IVA por importaciones (MDP)",
  x = NULL,
  y = "Recaudación de IVA (MDP)"
) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#8a203b"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e0e0e0"),
    axis.text.x = element_text(size = 9, color = "#666666", lineheight = 0.8),
    axis.text.y = element_text(size = 10, color = "#666666"),
    axis.title.y = element_text(size = 11, color = "#333333", margin = margin(r = 10))
  )


ggsave(
  filename = "recaudacion.svg", 
  plot = p, 
  device = "svg", 
  bg = "transparent", # Obliga a ggsave a respetar la transparencia
  width = 10,         # Ancho en pulgadas (ajusta según necesites)
  height = 6,         # Alto en pulgadas (ajusta según necesites)
  units = "in"
)