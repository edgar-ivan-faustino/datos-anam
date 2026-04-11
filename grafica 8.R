# 1. Cargar librería
library(ggplot2)

# 2. Dataset con posiciones X optimizadas (Enero pegado al eje)
datos_final <- data.frame(
  pos_x = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.6, 15.2, 16.2, 16.8), 
  valor = c(439.4, 467.2, 335.0, 858.2, 816.0, 765.2, 711.2, 656.8, 716.0, 801.6, 766.2, 781.8, 618.9, 702.7, 603.7, 652.9, 289.5, 521.5),
  tipo = c(rep("IEPS", 15), "IEPS_Sin", "IEPS", "IEPS_Sin")
)

# 3. Generar la gráfica con fuente máxima en Eje X
ggplot(datos_final, aes(x = pos_x, y = valor, fill = tipo)) +
  # Barras
  geom_col(width = 0.6, color = NA) +
  # Colores Institucionales
  scale_fill_manual(values = c("IEPS" = "#2D6B57", "IEPS_Sin" = "#9C2A44")) +
  # Ajuste del Eje X: Etiquetas centradas en sus bloques
  scale_x_continuous(breaks = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.9, 16.5), 
                     labels = c("E25", "F25", "M25", "A25", "M25", "J25", "Jl25", "A25", "S25", "O25", "N25", "D25", "E26", "F26", "M26", "A26"),
                     expand = c(0.01, 0)) + 
  # Eje Y: Sin etiquetas, conservando líneas de guía (grid)
  scale_y_continuous(limits = c(0, 1050), 
                     breaks = seq(0, 1000, 200), 
                     expand = c(0,0),
                     labels = NULL) + 
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#D3D3D3"),
    axis.title = element_blank(),
    # Tamaño de fuente aumentado a 14.17 (un 12% extra sobre el previo 12.65)
    axis.text.x = element_text(face = "bold", size = 14.17, color = "#4D4D4D"),
    axis.text.y = element_blank(),
    legend.position = "none",
    # Margen derecho ligeramente mayor para evitar cortes en el texto largo
    plot.margin = margin(10, 20, 10, 10)
  )