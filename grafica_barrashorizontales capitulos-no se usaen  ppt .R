library(ggplot2)
library(dplyr)
library(scales)
library(ggtext)

# conjunto Datos
df <- data.frame(
  id = 3:1, 
  capitulo = c("Capítulo 84", "Capítulo 87", "Capítulo 85"),
  desc = c("Reactores nucleares, calderas<br>y máquinas", 
           "Vehículos automóviles", 
           "Máquinas y aparatos<br>eléctricos"),
  ene_feb = c(-4862.0, -4306.5, -2598.7),
  marzo = c(1852.1, 1487.1, 1135.0),
  color_borde = c("#9d2442", "#a88137", "#215d52") 
) %>%
  mutate(
    capitulo_f = factor(capitulo, levels = capitulo[order(id)]),
    label_html = sprintf(
      "<span style='font-size:12pt; font-weight:bold;'>%s</span><br><span style='font-size:9.5pt;'>%s</span>",
      capitulo, desc
    )
  )

# posición cajas
pos_cajas_x <- -8000

# gráfica
p <- ggplot(df, aes(y = capitulo_f)) +
  
  geom_segment(aes(x = pos_cajas_x, xend = ene_feb, y = capitulo_f, yend = capitulo_f),
               linetype = "dashed", color = "#cccccc", linewidth = 0.6) +
  
  geom_col(aes(x = ene_feb, fill = "Ene-Feb"), width = 0.7) +
  geom_col(aes(x = marzo, fill = "Marzo"), width = 0.7) +
  
  geom_vline(xintercept = 0, color = "#333333", linewidth = 1) +
  
  
  geom_label(aes(x = ene_feb - 150, 
                 label = sprintf("-$%s MDP", comma(abs(ene_feb), accuracy = 0.1))),
             color = "#9d2442", fill = "white", fontface = "bold", size = 4.5,
             label.r = unit(0.6, "lines"), label.padding = unit(0.3, "lines"),
             linewidth = 0.8, hjust = 1) +
  
  
  geom_label(aes(x = marzo + 150, 
                 label = sprintf("$%s MDP", comma(marzo, accuracy = 0.1))),
             color = "#215d52", fill = "white", fontface = "bold", size = 4.5,
             label.r = unit(0.6, "lines"), label.padding = unit(0.3, "lines"),
             linewidth = 0.8, hjust = 0) +
  
  # geom_richtext
  geom_richtext(aes(x = pos_cajas_x, label = label_html, color = color_borde),
                fill = "white", label.r = unit(0.8, "lines"), 
                label.padding = unit(0.5, "lines"), size = 3.5, 
                hjust = 0.5, label.size = 1.2, show.legend = FALSE) +
  
  scale_color_identity() +
  
  scale_fill_manual(
    values = c("Ene-Feb" = "#9d2442", "Marzo" = "#729288"),
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = c(-4000, -2000, 0, 2000),
    labels = comma_format(),
    limits = c(-10000, 3800) 
  ) +
  
  labs(x = "Millones de pesos (MDP)", y = NULL) +
  
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ebebeb"),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 11, color = "#555555"),
    axis.title.x = element_text(size = 12, face = "bold", color = "#444444", margin = margin(t = 10)),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 12, color = "#444444"),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "grafica_7.svg", 
  plot = p, 
  device = "svg", 
  bg = "transparent", 
  width = 10,         
  height = 6,         
  units = "in"
)

showtext_opts(dpi = 300)

ggsave(
  filename = "grafica_7.png", 
  plot     = p,
  device   = "png",
  bg       = "white", 
  width    = 10,
  height   = 6,
  units    = "in",
  dpi      = 300      
)

