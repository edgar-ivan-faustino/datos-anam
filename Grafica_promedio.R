library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(showtext)


Sys.setlocale("LC_TIME", "Spanish") 
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# Datos
load("C:/Users/JuanAndrésRamírezSeg/OneDrive - Agencia Nacional de Aduanas de México/Documentos/tablero_info diaria/Df_grafica.RData")

datos <- Ts_pd_mensual

#fechas
datos_grafica <- datos %>%
  mutate(
    Inicio_Mes = as.Date(paste(ano, mes, "01", sep = "-")),
    Fin_Mes = Inicio_Mes %m+% months(1),
    # mitad de mes
    Mitad_Mes = Inicio_Mes + floor(as.numeric(Fin_Mes - Inicio_Mes) / 2),
    Siguiente_Promedio = lead(pd_mensual)
  )

# promedios Anuales, lineas tendencia
promedio_2024 <- mean(datos_grafica$pd_mensual[datos_grafica$ano == 2024], na.rm = TRUE)
promedio_2025 <- mean(datos_grafica$pd_mensual[datos_grafica$ano == 2025], na.rm = TRUE)
promedio_2026 <- mean(datos_grafica$pd_mensual[datos_grafica$ano == 2026], na.rm = TRUE)

if(is.nan(promedio_2024)) promedio_2024 <- NA
if(is.nan(promedio_2025)) promedio_2025 <- NA
if(is.nan(promedio_2026)) promedio_2026 <- NA

# Marcas Eje x
marcas_x <- datos_grafica$Mitad_Mes[seq(1, nrow(datos_grafica), by = 2)]


# Gráfica
p <- ggplot(datos_grafica) +
  
  # Fondo sombreado
  annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2025-01-01"), ymin = -Inf, ymax = Inf, fill = "#F0F8FF", alpha = 0.6) +
  annotate("rect", xmin = as.Date("2025-01-01"), xmax = as.Date("2026-01-01"), ymin = -Inf, ymax = Inf, fill = "#FFFFFF", alpha = 0.8) +
  annotate("rect", xmin = as.Date("2026-01-01"), xmax = max(datos_grafica$Fin_Mes, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "#F0F8FF", alpha = 0.6) +
  
  # Líneas de referencia
  { if (!is.na(promedio_2024)) geom_hline(yintercept = promedio_2024, linetype = "dashed", color = "tan4", linewidth = 0.6) } +
  { if (!is.na(promedio_2025)) geom_hline(yintercept = promedio_2025, linetype = "dashed", color = "darkred", linewidth = 0.6) } +
  { if (!is.na(promedio_2026)) geom_hline(yintercept = promedio_2026, linetype = "dashed", color = "gray50", linewidth = 0.6) } +
  
  { if (!is.na(promedio_2024)) annotate("text", x = min(datos_grafica$Inicio_Mes) + 10, y = promedio_2024, label = "P.D. 2024", color = "tan4", size = 3, fontface = "bold", hjust = 0, vjust = -0.5) } +
  { if (!is.na(promedio_2025)) annotate("text", x = min(datos_grafica$Inicio_Mes) + 10, y = promedio_2025, label = "P.D. 2025", color = "darkred", size = 3, fontface = "bold", hjust = 0, vjust = -0.5) } +
  { if (!is.na(promedio_2026)) annotate("text", x = min(datos_grafica$Inicio_Mes) + 10, y = promedio_2026, label = "P.D. 2026", color = "gray50", size = 3, fontface = "bold", hjust = 0, vjust = -0.5) } +
  
  # Escalones y Puntos
  geom_segment(aes(x = Inicio_Mes, xend = Fin_Mes, y = pd_mensual, yend = pd_mensual), color = "#12522b", linewidth = 1.2) +
  geom_segment(aes(x = Fin_Mes, xend = Fin_Mes, y = pd_mensual, yend = Siguiente_Promedio), color = "#12522b", linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(x = Mitad_Mes, y = pd_mensual), color = "#12522b", size = 2) +
  
  # Ejex alineads
  scale_x_date(
    breaks = marcas_x, 
    labels = function(x) paste0(toupper(format(x, "%b")), "\n", format(x, "%y")),
    limits = c(min(datos_grafica$Inicio_Mes, na.rm = TRUE), max(datos_grafica$Fin_Mes, na.rm = TRUE)),
    expand = c(0.02, 0)
  ) +
  
  # Eje Y
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) +
  
  labs(x = NULL, y = "Promedio Diario por Mes (MDP)") +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85"),
    
    axis.text.x = element_text(
      color = "gray40", 
      size = 9, 
      lineheight = 0.9,      
      vjust = 1,             
      margin = margin(t = 5),
      hjust = 0.5 # Centrado perfecto bajo el punto
    ),
    axis.text.y = element_text(color = "gray40", size = 9),
    axis.title.y = element_text(color = "gray30", size = 10, face = "bold", margin = margin(r = 10))
  )


ggsave("grafica_tendencia.png", plot = p, device = "png", bg = "white", width = 10, height = 5, dpi = 300)

ggsave(
  filename = "grafica_tendencia.svg",
  plot     = p,
  device   = "svg",
  bg       = "transparent",
  width    = 10,
  height   = 5.5,
  units    = "in"
)
