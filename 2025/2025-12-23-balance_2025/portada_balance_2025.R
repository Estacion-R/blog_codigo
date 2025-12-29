# Portada Balance 2025 - Estación R
# Genera una imagen con los logros destacados del año

library(ggplot2)
library(showtext)
library(png)
library(grid)

# Definir directorio de trabajo para las rutas relativas
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

# Agregar fuente Ubuntu (la del blog)
font_add_google("Ubuntu", "ubuntu")
showtext_auto()

# Colores de Estación R (basados en el logo - AZUL)
color_fondo <- "#1a1a2e"
color_primario <- "#3366FF"
color_texto <- "#ffffff"

# Cargar imágenes de cursos
img_intro <- readPNG("img/ER - CUROS - Introducción al procesamiento de datos con R.png")
img_shiny <- readPNG("img/ER - CUROS - Introducción al armado de Dashboards con R + Shiny.png")
img_viz <- readPNG("img/ER - CUROS - Introducción a la visualización de datos con R.png")
img_intermedio <- readPNG("img/ER - CUROS - R intermedio.png")
img_logo <- readPNG("img/Logo_PNG_Baja_Mesa de trabajo 1 copia.png")

# Gráfico comprimido con título en una línea y texto grande
p <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(20, 30, 20, 30)
  ) +

 # Título en una sola línea
  annotate("text", x = 2.5, y = 2.5, label = "2025, ¡Qué año!",
           size = 50, color = color_texto, fontface = "bold", family = "ubuntu") +

 # Subtítulo
  annotate("text", x = 2.5, y = 2.05, label = "Balance anual de Estación R",
           size = 20, color = "#aaaaaa", family = "ubuntu") +

 # Imágenes de cursos (fila)
  annotation_custom(
    rasterGrob(img_intro, interpolate = TRUE),
    xmin = 0.5, xmax = 1.5, ymin = 0.7, ymax = 1.9
  ) +
  annotation_custom(
    rasterGrob(img_shiny, interpolate = TRUE),
    xmin = 1.5, xmax = 2.5, ymin = 0.7, ymax = 1.9
  ) +
  annotation_custom(
    rasterGrob(img_viz, interpolate = TRUE),
    xmin = 2.5, xmax = 3.5, ymin = 0.7, ymax = 1.9
  ) +
  annotation_custom(
    rasterGrob(img_intermedio, interpolate = TRUE),
    xmin = 3.5, xmax = 4.5, ymin = 0.7, ymax = 1.9
  ) +

 # Etiquetas debajo de las imágenes
  annotate("text", x = 1, y = 0.45, label = "2 ediciones",
           size = 10, color = color_texto, family = "ubuntu") +
  annotate("text", x = 2, y = 0.45, label = "2 ediciones",
           size = 10, color = color_texto, family = "ubuntu") +
  annotate("text", x = 3, y = 0.45, label = "NUEVO",
           size = 10, color = color_primario, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 4, y = 0.45, label = "NUEVO",
           size = 10, color = color_primario, fontface = "bold", family = "ubuntu") +

 # Logo blanco y URL en el pie
  annotation_custom(
    rasterGrob(img_logo, interpolate = TRUE),
    xmin = 1.4, xmax = 2.6, ymin = -0.05, ymax = 0.4
  ) +
  annotate("text", x = 3.5, y = 0.18, label = "estacion-r.com",
           size = 12, color = "#aaaaaa", family = "ubuntu") +

 # Límites
  coord_cartesian(xlim = c(0.3, 4.7), ylim = c(-0.05, 2.75), clip = "off") +

 # Aspecto apaisado (16:7)
  theme(aspect.ratio = 7/16)

# Guardar imagen
ggsave("img/portada_balance_2025.png", p, width = 16, height = 7, dpi = 300, bg = color_fondo)

message("Portada guardada en: img/portada_balance_2025.png")
