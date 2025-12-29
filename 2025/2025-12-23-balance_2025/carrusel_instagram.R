# Carrusel Instagram - Balance 2025 Estación R
# Genera imágenes cuadradas (1080x1080) para Instagram

library(ggplot2)
library(showtext)
library(png)
library(grid)

# Definir directorio de trabajo
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(script_dir)

# Fuente Ubuntu + Big Shoulders para números
font_add_google("Ubuntu", "ubuntu")
font_add_google("Big Shoulders Display", "bigshoulders")
showtext_auto()

# Colores de Estación R
color_fondo <- "#1a1a2e"
color_primario <- "#3366FF"
color_texto <- "#ffffff"
color_gris <- "#aaaaaa"

# Cargar logo
img_logo <- readPNG("img/Logo_PNG_Baja_Mesa de trabajo 1 copia.png")

# Crear carpeta para carrusel
dir.create("img/carrusel", showWarnings = FALSE)

# ========== SLIDE 1: Portada ==========
p1 <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  annotation_custom(
    rasterGrob(img_logo, interpolate = TRUE),
    xmin = 3, xmax = 7, ymin = 7.5, ymax = 9.5
  ) +
  annotate("text", x = 5, y = 5.5, label = "2025",
           size = 80, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 3.5, label = "¡Qué año!",
           size = 30, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 2.3, label = "Balance anual",
           size = 14, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 0.8, label = "Deslizá para ver →",
           size = 10, color = color_gris, family = "ubuntu")

ggsave("img/carrusel/01_portada.png", p1, width = 10, height = 10, dpi = 108, bg = color_fondo)

# ========== SLIDE 2: Cursos nuevos ==========
p2 <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  annotate("text", x = 5, y = 7, label = "4",
           size = 80, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 5.2, label = "cursos nuevos",
           size = 20, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 4, label = "Visualización · Mapas\nR Intermedio · R + Excel",
           size = 11, color = color_gris, family = "ubuntu", lineheight = 1.3) +
  annotate("text", x = 5, y = 2.4, label = "+100",
           size = 40, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 1.5, label = "alumnos en nuestras aulas",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 0.5, label = "2/7",
           size = 8, color = color_gris, family = "ubuntu")

ggsave("img/carrusel/02_cursos.png", p2, width = 10, height = 10, dpi = 108, bg = color_fondo)

# ========== SLIDE 3: Ediciones ==========
p3 <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  annotate("text", x = 5, y = 6.5, label = "4",
           size = 100, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 4.2, label = "ediciones",
           size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 2.8, label = "de nuestros cursos insignia\nR de cero · R + Shiny",
           size = 12, color = color_gris, family = "ubuntu", lineheight = 1.3) +
  annotate("text", x = 5, y = 0.5, label = "3/7",
           size = 8, color = color_gris, family = "ubuntu")

ggsave("img/carrusel/03_ediciones.png", p3, width = 10, height = 10, dpi = 108, bg = color_fondo)

# ========== SLIDE 4: Becas ==========
p4 <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  annotate("text", x = 5, y = 6.5, label = "+20",
           size = 80, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 4.2, label = "becas otorgadas",
           size = 20, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 2.8, label = "Porque el acceso\nno puede ser barrera",
           size = 12, color = color_gris, family = "ubuntu", lineheight = 1.3) +
  annotate("text", x = 5, y = 0.5, label = "4/7",
           size = 8, color = color_gris, family = "ubuntu")

ggsave("img/carrusel/04_becas.png", p4, width = 10, height = 10, dpi = 108, bg = color_fondo)

# ========== SLIDE 5: Equipo ==========
p5 <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  annotate("text", x = 5, y = 7.8, label = "Equipo docente",
           size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 6.8, label = "de lujo",
           size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 4.8, label = "Elián Soutullo",
           size = 14, color = color_primario, family = "ubuntu") +
  annotate("text", x = 5, y = 4, label = "Tomás Bustos",
           size = 14, color = color_primario, family = "ubuntu") +
  annotate("text", x = 5, y = 3.2, label = "Luis Verde Arregoitia",
           size = 14, color = color_primario, family = "ubuntu") +
  annotate("text", x = 5, y = 2.4, label = "Federico Baraghian",
           size = 14, color = color_primario, family = "ubuntu") +
  annotate("text", x = 5, y = 1.2, label = "¡Gracias infinitas!",
           size = 12, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 0.5, label = "5/7",
           size = 8, color = color_gris, family = "ubuntu")

ggsave("img/carrusel/05_equipo.png", p5, width = 10, height = 10, dpi = 108, bg = color_fondo)

# ========== SLIDE 6: Comunidad ==========
p6 <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  annotate("text", x = 5, y = 7.8, label = "Comunidad",
           size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 6.8, label = "activa",
           size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 5.2, label = "Consultorio Abierto de R",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 4.4, label = "LatinR 2025",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 3.6, label = "Newsletter semanal",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 2.6, label = "+100",
           size = 30, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 1.6, label = "publicaciones en redes",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 0.5, label = "6/7",
           size = 8, color = color_gris, family = "ubuntu")

ggsave("img/carrusel/06_comunidad.png", p6, width = 10, height = 10, dpi = 108, bg = color_fondo)

# ========== SLIDE 7: 2026 ==========
p7 <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  annotate("text", x = 5, y = 7.5, label = "2026",
           size = 70, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 5.8, label = "¿Qué se viene?",
           size = 18, color = color_texto, fontface = "bold", family = "ubuntu") +
  annotate("text", x = 5, y = 4.6, label = "Nuevos cursos",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 3.9, label = "R + IA",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 3.2, label = "R + Paquetes",
           size = 11, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 2.5, label = "¿Podcast?",
           size = 11, color = color_primario, family = "ubuntu") +
  annotate("text", x = 5, y = 1.4, label = "¿Qué te gustaría aprender?",
           size = 12, color = color_texto, family = "ubuntu") +
  annotate("text", x = 5, y = 0.8, label = "Contanos en comentarios",
           size = 10, color = color_gris, family = "ubuntu") +
  annotate("text", x = 5, y = 0.2, label = "7/7",
           size = 8, color = color_gris, family = "ubuntu")

ggsave("img/carrusel/07_2026.png", p7, width = 10, height = 10, dpi = 108, bg = color_fondo)

message("✅ Carrusel generado en: img/carrusel/")
message("   - 01_portada.png")
message("   - 02_cursos.png")
message("   - 03_ediciones.png")
message("   - 04_becas.png")
message("   - 05_equipo.png")
message("   - 06_comunidad.png")
message("   - 07_2026.png")
