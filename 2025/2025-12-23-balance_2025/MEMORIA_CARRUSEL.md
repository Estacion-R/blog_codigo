# Memoria: Generación de Carruseles para Instagram

## Resumen

Este documento describe cómo generar carruseles de imágenes para Instagram usando R y ggplot2, siguiendo la identidad visual de Estación R.

---

## Especificaciones Técnicas

### Formato de imagen
- **Dimensiones**: 1080x1080 px (cuadrado para Instagram)
- **En R**: `width = 10, height = 10, dpi = 108` (10 * 108 = 1080)
- **Formato**: PNG

### Paleta de colores (Estación R)
```r
color_fondo <- "#1a1a2e"      # Azul oscuro (fondo)
color_primario <- "#3366FF"   # Azul brillante (números destacados)
color_texto <- "#ffffff"      # Blanco (títulos)
color_gris <- "#aaaaaa"       # Gris (subtítulos, detalles)
```

### Tipografías
- **Números grandes**: `Big Shoulders Display` (fuente condensed, impactante)
- **Texto general**: `Ubuntu` (fuente de marca de Estación R)

---

## Estructura del Script R

### 1. Librerías necesarias
```r
library(ggplot2)
library(showtext)
library(png)
library(grid)
```

### 2. Configuración de fuentes
```r
font_add_google("Ubuntu", "ubuntu")
font_add_google("Big Shoulders Display", "bigshoulders")
showtext_auto()
```

### 3. Cargar logo (si se usa)
```r
img_logo <- readPNG("img/Logo_PNG_Baja_Mesa de trabajo 1 copia.png")
```

### 4. Template base para cada slide
```r
p <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    panel.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off")
```

### 5. Guardar imagen
```r
ggsave("img/carrusel/01_slide.png", p, width = 10, height = 10, dpi = 108, bg = color_fondo)
```

---

## Guía de Tamaños de Texto

| Elemento | Tamaño | Familia | Estilo |
|----------|--------|---------|--------|
| Número grande (protagonista) | 80-100 | bigshoulders | bold |
| Número secundario | 30-40 | bigshoulders | bold |
| Título principal | 20-22 | ubuntu | bold |
| Subtítulo | 11-14 | ubuntu | normal |
| Indicador de slide (X/7) | 8 | ubuntu | normal |
| CTA / instrucciones | 10-12 | ubuntu | normal |

---

## Posicionamiento Vertical (eje Y)

El canvas va de 0 a 10. Distribución típica:

```
Y = 9.5 - 7.5  → Logo (si se usa)
Y = 7.5 - 6.5  → Título superior
Y = 6.5 - 4.0  → Número grande / contenido principal
Y = 4.0 - 2.5  → Subtítulo / detalles
Y = 2.5 - 1.0  → Información adicional
Y = 0.5        → Indicador de slide (X/7)
```

---

## Tipos de Slides

### Slide con número protagonista
```r
annotate("text", x = 5, y = 6.5, label = "4",
         size = 100, color = color_primario, fontface = "bold", family = "bigshoulders") +
annotate("text", x = 5, y = 4.2, label = "cursos nuevos",
         size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
annotate("text", x = 5, y = 2.8, label = "Detalle adicional",
         size = 12, color = color_gris, family = "ubuntu")
```

### Slide con título y lista
```r
annotate("text", x = 5, y = 7.8, label = "Título",
         size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
annotate("text", x = 5, y = 6.8, label = "subtítulo",
         size = 22, color = color_texto, fontface = "bold", family = "ubuntu") +
annotate("text", x = 5, y = 4.8, label = "Item 1",
         size = 14, color = color_primario, family = "ubuntu") +
annotate("text", x = 5, y = 4.0, label = "Item 2",
         size = 14, color = color_primario, family = "ubuntu")
```

### Slide con logo
```r
annotation_custom(
  rasterGrob(img_logo, interpolate = TRUE),
  xmin = 3, xmax = 7, ymin = 7.5, ymax = 9.5
)
```

---

## Ejecución

### Desde terminal (recomendado)
```bash
cd "/ruta/al/proyecto"
Rscript carrusel_instagram.R
```

### Desde RStudio
Descomentar las líneas de `setwd()` si se ejecuta desde RStudio:
```r
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)
```

---

## Tiempos de Ejecución

- Primera ejecución: ~2 minutos (descarga de fuentes de Google)
- Ejecuciones posteriores: ~30 segundos

---

## Archivos de Referencia

- **Script completo**: `carrusel_instagram.R`
- **Logo blanco**: `img/Logo_PNG_Baja_Mesa de trabajo 1 copia.png`
- **Salida**: `img/carrusel/*.png`

---

## Tips

1. **Saltos de línea**: Usar `\n` dentro del label y agregar `lineheight = 1.3`
2. **Centrado**: Siempre usar `x = 5` para centrar horizontalmente
3. **Pruebas rápidas**: Generar solo 1 slide comentando los demás para iterar más rápido
4. **Caracteres especiales**: La fuente Ubuntu soporta acentos y ñ correctamente

---

## Ejemplo Mínimo

```r
library(ggplot2)
library(showtext)

font_add_google("Ubuntu", "ubuntu")
font_add_google("Big Shoulders Display", "bigshoulders")
showtext_auto()

color_fondo <- "#1a1a2e"
color_primario <- "#3366FF"
color_texto <- "#ffffff"

p <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_fondo, color = NA),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  annotate("text", x = 5, y = 6, label = "42",
           size = 100, color = color_primario, fontface = "bold", family = "bigshoulders") +
  annotate("text", x = 5, y = 3.5, label = "respuestas",
           size = 22, color = color_texto, fontface = "bold", family = "ubuntu")

ggsave("test.png", p, width = 10, height = 10, dpi = 108, bg = color_fondo)
```

---

*Documento creado: Diciembre 2025*
*Última actualización: Balance Anual 2025*
