### Proyecto: Visualizaciónde Datos con R - Tasa de mortalidad infantil en México

### Autor: Pablo Tiscornia - Estación R
### Fuente: INEGI - https://www.inegi.org.mx/app/tabulados/interactivos/?px=Mortalidad_02&bd=Mortalidad

### Cargo librerías
library(tidyverse)
library(janitor)
library(geomtextpath)

### Cargo datos
df_museos_mx <- read_csv("data/data-museos_mx.csv") |>
  clean_names()
