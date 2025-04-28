# Librerías
library(tidyverse)
#devtools::install_github("holatam/eph")
library(eph)
library(sf)
library(ggmap)
library(hrbrthemes)
#remotes::install_github("ewenme/ghibli")
library(ghibli)
library(kableExtra)
library(DataExplorer)
library(skimr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               Carga de datos                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b_inmuebles_apn <- read_csv("https://infra.datos.gob.ar/catalog/otros/dataset/6/distribution/6.1/download/inmuebles-estado-nacional.csv")


skim(b_inmuebles_apn)

calculate_tabulates(b_inmuebles_apn, "pais") %>% 
  arrange(-Freq) %>% 
  ggplot(aes(x = reorder(pais, Freq), y = Freq)) +
  geom_col(fill = "forestgreen", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = Freq), hjust = -0.2, size = 3) +
  labs(title = "Cantidad de inmuebles del Estado Nacional Argentino por país",
       caption= "Fuente: https://datos.gob.ar/",
       fill = "", x = "País", y = "Cantidad") +
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold"))

b_inmuebles_apn %>% 
  filter(pais == "Argentina") %>% 
  calculate_tabulates( "provincia", add.percentage = "col") %>% 
  mutate(Freq = as.numeric(Freq)) %>% 
  arrange(-Freq) %>% 
  ggplot(aes(x = reorder(provincia, Freq), y = Freq)) +
  geom_col(fill = "forestgreen", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = paste0(Freq, "%"), y = Freq + 0.5), hjust = -0.1, size = 3) +
  labs(title = "Distribución porcentual de los inmuebles del Estado \n Nacional Argentino por provincia",
       caption= "Fuente: https://datos.gob.ar/",
       fill = "", x = "Provincia", y = "Porcentaje") +
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold")) +
  #scale_y_continuous(limits = c(0, 110), breaks = c(0, 20, 40, 60, 80 ,100))
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 20, 40, 60, 80, 100))

b_inmuebles_apn_caba <- b_inmuebles_apn %>% 
  filter(provincia == "Ciudad Autonoma de Buenos Aires")

capa_comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')

# Hacemos al objeto uno de tipo espacial
capa_inmuebles_caba <- b_inmuebles_apn_caba %>% 
  filter(!is.na(longitud), !is.na(latitud), !is.na(departamento)) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
  mutate(lat = st_coordinates(.)[,1],
         lon = st_coordinates(.)[,2])

ggplot() +
  geom_sf(data = capa_comunas) +
  geom_sf(data = capa_inmuebles_caba, color = "forestgreen", alpha = 0.4) +
  labs(title = "Distribución geográfica de los inmuebles del Estado \n Nacional Argentino.",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption= "Fuente: https://datos.gob.ar/",
       fill = "", x = "Provincia", y = "Cantidad") +
  theme_void() +
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold"))

b_inmuebles_apn_caba <- b_inmuebles_apn_caba %>% 
  mutate(calle_numero = paste0(calle, "_", numero)) %>% 
  mutate(duplicados_dir   = ifelse(duplicated(calle_numero) == TRUE, 1, 2),
         duplicados_coord = ifelse(duplicated(longitud, latitud) == TRUE, 1 ,2))

b_inmuebles_apn_caba %>% 
  filter(duplicados_dir == 1 | duplicados_coord == 1) %>%  
  select(codigo_del_inmueble, calle_numero,  departamento, 
         superficie_aproximada_m2, longitud, latitud, duplicados_dir, duplicados_coord) %>% 
  arrange(codigo_del_inmueble)

# Remuevo casos duplicados, quedandome con los unicos
b_inmuebles_apn_caba <- b_inmuebles_apn_caba %>% 
  distinct(calle_numero,  .keep_all = TRUE)

# Hacemos al objeto uno de tipo espacial y remuevo los duplicados por coordenadas
capa_inmuebles_caba <- b_inmuebles_apn_caba %>% 
  filter(!is.na(longitud), !is.na(latitud), !is.na(departamento)) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
  mutate(lat = st_coordinates(.)[,1],
         lon = st_coordinates(.)[,2]) %>% 
  rename(superficie = superficie_aproximada_m2) %>% 
  distinct(geometry,  .keep_all = TRUE)

ggplot() +
  geom_sf(data = capa_comunas) +
  geom_sf(data = capa_inmuebles_caba, color = "forestgreen", alpha = 0.4, 
          aes(size = as.numeric(superficie))) +
  labs(title = "Distribución geográfica de los inmuebles del Estado \n Nacional Argentino por superficie del inmueble.",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption= "Fuente: https://datos.gob.ar/",
       fill = "", x = "Provincia", y = "Cantidad") +
  theme_void() +
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold"), 
        legend.position = "none")

# Frecuencia
calculate_tabulates(capa_inmuebles_caba, "departamento") %>% 
  arrange(-as.numeric(Freq)) %>% 
  rename(Cantidad = Freq, Comuna = departamento)

#Vemos que hay edificios sin dato en departamento
#unique(capa_inmuebles_caba$departamento)

# Los quitamos de la base
capa_inmuebles_caba <- capa_inmuebles_caba %>% 
  filter(departamento != "")

# Tabla
porc_comuna <- capa_inmuebles_caba %>% 
  calculate_tabulates(x = "departamento", add.percentage = "col")



# Gráfico
ggplot(data = porc_comuna) + 
  geom_col(aes(x = reorder(departamento, as.numeric(Freq)), y = as.numeric(Freq),
               fill = ifelse(departamento == "Comuna 1", "A","B"))) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(A="#AF9699FF", B="#F3E8CCFF")) +
  geom_text(aes(x = reorder(departamento, as.numeric(Freq)), y = as.numeric(Freq), 
                label = paste0(Freq, "%")),
            hjust = "inward") +
  labs(title = "Distribución porcentual de los inmuebles del Estado Nacional \n por Comuna",
       x = "Comuna", y = "Porcentaje",
       caption = "Fuente: Jefatura de Gabinete de Ministros. Agencia de Administración de Bienes del Estado. Dirección Nacional del Registro de Bienes Inmuebles.",
       fill = "") +
  coord_flip() +
  theme_minimal()+
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold"), 
        legend.position = "none")



inmuebles_x_comuna <- capa_inmuebles_caba %>%
  mutate(comunas = case_when(departamento == "Comuna 1" ~ 1, departamento == "Comuna 2" ~ 2, 
                             departamento == "Comuna 3" ~ 3, departamento == "Comuna 4" ~ 4, 
                             departamento == "Comuna 5" ~ 5, departamento == "Comuna 6" ~ 6,
                             departamento == "Comuna 7" ~ 7, departamento == "Comuna 8" ~ 8, 
                             departamento == "Comuna 9" ~ 9, departamento == "Comuna 10" ~ 10,
                             departamento == "Comuna 11" ~ 11, departamento == "Comuna 12" ~ 12,
                             departamento == "Comuna 13" ~ 13, departamento == "Comuna 14" ~ 14,
                             departamento == "Comuna 15" ~ 15)) %>% 
  filter(!is.na(comunas)) %>%
  group_by(comunas) %>%
  summarise(cantidad=n()) %>% 
  st_set_geometry(NULL)

capa_comunas <- capa_comunas %>%
  #mutate(comunas = as.numeric(levels(comunas))[comunas]) %>%
  mutate(comunas = as.numeric(comunas)) %>% 
  left_join(inmuebles_x_comuna, by = "comunas")

ggplot() +
  geom_sf(data = capa_comunas, aes(fill=cantidad), color = NA) +
  geom_sf_text(data = capa_comunas, aes(label = comunas), size=2.5, colour = "black") +
  labs(title = "Inmuebles Propios del Estado Nacional y Alquilados",
       subtitle = "Densidad de propiedades",
       fill = "Cantidad",
       caption= "Fuente: https://datos.gob.ar/") +
  theme_void() +
  scale_fill_distiller(palette = "Spectral")


capa_radios <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")

# Join de capa inmuebles y capa de radios + creo variable de cantidad
inmuebles_x_radio <- st_join(capa_inmuebles_caba, capa_radios) %>% 
  group_by(RADIO_ID) %>%
  summarise(cantidad=n()) %>% 
  st_set_geometry(NULL)

# Llevo variable de cantidad a la capa de radios
capa_radios <- capa_radios %>%
  left_join(inmuebles_x_radio, by = "RADIO_ID")

# Visualizo distribución de inmuebles por radio censal
ggplot() +
  geom_sf(data = capa_radios, aes(fill=cantidad), color = NA) +
  labs(title = "Inmuebles Propios del Estado Nacional y Alquilados",
       subtitle = "Propiedades publicadas",
       fill = "Cantidad",
       caption= "Fuente: xxx") +
  theme_void() +
  scale_fill_distiller(palette = "Spectral")


ggplot() +
  geom_sf(data = capa_radios, aes(fill=cantidad)) +
  labs(title = "Inmuebles Propios del Estado Nacional y Alquilados",
       subtitle = "Propiedades publicadas",
       fill = "Cantidad",
       caption= "Fuente: xxx") +
  theme_void() +
  coord_sf(xlim = c(-58.39, -58.35), ylim = c(-34.63, -34.56), expand = FALSE)+
  scale_fill_distiller(palette = "Spectral")


bbox <- make_bbox(lon = b_inmuebles_apn_caba$longitud, lat = b_inmuebles_apn_caba$latitud)

bbox

CABA <- get_stamenmap(bbox, zoom = 11, maptype = "terrain")

# ggmap(CABA)

### Para replicar
theme_caba_map <- theme (plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"), #ajustar los margenes del gráfico
                         title=element_text(size=10, face = "bold"), #tamaño de titulo del mapa
                         legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
                         legend.key.width = unit(0.4,"cm"), #ancho de cuadrados de referencia 
                         legend.position="right", #ubicacion de leyenda
                         legend.direction = "vertical", #dirección de la leyenda
                         legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
                         legend.text=element_text(size=7), #tamaño de texto de leyenda
                         plot.caption=element_text(face = "italic", colour = "gray35",size=6), #tamaño de nota al pie
                         axis.text = element_blank(), #texto eje X e Y
                         axis.ticks = element_blank())

ggmap(CABA) +
  geom_point(data = b_inmuebles_apn_caba, aes(x = longitud, y = latitud),
             color = "forestgreen", alpha = .5) +
  stat_density_2d(data = b_inmuebles_apn_caba, aes(x = longitud, y = latitud, 
                                                   fill = stat(level)),alpha = .4,
                  bins = 25,
                  geom = "polygon") +
  labs(title="Distribución de los inmuebles del Estado Nacional",
       subtitle="Ciudad Autónoma de Buenos Aires",
       x="",
       y="",
       caption= "Fuente: https://datos.gob.ar/",
       fill="Nivel")+
  scale_fill_distiller(palette = "Spectral") +
  #scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, "YlOrRd"))
  #theme_caba_c
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y.left = element_blank(),
        plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        legend.position = "none",
        title=element_text(size=10, face = "bold"))
