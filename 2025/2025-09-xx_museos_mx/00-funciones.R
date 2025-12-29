fn_serie_tiempo <- function(base, var_entidad) {
  base |>
    filter(entidad == var_entidad) |>
    ggplot(aes(x = anio, y = tasa_mortalidad_infantil, color = entidad)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    ylim()
  labs(
    title = "Tasa de mortalidad infantil (por cada 1.000 nacidos vivos)",
    subtitle = "Mexico, serie 2000-2023",
    x = "Año",
    y = "Tasa de mortalidad infantil",
    color = "Entidad"
  ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.position = "none"
    ) +
    facet_wrap(~entidad, scales = "free_y")
}

fn_serie_tiempo(base = df_mort_inf_mx_trab, var_entidad = "Total")
