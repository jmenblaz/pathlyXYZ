



# tilted maps

# wrapper

plot_tiltedmaps_shared <- function(map_list, layer = NA, palette = "viridis", color = "grey50",
                                   direction = 1, begin = 0, end = 1, alpha = 1,
                                   shared_scale = TRUE) { # Nuevo parámetro

  ## [Mismos checks iniciales de tu función...]
  if(all(is.na(layer))) layer <- "value"
  if(length(layer) == 1) layer <- rep(layer, length(map_list))
  # ... (resto de reps)

  # Calcular límites comunes si se solicita
  limits <- NULL
  if(shared_scale) {
    all_values <- unlist(lapply(seq_along(map_list), function(i) {
      if(!is.na(layer[i])) return(map_list[[i]][[layer[i]]])
    }))
    limits <- range(all_values, na.rm = TRUE)
  }

  map_tilt <- ggplot2::ggplot()

  for (i in seq_along(map_list)) {
    # IMPORTANTE: Solo añadimos new_scale si NO queremos escala compartida
    if (!shared_scale && i > 1) {
      map_tilt <- map_tilt + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
    }

    if(!is.na(layer[[i]])){
      map_tilt <- map_tilt +
        ggplot2::geom_sf(
          data = map_list[[i]],
          ggplot2::aes(fill = .data[[layer[[i]]]], color = .data[[layer[[i]]]]),
          size = 0.01
        )
    } else {
      map_tilt <- map_tilt +
        ggplot2::geom_sf(data = map_list[[i]], color = color[i], alpha = alpha[i])
    }
  }

  # Aplicar la escala UNA SOLA VEZ al final (si es compartida)
  # o dejar que ggplot maneje la estética si no hay escalas nuevas
  if (shared_scale) {
    map_tilt <- map_tilt +
      ggplot2::scale_fill_viridis_c(option = palette[1], direction = direction[1],
                                    limits = limits, name = "Escala Común") +
      ggplot2::scale_color_viridis_c(option = palette[1], direction = direction[1],
                                     limits = limits, guide = "none")
  }

  map_tilt + ggplot2::theme_void()
}
