plot_coefficients <- function(data, item.col, point.coeficient, lwr.ci, up.ci, 
                              selected.items = NULL, 
                              x.label = "Items", y.label = "Coefficient", 
                              title = "Coefficient Plot") {
  # Validar existencia de columnas
  required_cols <- c(item.col, point.coeficient, lwr.ci, up.ci)
  if (!all(required_cols %in% colnames(data))) {
    stop("El data.frame no contiene todas las columnas especificadas.")
  }
  
  # Filtrar si el usuario selecciona ítems específicos
  if (!is.null(selected.items)) {
    data <- data[data[[item.col]] %in% selected.items, ]
    
    if (nrow(data) == 0) {
      stop("Ningun ítem seleccionado está en los datos. Revisa los nombres.")
    }
  }
  
  # Convertir Item a factor ORDENADO para evitar problemas de truncamiento
  data[[item.col]] <- factor(data[[item.col]], levels = unique(data[[item.col]]), ordered = TRUE)
  
  # Crear el grafico con ggplot2
  library(ggplot2)
  p <- ggplot(data, aes_string(x = item.col, y = point.coeficient)) +
    geom_point(size = 3) +
    geom_errorbar(aes_string(ymin = lwr.ci, ymax = up.ci), width = 0.2) +
    labs(
      title = title,
      x = x.label,
      y = y.label
    ) +
    theme_minimal()
  
  return(p)
}
