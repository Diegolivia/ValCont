#### Plot para resultados de coef de validez contenido ####
plot_coefficients <- function(data, point.coeficient, lwr.ci, up.ci, x.label = "Categories", y.label = "Coefficient", title = "Coefficient Plot") {
  # Validar existencia de columnas
  required_cols <- c(point.coeficient, lwr.ci, up.ci)
  if (!all(required_cols %in% colnames(data))) {
    stop("El data.frame no contiene todas las columnas especificadas.")
  }
  
  # Crear el gráfico con ggplot2
  library(ggplot2)
  p <- ggplot(data, aes_string(x = "Cat", y = point.coeficient)) +
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
