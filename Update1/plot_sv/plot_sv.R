#### plot substantive validity ####
plot_vsub_results <- function(results, type = "psa") {
  # Validar el tipo de gráfico
  if (!type %in% c("psa", "csv")) {
    stop("El argumento 'type' debe ser 'psa' o 'csv'.")
  }
  
  # Preparar datos según el tipo
  plot_data <- do.call(rbind, lapply(names(results), function(col_name) {
    result <- if (type == "psa") results[[col_name]]$psa_results else results[[col_name]]$csv_results
    result$Column <- col_name  # Añadir nombre de columna
    return(result)
  }))
  
  # Crear el gráfico con ggplot2
  library(ggplot2)
  p <- ggplot(plot_data, aes(x = as.factor(Cat), y = if (type == "psa") psa else csv)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr.ci, ymax = up.ci), width = 0.2) +
    facet_wrap(~ Column, scales = "free_y") +
    labs(
      title = if (type == "psa") "PSA con Intervalos de Confianza" else "CSV con Intervalos de Confianza",
      x = "Categorías",
      y = if (type == "psa") "Proporción (PSA)" else "Coeficiente (CSV)"
    ) +
    theme_minimal()
  
  return(p)
}