CID <- function(group1, group2, coef.col = "coef", lwr.col = "lwr.ci", upr.col = "upr.ci") {
  # Validar que los argumentos son data.frames
  if (!is.data.frame(group1) || !is.data.frame(group2)) {
    stop("Ambos argumentos 'group1' y 'group2' deben ser data.frames.")
  }
  
  # Validar que las columnas necesarias existen en los data.frames
  required_cols <- c(coef.col, lwr.col, upr.col)
  if (!all(required_cols %in% colnames(group1))) {
    stop("El data.frame 'group1' no contiene las columnas necesarias.")
  }
  if (!all(required_cols %in% colnames(group2))) {
    stop("El data.frame 'group2' no contiene las columnas necesarias.")
  }
  
  # Combinar los datos para realizar las comparaciones
  combined_data <- merge(
    group1,
    group2,
    by = "row.names",
    suffixes = c("_1", "_2")
  )
  rownames(combined_data) <- combined_data$Row.names
  combined_data$Row.names <- NULL
  
  # Verificar si hubo exclusiones debido a ítems ausentes en un grupo
  if (nrow(combined_data) < nrow(group1) || nrow(combined_data) < nrow(group2)) {
    warning("Algunos items no tienen correspondencia entre los grupos y han sido excluidos de la comparacion.")
  }
  
  # Calcular la diferencia de los coeficientes y los intervalos de confianza
  combined_data <- within(combined_data, {
    Delta <- round(get(paste0(coef.col, "_1")) - get(paste0(coef.col, "_2")), 3)
    lwr.ci <- round(
      Delta - sqrt((get(paste0(coef.col, "_1")) - get(paste0(lwr.col, "_1")))^2 +
                          (get(paste0(upr.col, "_2")) - get(paste0(coef.col, "_2")))^2), 3)
    upr.ci <- round(
      Delta + sqrt((get(paste0(upr.col, "_1")) - get(paste0(coef.col, "_1")))^2 +
                          (get(paste0(coef.col, "_2")) - get(paste0(lwr.col, "_2")))^2), 3)
  })
  
  # Seleccionar las columnas finales para el reporte y reiniciar nombres de filas
  result <- combined_data[, c("Delta", "lwr.ci", "upr.ci")]
  row.names(result) <- NULL
  
  return(result)
}
