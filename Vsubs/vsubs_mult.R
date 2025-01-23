vsub_mult <- function(data, columns, conf.level = 0.95) {
  # Subfunción interna para calcular PSA y CSV
  vsub_sing_internal <- function(item, conf.level) {
    # Crear tabla de frecuencias
    freq_table <- as.data.frame(table(item))
    colnames(freq_table) <- c("Cat", "Freq")
    freq_table$Cat <- as.numeric(as.character(freq_table$Cat))  # Asegurar categorías numéricas
    
    # Total de respuestas
    Ntotal <- sum(freq_table$Freq)
    
    # PSA
    psa_results <- freq_table
    psa_results$psa <- round(freq_table$Freq / Ntotal, 3)
    psa_ci <- scoreci(freq_table$Freq, Ntotal, conf.level)
    psa_results <- cbind(psa_results, psa_ci)
    
    # CSV
    csv_results <- freq_table
    csv_results$csv <- round((freq_table$Freq - (Ntotal - freq_table$Freq)) / Ntotal, 3)
    
    # Evitar errores en el intervalo de confianza con magnitud
    csv_abs <- abs(csv_results$csv)
    csv_ci <- scoreci(csv_abs * Ntotal, Ntotal, conf.level)  # Usar valor absoluto para CI
    csv_ci <- csv_ci * sign(csv_results$csv)  # Restaurar el signo en los límites
    
    csv_results <- cbind(csv_results, csv_ci)
    
    # Resultados para PSA y CSV
    return(list(psa_results = psa_results, csv_results = csv_results))
  }
  
  # Si columns son índices, convertirlos a nombres
  if (is.numeric(columns)) {
    columns <- colnames(data)[columns]
  }
  
  # Aplicar la subfunción a cada columna especificada
  results <- lapply(columns, function(col) {
    item <- data[[col]]
    vsub_sing_internal(item, conf.level)
  })
  
  # Nombrar los resultados con los nombres de las columnas
  names(results) <- columns
  
  return(results)
}

# Subfunción para calcular intervalos de confianza con el método de Wilson
scoreci <- function(x, n, conf.level) {
  zalpha <- abs(qnorm((1 - conf.level) / 2))
  phat <- x / n
  bound <- (zalpha * sqrt((phat * (1 - phat) + (zalpha^2) / (4 * n)) / n)) /
    (1 + (zalpha^2) / n)
  midpnt <- (phat + (zalpha^2) / (2 * n)) / (1 + (zalpha^2) / n)
  
  up.ci <- round(midpnt + bound, digits = 3)
  lwr.ci <- round(midpnt - bound, digits = 3)
  
  return(data.frame(lwr.ci = lwr.ci, up.ci = up.ci))
}

