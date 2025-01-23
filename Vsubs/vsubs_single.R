vsub_single <- function(item, conf.level = 0.95)
{
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
  # Función principal  
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
  
  # SVC
  svc_results <- freq_table
  svc_results$svc <- round((freq_table$Freq - (Ntotal - freq_table$Freq)) / Ntotal, 3)
  
  # Evitar errores en el intervalo de confianza con magnitud
  svc_abs <- abs(svc_results$svc)
  svc_ci <- scoreci(svc_abs * Ntotal, Ntotal, conf.level)  # Usar valor absoluto para CI
  svc_ci <- svc_ci * sign(svc_results$svc)  # Restaurar el signo en los límites
  
  svc_results <- cbind(svc_results, svc_ci)
  
  # Reportar
  return(list("PSA" = psa_results, "SVC" = svc_results))
}
