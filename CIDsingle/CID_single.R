CID_single <- function(coef1, coef2, lci1, uci1, lci2, uci2) {
  # Validar que todos los argumentos sean numéricos
  if (!all(sapply(list(coef1, coef2, lci1, uci1, lci2, uci2), is.numeric))) {
    stop("Todos los argumentos deben ser valores numéricos.")
  }
  
  # Validar que los límites sean coherentes
  if (!(lci1 <= coef1 && coef1 <= uci1)) stop("El coeficiente 1 no está dentro de su intervalo de confianza.")
  if (!(lci2 <= coef2 && coef2 <= uci2)) stop("El coeficiente 2 no está dentro de su intervalo de confianza.")
  
  # Calcular la diferencia entre los coeficientes
  difference <- coef1 - coef2
  
  # Calcular el intervalo de confianza para la diferencia usando MOVER
  lower_diff <- difference - sqrt((coef1 - lci1)^2 + (uci2 - coef2)^2)
  upper_diff <- difference + sqrt((uci1 - coef1)^2 + (coef2 - lci2)^2)
  
  # Retornar los resultados
  return(data.frame(
    Difference = round(difference, 3),
    lwr.ci = round(lower_diff, 3),
    upr.ci = round(upper_diff, 3)
  ))
}
