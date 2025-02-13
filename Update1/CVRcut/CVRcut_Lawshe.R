CVRcut_Lawshe <- function(num_jueces, interpolacion = "none", legend = TRUE) {
  # Datos tabulados de Lawshe
  data <- data.frame(
    num_jueces = c(5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 40),
    cvr_critico = c(0.99, 0.99, 0.99, 0.75, 0.78, 0.62, 0.49, 0.42, 0.37, 0.33, 0.29)
  )
  
  # Interpolación si es necesario
  if (num_jueces %in% data$num_jueces) {
    cutoff <- data$cvr_critico[data$num_jueces == num_jueces]
    notas <- 3  # Tabulado
  } else if (interpolacion != "none") {
    if (interpolacion == "linear") {
      cutoff <- approx(data$num_jueces, data$cvr_critico, xout = num_jueces)$y
    } else if (interpolacion == "splin") {
      fit <- smooth.spline(data$num_jueces, data$cvr_critico)
      cutoff <- predict(fit, x = num_jueces)$y
    } else {
      cutoff <- NA
    }
    notas <- 2  # Interpolado
  } else {
    cutoff <- NA
    notas <- 1  # No tabulado y sin interpolación
  }
  
  # Calcular número mínimo de jueces
  if (!is.na(cutoff)) {
    min_jueces <- ceiling(cutoff * (num_jueces / 2) + num_jueces / 2)
  } else {
    min_jueces <- NA
  }
  
  # Resultado
  result <- data.frame(
    Method = "Lawshe",
    Cutoff = cutoff,
    MinJueces = min_jueces,
    Notas = notas
  )
  
  # Leyenda opcional
  leyenda <- data.frame(
    Código = c(1, 2, 3),
    Descripción = c("No tabulado, devuelve NA", 
                    "No tabulado, interpolado o extrapolado", 
                    "Tabulado")
  )
  
  if (legend) {
    return(list(Resultado = result, Leyenda = leyenda))
  } else {
    return(result)
  }
}}
