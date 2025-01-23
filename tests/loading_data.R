########################
## Ejemplo: 5 items columnas), 30 jueces (filas)
# Data para Vsubs funciones

data.gais <- read.csv("F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\ejemplos\\ej5.txt",
                header=T,
                sep = "")


## Ejemplo: 15 items (filas), 15 jueces (columnas)
# Without header columns
ej1 <- read.csv("F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\ejemplos\\ej1.txt",
                header=FALSE,
                sep = "")

## Ejemplo: 5 items (filas), 6 jueces (columnas)
# Witht header columns
ej2 <- read.csv("F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\ejemplos\\ej2.txt",
                header=T,
                sep = "")


## Random data: 12 items (filas), 6 jueces (columnas)
random_data1 <- data.frame(replicate(n = 6,sample(1:5,12,rep=TRUE)))


## Random data: 11 items (filas), 4 jueces (columnas)
#Low ratings
random_data2 <- data.frame(
  juez1 = sample(1:2, 11, replace = TRUE),
  juez2 = sample(2:3, 11, replace = TRUE),
  juez3 = sample(1:2, 11, replace = TRUE),
  juez4 = sample(1:3, 11, replace = TRUE))

## Random data: 10 items (filas), 6 jueces (columnas)
#High ratings
random_data3 <- data.frame(
  obs1 = sample(5:7, 10, replace = TRUE),
  obs2 = sample(4:7, 10, replace = TRUE),
  obs3 = sample(4:7, 10, replace = TRUE),
  obs4 = sample(5:7, 10, replace = TRUE),
  obs5 = sample(5:7, 10, replace = TRUE),
  obs6 = sample(6:7, 10, replace = TRUE))
