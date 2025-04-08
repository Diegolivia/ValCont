

########################
## Example: 5 items (in columns), 30 judges (in rows)
# (Data para Vsubs function)

data.gais <- read.csv("F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\ejemplos\\ej5.txt",
                header=T,
                sep = "")


## Ejemplo 1: 15 items (in rows), 15 jueces (in columna)
# Without header columns
ej1 <- read.csv("F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\ejemplos\\ej1.txt",
                header=FALSE,
                sep = "")

## Ejemplo 2: 5 items (filas), 6 jueces (columnas)
# Witht header columns
ej2 <- read.csv("F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\ejemplos\\ej2.txt",
                header=T,
                sep = "")


## Example 3: Random data, 12 items (in rows), 6 jueces (in columns)
ej3 <- data.frame(replicate(n = 6,sample(1:5,12,rep=TRUE)))


## Example 4: 12 items (in rows), 6 jueces (in columns)
ej4 <- load(ej4)

## Random data: 11 items (in rows), 4 jueces (in columns)
#Low ratings
randata_low <- data.frame(
  juez1 = sample(1:2, 11, replace = TRUE),
  juez2 = sample(2:3, 11, replace = TRUE),
  juez3 = sample(1:2, 11, replace = TRUE),
  juez4 = sample(1:3, 11, replace = TRUE))

## Random data: 10 items (filas), 6 jueces (columnas)
#High ratings
randata_high <- data.frame(
  obs1 = sample(5:7, 10, replace = TRUE),
  obs2 = sample(4:7, 10, replace = TRUE),
  obs3 = sample(4:7, 10, replace = TRUE),
  obs4 = sample(5:7, 10, replace = TRUE),
  obs5 = sample(5:7, 10, replace = TRUE),
  obs6 = sample(6:7, 10, replace = TRUE))

## Random data for CVR: 10 items (filas), 6 jueces (columnas)
#High ratings
randata.CVR <- data.frame(
  obs1 = sample(3:3, 10, replace = TRUE),
  obs2 = sample(3:3, 10, replace = TRUE),
  obs3 = sample(2:3, 10, replace = TRUE),
  obs4 = sample(3:3, 10, replace = TRUE),
  obs5 = sample(2:3, 10, replace = TRUE),
  obs6 = sample(1:3, 10, replace = TRUE))
