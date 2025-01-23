
## H Aiken ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\Hg\\HAiken.R")

Haiken(data = ej1, ncat = 5, conf.level = .90)
Haiken(data = ej2, ncat = 5, conf.level = .90)

## V Aiken ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\Vaiken\\Vaiken.R")

Vaiken(data = ej1, min = 1, max = 6, conf.level = .90)
Vaiken(data = ej2, min = 1, max = 5, conf.level = .90)
Vaiken(data = ej4, min = 1, max = 5, conf.level = .90)


## CVR ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\CVR\\CVR.R")
ej5 <- data.frame(
  j1 = 3,
  j2 = sample(2:3, 10, replace = TRUE),
  j3 = 3,
  j4 = sample(2:3, 10, replace = TRUE))

CVR(data = ej5)

## CVC ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\CVC\\CVC.R")

CVC(data = ej1, max = 6, conf.level = .90)
CVC(data = ej2, max = 5, conf.level = .90)
CVC(data = ej4, max = 5, conf.level = .90)
CVC(data = ej5, max = 5, conf.level = .90)

## CVI ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\CVI\\CVI.R")


CVI(data = ej1, cut = 4, conf.level = .90)
CVI(data = ej1, cut = 5, conf.level = .90)
CVI(data = ej2, cut = 4, conf.level = .90)
CVI(data = ej5, cut = 3, conf.level = .90)
CVI(data = random_data3, cut = 6, conf.level = .90)
CVI(data = random_data2, cut = 2, conf.level = .90)

## CVI_R ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\CVIR\\CVI_R.R")

CVI_R(data = ej1, cut = 4, conf.level = .90)
CVI_R(data = ej1, cut = 5, conf.level = .90)
CVI_R(data = ej1, cut = 6, conf.level = .90)

CVI_R(data = ej2, cut = 4, conf.level = .90)
CVI_R(data = random_data3, cut = 6, conf.level = .90)
CVI_R(data = random_data2, cut = 2, conf.level = .90)

## MER ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\MER\\MER.R")

MER(data = ej1, ncat = 6, start = 1, conf.level = .90)
MER(data = ej2, ncat = 6, start = 1, conf.level = .90)


MER(data = random_data1, ncat = 5, start = 1, conf.level = .90)
MER(data = random_data2, ncat = 3, start = 1, conf.level = .90)
MER(data = random_data3, ncat = 7, start = 1, conf.level = .90)

## Vsubs single item ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\Vsubs\\vsubs_single.R")

vsub_single(item = data.gais$gais2, conf.level = .90)
vsub_single(item = data.gais$gais5, conf.level = .90)

vsub_single(item = data.gais$gais3, conf.level = .90)
DescTools::MultinomCI(table(data.gais$gais3), 
                      method = "wilson",
                      conf.level = .90)

## Vsubs multiple items ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\Vsubs\\vsubs_mult.R")

vsub_mult(data = data.gais, columns = c("gais2", "gais3"), conf.level = .90)
vsub_mult(data = data.gais, columns = 3:4, conf.level = .90)
vsub_mult(data = data.gais, columns = 1:4, conf.level = .90)

## CID ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\CID\\CID.R")

# CID: CVC
cvc.nse1 <- CVC(data = random_data3, max = 7,conf.level = .90)
cvc.nse2 <- CVC(data = random_data2, max = 7, conf.level = .90)

CID(group1 = cvc.nse1, 
          group2 = cvc.nse2, 
          coef.col = "CVC", 
          lwr.col = "lwr.ci", 
          upr.col = "upr.ci")

# CID: MER
mer.grop1 <- MER(data = random_data3, ncat = 7, start = 1, conf.level = .95)
mer.grop2 <- MER(data = random_data2, ncat = 7, start = 1, conf.level = .95)

CID(group1 = mer.grop1, 
    group2 = mer.grop2,
    coef.col = "MER", 
    lwr.col = "lwr.ci", 
    upr.col = "upr.ci")

# CID: V
output.V1 <- Vaiken(data = random_data2, min = 1, max = 7, conf.level = .90)
output.V2 <- Vaiken(data = random_data3, min = 1, max = 7, conf.level = .90)

CID(group1 = output.V2, 
    group2 = output.V1,
    coef.col = "V", 
    lwr.col = "lwr.ci", 
    upr.col = "upr.ci")


## CIR ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\CIR\\CIR.R")

# CIR: V
CIR(group1 = output.V2, 
    group2 = output.V1, 
    coef.col = "V", 
    lwr.col = "lwr.ci", 
    upr.col = "upr.ci")

# CIR: 
CIR(group1 = cvc.nse1, 
    group2 = cvc.nse2, 
    coef.col = "CVC", 
    lwr.col = "lwr.ci", 
    upr.col = "upr.ci")

## Example: Transposing columns (currently, items) to files (currently, raters)
# Data
Group1 <- data.frame(
  item1 = sample(4:5, 10, replace = TRUE),
  item2 = sample(4:5, 10, replace = TRUE),
  item3 = sample(4:5, 10, replace = TRUE),
  item4 = sample(4:5, 10, replace = TRUE))

Group2 <- data.frame(
  item1 = sample(4:5, 10, replace = TRUE),
  item2 = sample(4:5, 10, replace = TRUE),
  item3 = sample(4:5, 10, replace = TRUE),
  item4 = sample(4:5, 10, replace = TRUE))

# Transposing 
Group1.t <- t(Group1)
Group2.t <- t(Group2)

# Naming columns (now, raters)
colnames(Group1.t) <- paste("judge",sep = "", seq(1:10))
colnames(Group2.t) <- paste("judge",sep = "", seq(1:10))

# Run MER, with data as data.frame
mer.group1 <- MER(data = as.data.frame(Group1.t), 
                  ncat = 5, 
                  start = 1, 
                  conf.level = .95)

mer.group2 <- MER(data = as.data.frame(Group2.t), 
                  ncat = 5, 
                  start = 1, 
                  conf.level = .95)

CID(group1 = mer.group1, 
    group2 = mer.group2,
    coef.col = "MER", 
    lwr.col = "lwr.ci", 
    upr.col = "upr.ci")

CIR(group1 = mer.group1,
    group2 = mer.group2,
    coef.col = "MER",
    lwr.col = "lwr.ci",
    upr.col = "upr.ci")

## CID general ####
source(file = "F:\\carpetasMODIFICADAS\\INVESTIGACION\\Aiken_2do-round\\Livia\\VContenido.R\\CID_general\\CID_mover.R")
cid_mover(coef1 = .654, 
          coef2 = .799, 
          lci1 = .601, 
          uci1 = .732, 
          lci2 = .701, 
          uci2 = .844)

clipr::write_clip(content = gais_scale)

###
#### Ejemplos ####

## CVR critico: Lawshe
CVRcut_Lawshe(num_jueces = 4, interpolacion = "none")
CVRcut_Lawshe(num_jueces = 4, interpolacion = "splin")
CVRcut_Lawshe(num_jueces = 4, interpolacion = "linear")

CVRcut_Lawshe(num_jueces = 22, interpolacion = "none")
CVRcut_Lawshe(num_jueces = 22, interpolacion = "linear")
CVRcut_Lawshe(num_jueces = 20, interpolacion = "splin")


## CVR critico: Wilson
CVRcut_Wilson(num_jueces = 4, alpha = .05, tails = "one")
CVRcut_Wilson(num_jueces = 5, alpha = .05, tails = "one")
CVRcut_Wilson(num_jueces = 22, alpha = .05, tails = "one")
CVRcut_Wilson(num_jueces = 40, alpha = .05, tails = "one")
CVRcut_Wilson(num_jueces = 40, alpha = .01, tails = "one")

## CVR critico: Ayres
CVRcut_Ayres(num_jueces = 4)
CVRcut_Ayres(num_jueces = 5)
CVRcut_Ayres(num_jueces = 6)
CVRcut_Ayres(num_jueces = 7)
CVRcut_Ayres(num_jueces = 8)
CVRcut_Ayres(num_jueces = 22)
CVRcut_Ayres(num_jueces = 40, alpha = .05)

## CVR critico: Baghestani
CVRcut_Baghestani(num_jueces = 5,prior = "Uniform",alpha = .05)
CVRcut_Baghestani(num_jueces = 22,prior = "Uniform",alpha = .05)
CVRcut_Baghestani(num_jueces = 5,prior = "Uniform",alpha = .05)
CVRcut_Baghestani(num_jueces = 40,prior = "Uniform",alpha = .05)


CVRcut(N_min = 5, N_max = 100, method = "Wilson", alpha = 0.05, tails = "one",prior = "Uniform")
