# Datos ----      
load("act24_al_13nov.RData")


##Nueva variable---- 
act$factor= rep(1, 937)

#Librerías ----
library(dplyr)
library(survey)
library(labelled)


# Declaración del modelo 
disenyo <- svydesign(ids=~1, data=act, weights = ~factor)
class(disenyo)
#Tablas ----



#Gráficos ----


