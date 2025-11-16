library(survey);library(haven); library(janitor); library(dplyr); library(stringr)


# Extrayendo datos de la ENAHO ---------------------------------
#1) designar el espacio de trabajo
setwd("C:/Users/kevin/OneDrive - Universidad de Costa Rica/Escritorio/II ciclo/Encuestas/Datos")

#2) cargar base de datos de la ENAHO
enaho <- read_dta("/Users/fernanda/Library/CloudStorage/OneDrive-UniversidaddeCostaRica/Documentos/ENCUESTA DEL CONSUMIDOR/PONDERACION/ENAHO2023.DTA")

#3) Estimaciones poblacionales de la ENAHO

enaho %>% tabyl(A4) 
enaho %>% tabyl(A5) 
enaho %>% tabyl(A14) 

enaho <- enaho %>% mutate(SD2=as_factor(A4))
enaho %>% tabyl(SD2) #sexo

#enaho <- enaho %>% mutate(A14=as_ficcor(A14))
#enaho %>% tabyl(A14) #educacion
#attributes(enaho$A14)$label

#recodificaciones
enaho <- enaho %>% mutate(edad= case_when(
  A5 < 18 ~ NA,
  A5 >= 18 & A5 <= 34  ~ 1,
  A5 >= 35 & A5 <= 49  ~ 2,
  A5 > 49 ~ 3))
enaho <- enaho %>% mutate(edad=factor(edad, labels = c("18a34", "35a49", "50+")))
enaho %>% tabyl(edad) #edad

enaho <- enaho %>% mutate(edu= case_when(
  A14 <=19 ~ 1,
  A14 >= 21 & A14 <= 49   ~ 2,
  A14 >= 51 & A14 <= 119   ~ 3, 
  A14 > 200 ~ NA ))
enaho <- enaho %>% mutate(edu=factor(edu, labels = c("primaria", "secundaria", "universitaria")))
enaho %>% tabyl(edu) #educacion


diseno.enaho <- svydesign(ids = ~nro_Vivienda, 
                          strata = ~REGION+ZONA, 
                          weights = ~FACTOR, 
                          data = subset(enaho, 
                                        A5>=18 & is.na(edad)==FALSE & 
                                          is.na(edu)==FALSE))
tabla1 <- svytable(~edu+edad+SD2, diseno.enaho); tabla1
tabla1 <- as.data.frame(tabla1); tabla1
save(tabla1, file="tabla_postest_enaho2023.Rdata")

#Lo de arriba ya lo hicimos
# Ponderacion ----------------------------------------------------------------
load("act24_al_13nov.RData") 
load("tabla_postest_enaho2024.Rdata")
#4) Imputacion 
#revisar perdidos en sexo 
act %>% tabyl(CS1) #no hay perdidos
act <- act %>% mutate(CS1= case_when(CS1=="HOMBRE"~ "Hombre", 
                                     CS1== "MUJER" ~ "Mujer", 
                                     CS1== "NO RESPONDE" ~ NA))
#las etiquetas tienen que coincidir con la tabla de la ENAHO
sum(is.na(act$CS1))

#Recodificación por edad 
act %>% tabyl(CS2)
act <- act %>% mutate(CS2= case_when(CS2== 99 ~ NA, .default=CS2))
sum(is.na(act$CS2)) #no hay perdidos


#Educación 
act <- act %>% mutate(edu= case_when(
  CS3== "NINGUNO" ~ 1,
  CS3== "PRIMARIA – PRIMER GRADO" ~ 1,
  CS3== "PRIMARIA – SEGUNDO GRADO" ~ 1,
  CS3== "PRIMARIA – TERCER GRADO" ~ 1,
  CS3== "PRIMARIA – CUARTO GRADO" ~ 1,
  CS3== "PRIMARIA – QUINTO GRADO" ~ 1,
  CS3== "PRIMARIA – SEXTO GRADO" ~ 1, 
  CS3==  "SECUNDARIA – GRADO 1" ~ 2, 
  CS3==  "SECUNDARIA – GRADO 2" ~ 2, 
  CS3==  "SECUNDARIA – GRADO 3" ~ 2, 
  CS3==  "SECUNDARIA – GRADO 4" ~ 2, 
  CS3==  "SECUNDARIA – GRADO 5- SIN TÍTULO" ~ 2,
  CS3==  "SECUNDARIA – GRADO 5- CON TÍTULO" ~ 2,
  CS3==  "SECUNDARIA – GRADO 6- SIN TÍTULO" ~ 2,
  CS3==  "SECUNDARIA – GRADO 6- CON TÍTULO" ~ 2,
  CS3==  "PARAUNIVERSITARIA/TÉCNICO – INCOMPLETO" ~ 2,
  CS3==  "PARAUNIVERSITARIA/TÉCNICO – COMPLETO" ~ 2,
  CS3==  "UNIVERSIDAD – BACHILLERATO INCOMPLETO" ~ 3,
  CS3==  "UNIVERSIDAD – BACHILLERATO COMPLETO" ~ 3,
  CS3==  "UNIVERSIDAD – MAESTRÍA / DOCTORADO" ~ 3,
  CS3==  "NS/NR" ~ NA))
act <- act %>% mutate(edu=factor(edu, levels=c(1,2,3), labels = c("primaria", "secundaria", "universitaria")))
act %>% tabyl(edu)

#Estado conyugal 
levels(act$CS8)=c("Soltero", "Unión Libre", "Casado/a", "Separado/a", "Divorciado/a", "Viudo/a", "NA")


#Edad
act %>% tabyl(CS2) 
act <- act %>% mutate(CS2= as.numeric(as.character(CS2))) %>%
  mutate(edad= case_when(
  CS2 >= 18 & CS2 <= 34  ~ 1,
  CS2 >= 35 & CS2 <= 49  ~ 2,
  CS2 > 49 & CS2 < 99 ~ 3, 
  .default = NA))
act <- act %>% mutate(edad=factor(edad, labels = c("18a34", "35a49", "50+")))
act %>% tabyl(edad)


#sexo
set.seed(13112025) #usar la fecha del dia que se corrio 
tabyl(act$CS1)
aleatorios.sexo <- runif(9,0,1) #numeros de valores a imputar
aleatorios.sexo.cat <- ifelse(aleatorios.sexo <= 0.4579741, "Hombre", "Mujer")
act <- act %>% mutate(SD2=CS1)
act$SD2[is.na(act$SD2)==T] <- aleatorios.sexo.cat
act %>% tabyl(SD2)

#educacion 
set.seed(13112025)
aleatorios.edu <- runif(5,0,1)
tabyl(act$edu)
act.edu.cat <- ifelse(aleatorios.edu <= 0.2027897, "primaria", 
                      ifelse(aleatorios.edu <=0.4860515, "secundaria", "universitaria"))
act$edu[is.na(act$edu)==T] <- act.edu.cat
act %>% tabyl(edu)


#edad
set.seed(13112025)
aleatorios.edad <- runif(16,0,1)
act.edad.cat <- ifelse(aleatorios.edad <= 0.3517915, "18a34", 
                       ifelse(aleatorios.edad <= 0.3257329, "35a49", "50+"))
act$edad[is.na(act$edad)==T] <- act.edad.cat
tabyl(act$edad)



#5) probabilidad de seleccion
act %>% tabyl(C10)
act <- act %>% mutate(num.lineas = as.numeric(as.character(C10))) %>%
  mutate(num.lineas= case_when(num.lineas<=4~ num.lineas, 
                               .default = 4)) %>% 
  mutate(p.base= 1/num.lineas) %>% mutate(w.base= 1/p.base)



diseno.act0 <- svydesign(ids = ~1, data= act, weights = ~w.base)

#5) post estratificar
diseno.act <- postStratify(diseno.act0, strata=~edu+edad+SD2, population = tabla1)

svytable(~SD2, diseno.act)
#svytable(~SD2, diseno.enaho)

svytable(~edu, diseno.act)
#svytable(~edu, diseno.enaho)

svytable(~edad, diseno.act)
#svytable(~edad, diseno.enaho)

#6) normalizar los pesos 
act <- act %>% mutate(factor= 1/(diseno.act$prob))
n= dim(act)[1]
act$factor <- act$factor*(n/sum(1/diseno.act$prob))
sum(act$factor)

diseno.act2 <- svydesign(ids = ~1, data= act, weights = ~factor)

#7) verificar proporciones

svytable(~SD2, diseno.act2)/sum(icc$factor)
svytable(~CS1, diseno.enaho)/sum(1/diseno.enaho$prob)

svytable(~edu, diseno.act2)/sum(icc$factor)
svytable(~edu, diseno.enaho)/sum(1/diseno.enaho$prob)

svytable(~edad, diseno.act2)/sum(icc$factor)
svytable(~edad, diseno.enaho)/sum(1/diseno.enaho$prob)

save(icc, file="~/OneDrive - Universidad de Costa Rica/Documentos/ENCUESTA DEL CONSUMIDOR/BASE DE DATOS/act2024_ponderada.Rdata")
