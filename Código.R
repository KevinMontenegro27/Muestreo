# Datos ----      
load("act24_al_13nov.RData")


##Nueva variable---- 
act$factor= rep(1, 937)

#Librerías ----
library(dplyr)
library(survey)
library(labelled)

#Imputación 
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


# Declaración del modelo 
disenyo <- svydesign(ids=~1, data=act, weights = ~factor)
class(disenyo)
#Tablas ----



#Gráficos ----



