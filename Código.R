# Datos ----      
load("act24_al_13nov.RData")
#Librerías ----
library(dplyr)
library(survey)
library(labelled)
library(janitor)
library(haven)
library(tidyr)
library(ggplot2)

##Nueva variable---- 
act$factor= rep(1, 937)

# Recodificación I ---- 
#### Sexo ----
act %>% tabyl(CS1) #no hay perdidos
act <- act %>% mutate(CS1= case_when(CS1=="HOMBRE"~ "Hombre", 
                                     CS1== "MUJER" ~ "Mujer", 
                                     CS1== "NO RESPONDE" ~ NA))
#las etiquetas tienen que coincidir con la tabla de la ENAHO
sum(is.na(act$CS1))

### Edad ---- 
act %>% tabyl(CS2)
act <- act %>% mutate(CS2= case_when(CS2== 99 ~ NA, .default=CS2))
sum(is.na(act$CS2))


###Educación---- 
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

###Estado conyugal ---- 
levels(act$CS8)=c("Soltero/a", "Unión Libre", "Casado/a", "Separado/a", "Divorciado/a", "Viudo/a", "NA")

# Recodificación II---- 
### Edad----
act %>% tabyl(CS2) 
act <- act %>% mutate(CS2= as.numeric(as.character(CS2))) %>%
  mutate(edad= case_when(
    CS2 >= 18 & CS2 <= 34  ~ 1,
    CS2 >= 35 & CS2 <= 49  ~ 2,
    CS2 > 49 & CS2 < 99 ~ 3, 
    .default = NA))
act <- act %>% mutate(edad=factor(edad, labels = c("18a34", "35a49", "50+")))
act %>% tabyl(edad)


### Sexo----
set.seed(13112025) #usar la fecha del dia que se corrio 
tabyl(act$CS1)
aleatorios.sexo <- runif(9,0,1) #numeros de valores a imputar
aleatorios.sexo.cat <- ifelse(aleatorios.sexo <= 0.4579741, "Hombre", "Mujer")
act <- act %>% mutate(SD2=CS1)
act$SD2[is.na(act$SD2)==T] <- aleatorios.sexo.cat
act %>% tabyl(SD2)

### Educacion ---- 
set.seed(13112025)
aleatorios.edu <- runif(5,0,1)
tabyl(act$edu)
act.edu.cat <- ifelse(aleatorios.edu <= 0.2027897, "primaria", 
                      ifelse(aleatorios.edu <=0.4860515, "secundaria", "universitaria"))
act$edu[is.na(act$edu)==T] <- act.edu.cat
act %>% tabyl(edu)


#Imputación----
# Edad
set.seed(13112025)
aleatorios.edad <- runif(16,0,1)
act.edad.cat <- ifelse(aleatorios.edad <= 0.3517915, "18a34", 
                       ifelse(aleatorios.edad <= 0.3257329, "35a49", "50+"))
act$edad[is.na(act$edad)==T] <- act.edad.cat
tabyl(act$edad)

#Declaración del diseño---- 
disenyo <- svydesign(ids=~1, data=act, weights = ~factor)
class(disenyo)

#Tablas Geográficas----
###Tabla sobre Educación por edad, dividida por sexo----
tabla1 <- svytable(~edu+edad+SD2, disenyo); tabla1
tablas1 <- as.data.frame(tabla1); tabla1
###Tabla sobre estado conyugal por edad, dividida por sexo----
tabla2<- svytable(~CS8+edad+SD2, disenyo); tabla2
tablas2<- as.data.frame(tabla2)
### Tabla sobre si tenemos internet por sexo y dividido por edad----.
tabla11 <- svytable(~CS10+edad+SD2, disenyo); tabla11
tablas11<- as.data.frame(tabla11)



#Tablas sobre acoso sexual callejero----
###Tabla sobre Chiflado por edad, divido por sexo----. 
tabla3 <- svytable(~AS1_1+edad+SD2, disenyo); tabla3
tablas3<- as.data.frame(tabla3)
prop_tabla3 <- prop.table(tabla3, margin = c(2, 3))
prop_tabla3
###Tabla sobre Piropos por edad, divido por sexo----. 
tabla4 <- svytable(~AS2_1+edad+SD2, disenyo); tabla4
tablas4<- as.data.frame(tabla4)
prop_tabla4 <- prop.table(tabla4, margin = c(2, 3))
prop_tabla4
###Tabla sobre Pitado por edad, divido por sexo----. 
tabla5 <- svytable(~AS3_1+edad+SD2, disenyo); tabla5
tablas5<- as.data.frame(tabla5)
prop_tabla5 <- prop.table(tabla5, margin = c(2, 3))
prop_tabla5
###Tabla sobre Mostrado genitales sin desearlo por edad, divido por sexo----. 
tabla6 <- svytable(~AS4_1+edad+SD2, disenyo); tabla6
tablas6<- as.data.frame(tabla6)
prop_tabla6 <- prop.table(tabla6, margin = c(2, 3))
prop_tabla6

#Tablas sobre acoso sexual digital ----
###Tabla sobre mensajes por edad, divido por sexo----. 
tabla7 <- svytable(~AS5_1+edad+SD2, disenyo); tabla7
tablas7<- as.data.frame(tabla7)
prop_tabla7 <- prop.table(tabla7, margin = c(2, 3))
prop_tabla7
###Tabla sobre comentarios de tipo sexual por edad, divido por sexo----. 
tabla8 <- svytable(~AS6_1+edad+SD2, disenyo); tabla8
tablas8<- as.data.frame(tabla8)
prop_tabla8 <- prop.table(tabla8, margin = c(2, 3))
prop_tabla8
###Tabla sobre publicar información por edad, divido por sexo----. 
tabla9 <- svytable(~AS7_1+edad+SD2, disenyo); tabla9
tablas9<- as.data.frame(tabla9)
###Tabla sobre enviado fotos y videos manipulados sin desearlo por edad, divido por sexo----. 
tabla10 <- svytable(~AS8_1+edad+SD2, disenyo); tabla10
tablas10<- as.data.frame(tabla10)
prop_tabla10 <- prop.table(tabla3, margin = c(2, 3))
prop_tabla10



#Tablas por provincia dado el tipo de acoso---- 
###Tabla sobre Chiflados, por provincia dado sexo---- 
tabla12 <- svytable(~CS13_A+SD2+AS1_1, disenyo); tabla12
tablas12<- as.data.frame(tabla12)
###Tabla sobre Piropos, por provincia dado sexo---- 
tabla13 <- svytable(~CS13_A+SD2+AS2_1, disenyo); tabla13
tablas13<- as.data.frame(tabla13)
###Tabla sobre pitado, por provincia dado sexo---- 
tabla14 <- svytable(~CS13_A+SD2+AS3_1, disenyo); tabla14
tablas14<- as.data.frame(tabla14)
###Tabla sobre mostradi genitales, por provincia dado sexo---- 
tabla15 <- svytable(~CS13_A+SD2+AS4_1, disenyo); tabla15
tablas15<- as.data.frame(tabla15)


###Tabla de provincia dado sexo----.
tabla16 <- svytable(~CS13_A+SD2+AS5_1, disenyo); tabla16
tablas16<- as.data.frame(tabla16)
###Tabla de provincia dado sexo----.
tabla17 <- svytable(~CS13_A+SD2+AS6_1, disenyo); tabla17
tablas17<- as.data.frame(tabla17)
###Tabla de provincia dado sexo----.
tabla18 <- svytable(~CS13_A+SD2+AS7_1, disenyo); tabla18
tablas18<- as.data.frame(tabla18)
###Tabla de provincia dado sexo----.
tabla19 <- svytable(~CS13_A+SD2+AS8_1, disenyo); tabla19
tablas19<- as.data.frame(tabla19)


#Frecuencia del sexo que lo realiza----
total_casos <- sum(table(act$AS1_2)) + 
  sum(table(act$AS2_2)) +
  sum(table(act$AS3_2)) +
  sum(table(act$AS4_2)) +
  sum(table(act$AS5_2)) +
  sum(table(act$AS6_2)) +
  sum(table(act$AS7_2))

total_casos_hombres <- sum(table(act$AS1_2)[1]) + 
  sum(table(act$AS2_2)[1]) +
  sum(table(act$AS3_2)[1]) +
  sum(table(act$AS4_2)[1]) +
  sum(table(act$AS5_2)[1]) +
  sum(table(act$AS6_2)[1]) +
  sum(table(act$AS7_2)[1])

total_casos_mujeres <- sum(table(act$AS1_2)[2]) + 
  sum(table(act$AS2_2)[2]) +
  sum(table(act$AS3_2)[2]) +
  sum(table(act$AS4_2)[2]) +
  sum(table(act$AS5_2)[2]) +
  sum(table(act$AS6_2)[2]) +
  sum(table(act$AS7_2)[2])

total_casos_ambos <- sum(table(act$AS1_2)[3]) + 
  sum(table(act$AS2_2)[3]) +
  sum(table(act$AS3_2)[3]) +
  sum(table(act$AS4_2)[3]) +
  sum(table(act$AS5_2)[3]) +
  sum(table(act$AS6_2)[3]) +
  sum(table(act$AS7_2)[3])

total_casos_NS <- sum(table(act$AS1_2)[4]) + 
  sum(table(act$AS2_2)[4]) +
  sum(table(act$AS3_2)[4]) +
  sum(table(act$AS4_2)[4]) +
  sum(table(act$AS5_2)[4]) +
  sum(table(act$AS6_2)[4]) +
  sum(table(act$AS7_2)[4])

prop_hombres <- total_casos_hombres / total_casos
prop_mujeres <-total_casos_mujeres / total_casos
prop_ambos <-total_casos_ambos / total_casos
prop_NS <-total_casos_NS / total_casos

tabla20 <- as.data.frame(cbind(prop_hombres, prop_mujeres, prop_ambos, prop_NS))
names(tabla20) <- c("Hombres", "Mujeres", "Ambos", "NS/NR")
tabla20


#Gráficos ----
##Mapa----
library(dplyr)

tablas12$tipo_acoso <- "Chiflados"
tablas13$tipo_acoso <- "Piropos"
tablas14$tipo_acoso <- "Pitado"
tablas15$tipo_acoso <- "Mostrar genitales"
tablas16$tipo_acoso <- "Acoso5"
tablas17$tipo_acoso <- "Acoso6"
tablas18$tipo_acoso <- "Acoso7"
tablas19$tipo_acoso <- "Acoso8"

todas <- bind_rows(
  tablas12, tablas13, tablas14, tablas15,
  tablas16, tablas17, tablas18, tablas19
)

todas_limpio <- todas %>%
  filter(Freq > 0) %>% 
  filter(`SÍ` %in% c("SÍ","Sí","si","SI") | grepl("SÍ", AS1_1) |
           grepl("SÍ", AS2_1) | grepl("SÍ", AS3_1) |
           grepl("SÍ", AS4_1) | grepl("SÍ", AS5_1) |
           grepl("SÍ", AS6_1) | grepl("SÍ", AS7_1) |
           grepl("SÍ", AS8_1))

todas_limpio <- todas %>%
  filter(
    (AS1_1 == "SÍ") |
      (AS2_1 == "SÍ") |
      (AS3_1 == "SÍ") |
      (AS4_1 == "SÍ") |
      (AS5_1 == "SÍ") |
      (AS6_1 == "SÍ") |
      (AS7_1 == "SÍ") |
      (AS8_1 == "SÍ")
  )

t_sexo <- todas_limpio %>%
  group_by(CS13_A, SD2) %>% 
  summarise(total = sum(Freq), .groups = "drop")


cr <- ne_states(country = "Costa Rica", returnclass = "sf")
t_sexo <- t_sexo %>%
  mutate(CS13_A = case_when(
    CS13_A == "Limon" ~ "Limón",
    CS13_A == "San Jose" ~ "San José",
    TRUE ~ CS13_A
  ))



mapa <- cr %>%
  left_join(t_sexo, by = c("name" = "CS13_A"))


library(ggplot2)

ggplot(mapa) +
  geom_sf(aes(fill = total)) +
  scale_fill_viridis_c(option = "turbo") +
  facet_wrap(~ SD2) +
  labs(title = "Casos de acoso por provincia y sexo",
       fill = "Casos") +
  theme_minimal()


###Gráfico sobre Chiflado----
ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS1_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Tabla sobre piropo ----

ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS2_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Tabla sobre Pitado   ----

ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS3_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Tabla sobre genitales ----

ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS4_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

#Grafico acoso segun hombre mujer general
tipos_acoso <- c(
    "AS1_2", "AS2_2", "AS3_2", "AS4_2", "AS5_2", "AS6_2", "AS7_2", "AS8_2"
  )
 
  etiquetas <- c(
    "Silbidos con intenciones sexuales",
    "Piropos",
    "Pitado desde un vehículo",
    "Mostrado genitales sin desearlo",
    "Mensajes sexuales (digital)",
    "Comentarios sexuales (digital)",
    "Publicar información sexual (digital)",
    "Fotografía/video sexual (digital)"
  )
  
  tabla_acoso <- lapply(seq_along(tipos_acoso), function(i) {
    act %>%
      count(Quien = get(tipos_acoso[i])) %>%
      mutate(Tipo = etiquetas[i]) %>%
      filter(!is.na(Quien))
  }) %>%
    bind_rows()
  
  
  tabla_acoso <- tabla_acoso %>%
    group_by(Tipo) %>%
    mutate(Porcentaje = 100 * n / sum(n))
  
  tabla_acoso$Quien <- factor(tabla_acoso$Quien, levels = c("Mujeres", "Hombres", "Ambos", "NS/NR"))
  tabla_acoso$Tipo <- factor(tabla_acoso$Tipo, levels = rev(etiquetas))
  
  
  color_pal <- c(
    "Mujeres" = "#4A90E2", # azul
    "Hombres" = "#EA8722", # naranja
    "Ambos" = "#ACACAC",   # gris
    "NS/NR" = "#FFC90E"    # amarillo
  )
  
  ggplot(tabla_acoso, aes(x = Tipo, y = Porcentaje, fill = Quien)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = color_pal, drop = FALSE) +
    labs(
      title = "Persona que realizó el acoso sexual a hombres según tipo de acoso",
      x = NULL, y = "Porcentaje",
      fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(hjust = 1)
    )




###Tabla sobre Mensajes----

ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS5_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Tabla sobre Comentarios ----

ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS6_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Tabla sobre publicar infor ----

ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS7_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Tabla sobre FOtos o videos manupilados ----

ggplot(tablas3, aes(x = SD2, y = Freq, fill = AS8_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

### grafico de Persona que realizó el acoso sexual a hombres según tipo de acoso----.
tipos_acoso <- c(
    "AS1_2", "AS2_2", "AS3_2", "AS4_2", "AS5_2", "AS6_2", "AS7_2", "AS8_2"
  )
 
  etiquetas <- c(
    "Silbidos con intenciones sexuales",
    "Piropos",
    "Pitado desde un vehículo",
    "Mostrado genitales sin desearlo",
    "Mensajes sexuales (digital)",
    "Comentarios sexuales (digital)",
    "Publicar información sexual (digital)",
    "Fotografía/video sexual (digital)"
  )
  
  tabla_acoso <- lapply(seq_along(tipos_acoso), function(i) {
    act %>%
      count(Quien = get(tipos_acoso[i])) %>%
      mutate(Tipo = etiquetas[i]) %>%
      filter(!is.na(Quien))
  }) %>%
    bind_rows()
  
  
  tabla_acoso <- tabla_acoso %>%
    group_by(Tipo) %>%
    mutate(Porcentaje = 100 * n / sum(n))
  
  tabla_acoso$Quien <- factor(tabla_acoso$Quien, levels = c("Mujeres", "Hombres", "Ambos", "NS/NR"))
  tabla_acoso$Tipo <- factor(tabla_acoso$Tipo, levels = rev(etiquetas))

ggplot(tabla_acoso, aes(x = Tipo, y = Porcentaje, fill = Quien)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", drop = FALSE) +
  labs(
    x = NULL, y = "Porcentaje",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(hjust = 1)
  )




