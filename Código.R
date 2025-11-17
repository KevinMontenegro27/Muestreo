# Datos ----      
load("act24_al_13nov.RData")
#Librerías ----
library(dplyr)
library(survey)
library(labelled)
library(janitor)
library(haven)
#install.packages("rnaturalearth")
library(rnaturalearth)
#library(rnaturalearthdata)
library(sf)
library(ggplot2)


#Nueva variable---- 
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
  filter(
    if_any(starts_with("AS"), ~ .x == "SÍ")
  )


todas_limpio <- todas %>%
  filter(Freq > 0) %>% 
  filter(
    if_any(starts_with("AS"), ~ grepl("S[IÍ]", .x, ignore.case = TRUE))
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

#Gráfico por los tipos de acoso sexual----
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

###Gráfico sobre piropo ----

ggplot(tablas4, aes(x = SD2, y = Freq, fill = AS2_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Gráfico sobre Pitado   ----

ggplot(tablas5, aes(x = SD2, y = Freq, fill = AS3_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Gráfico sobre genitales ----

ggplot(tablas6, aes(x = SD2, y = Freq, fill = AS4_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)


###Gráfico sobre Mensajes----

ggplot(tablas7, aes(x = SD2, y = Freq, fill = AS5_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Gráfico sobre Comentarios ----

ggplot(tablas8, aes(x = SD2, y = Freq, fill = AS6_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Gráfico sobre publicar infor ----

ggplot(tablas9, aes(x = SD2, y = Freq, fill = AS7_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)

###Gráfico sobre FOtos o videos manupilados ----
ggplot(tablas10, aes(x = SD2, y = Freq, fill = AS8_1)) +
  geom_col(position = "dodge") +
  facet_wrap(~ edad) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Sexo",
    y = "Frecuencia",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 14)



###Grafico de Persona que realizó el acoso sexual según tipo de acoso----.
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

##grafico
library(dplyr)
library(ggplot2)
library(scales) 

COLOR_VERDE <- "#4CC0B0" 
COLOR_NARANJA <- "#F49E69"

tabla7 <- svytable(~AS5_1+edad+SD2, disenyo)

df_dispersion <- as.data.frame(prop.table(tabla7, margin = c(2, 3))) %>%
  filter(grepl("^[Ss][Íi]$", AS5_1)) 

ggplot(df_dispersion, aes(x = edad, y = Freq, group = SD2, color = SD2)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_line(linewidth = 1) +
  
  scale_color_manual(values = c("Hombre" = COLOR_VERDE, "Mujer" = COLOR_NARANJA)) + 
  
  labs(
    title = "",
    x = "Grupo de Edad",
    y = "Proporción de Personas Afectadas",
    color = "Sexo"
  ) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Freq, accuracy = 0.1)), 
            hjust = 0.5, vjust = -1, size = 3)

##grafico
obtener_proporcion_si_grupo <- function(variable, grupo, disenyo) {
  # Crea la fórmula para la tabla: ~ variable + grupo
  formula_tabla <- as.formula(paste("~", variable, "+", grupo))
  tabla <- svytable(formula_tabla, disenyo)

act <- act %>% mutate(CS2_clean = as.numeric(as.character(CS2))) 
df_violin <- act %>% 
  filter(!is.na(CS8) & CS8 != "NA")


ggplot(df_violin, aes(x = CS8, y = CS2_clean, fill = CS8)) +
  geom_violin(trim = TRUE, alpha = 0.7) + 
  geom_boxplot(width = 0.1, fill = "white") + 
  
  
  scale_fill_manual(values = c("Soltero" = COLOR_VERDE, 
                               "Unión Libre" = COLOR_NARANJA,
                               "Casado/a" = "#2B685F", 
                               "Separado/a" = "#F28448", #
                               
                               "Divorciado/a" = COLOR_VERDE,
                               "Viudo/a" = COLOR_NARANJA)) +
  
  labs(
    title = "Distribución de Edad por Estado Conyugal",
    x = "Estado Conyugal (CS8)",
    y = "Edad (Años)",
    fill = "Estado Conyugal"
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##grafico 
df_radar_largo <- bind_rows(lapply(variables_acoso, obtener_proporcion_si_grupo, 
                                   grupo = "edad", disenyo = disenyo))
df_radar_largo <- df_radar_largo %>%
  mutate(
    Etiqueta = case_when(
      Acoso == "AS1_1" ~ "1. Chiflado",
      Acoso == "AS2_1" ~ "2. Piropos",
      Acoso == "AS3_1" ~ "3. Pitado",
      Acoso == "AS4_1" ~ "4. Genitales",
      Acoso == "AS5_1" ~ "5. Mensajes Sex.",
      Acoso == "AS6_1" ~ "6. Comentarios Sex.",
      Acoso == "AS7_1" ~ "7. Info Privada",
      Acoso == "AS8_1" ~ "8. Fotos Manip."
    )
  )
COLOR_VERDE <- "#4CC0B0"
COLOR_NARANJA <- "#F49E69"
COLOR_TERCER <- "#FFD700" 

ggplot(df_radar_largo, aes(x = Etiqueta, y = Proporcion_Si, group = Grupo)) +
  
  geom_point(aes(color = Grupo), size = 5) + 
  
  scale_color_manual(values = c("18a34" = COLOR_NARANJA, 
                                "35a49" = COLOR_VERDE, 
                                "50+" = "red")) +
  
  coord_polar(start = 0) + 
  labs(
    title = "",
    subtitle = "La distancia del punto al centro indica la proporción de acoso.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5), 
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )










