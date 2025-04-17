# script for plotting Metacarpa abudnace over time----
#Notas-----
## command shift m para %>% 
#option - para <- 
#ver columnas con la funcion de 'names'

# ------Preamble---
rm(list = ls())

#--------Loading packages-------

packs <- c("tidyverse", "readxl")

lapply(packs, library, character.only = TRUE)
library(zoo)
library(ggplot2)

#-----Loading data------------

d <-read_excel(
  "data_removal_master.xlsx",
  skip = 4,
  trim_ws = TRUE,
  na = c("na", "NA")
) %>% 
  mutate_if(is.character, as.factor)

#--------Data preparacion Parte 1: Filtracion de data------------

summary(d)
#filter por proyect y site
d1 <- d %>% 
  filter(proyect == 1230286, Site == "PBLA")

#ver el resultado
view(d1)
print(d1)
names(d1)

#-------------Data preparacion Parte 2: Anadir una nueva columna que mostrar una fecha
#anadir una nueva variable que es la culminacion de 3 varibles para producir una fecha
#usar as.character funcion porque ciertas numeros estan solos (por ejemplo no es 01 es 1)
#mutate anadir una columna

d1$Year <- as.character(d1$Year)
d1$Month <- as.character(d1$Month)
d1$Day <- as.character(d1$Day)

d1 <- d1 %>% 
  mutate(new_date = as.Date(paste(d1$Year, d1$Month, d1$Day, sep = "-"), format = "%Y-%m-%d"))

#ver los resultados
print(head(d1))
names(d1)
view(d1)

#quiero mostrar un medio y error estandard entonces necisto calcular 
#stat_summary(fun.d1 = "mean_cl_boot")
#stat_summary(fun.d1, peom = "line")


#---------Plotting-------
#usar seq(from, to, by = interval) para generar una secuencia de fechas
#usar c() para crear una vector porque limits necesitan un vector como un input
#plot 
ggplot(d1, aes(x = new_date, y = Mastocarpus_latissimus)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun = mean, geom = "line") +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Cubierta Porcentual Media de Mastocarpus latissimus \ncon el tiempo") +
  theme_classic() +
  scale_x_date(
    limits = c(as.Date("2023-10-13"), as.Date("2025-01-07")), 
    breaks = seq(as.Date("2023-10-13"), as.Date("2025-01-07"), by = "3 months")
  )
#plot que muestra los dos control y pulso (cambiar limited para estar mas bajo y alto para permitir position_dodge)
ggplot(d1, aes(x = new_date, y = Mastocarpus_latissimus, color = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 15)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 15)) +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Cubierta Porcentual Media de Mastocarpus latissimus \ncon el tiempo") +
  theme_classic() +
  scale_x_date(
    limits = c(as.Date("2023-10-01"), as.Date("2025-01-20")), 
    breaks = c(seq(as.Date("2023-10-13"), as.Date("2025-01-13"), by = "3 months"))
  )

#plot lado por lado para comparar Mazzaella laminarioides a Mastocarpus latissimus
library(ggplot2)
install.packages(patchwork)

p1 <- ggplot(d1, aes(x = new_date, y = Mastocarpus_latissimus)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun = mean, geom = "line") +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Mastocarpus latissimus") +
  theme_classic() +
  scale_x_date(
    limits = c(as.Date("2023-10-13"), as.Date("2025-01-20"))
  )

p2 <- ggplot(d1, aes(x = new_date, y = Mazzaella_laminarioides)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun = mean, geom = "line") +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Mazzaella laminarioides") +
  theme_classic() +
  scale_x_date(
    limits = c(as.Date("2023-10-13"), as.Date("2025-01-07"))
  )
p1 + p2

p3 <- ggplot(d1, aes(x = new_date, y = Mastocarpus_latissimus, color = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 25)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 25)) +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Mastocarpus latissimus") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_date(
    limits = c(as.Date("2023-10-01"), as.Date("2025-01-20"))
  )

p4 <- ggplot(d1, aes(x = new_date, y = Mazzaella_laminarioides, color = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 25)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 25)) +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Mazzaella laminarioidess") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_date(
    limits = c(as.Date("2023-10-01"), as.Date("2025-01-20"))
  )

(p3 + p4) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")   

#------MINI PROYECTO_DIVERSIDAD DE ESPECIES-----
#1. Filtrar Sitios en el Sur: PBLA, CALF, CHEU, CHAI
d2 <- d %>% 
  filter(proyect == 1230286, Site == c("PBLA", "CALF", "CHEU", "CHAI"))

#ver los resultados
view(d2)
view(d2$Site)

#2. Calcular diversidad de especies 
#descargar paequete "vegan"
install.packages("vegan")
install.packages("permute")
install.packages("lattice")
library(vegan)
library(permute)
library(lattice)

#calcular diversidad de especies sessiles
#SESSILES: filtrar para sessiles
sessile <- d2 %>% 
  select(Lessonia_spicata:Pink_sponge)
view(sessile)

#calcular diversidad para sessiles
sessilediversity <- diversity(sessile %>% select(-Mastocarpus_latissimus), index = "shannon")
view(sessilediversity)

#ver si los datos son correctos
sessilediversityfake <- diversity(sessile, index = "shannon")
view(sessilediversityfake)

#crear una nueva columna con las valores de sessilediversity
sessile <- sessile %>% 
  mutate(sessilediversity)
view(sessile)

#MOBILE: filtrar para mobile + Mastocarpus
mobile <-  d2 %>% 
  select(Acanthina_monodon:Phymactis_sp, Mastocarpus_latissimus)
view(mobile)

#calcular diversidad para moviles
mobilediversity <- diversity(mobile %>% select(-Mastocarpus_latissimus), index = "shannon")
view(mobilediversity)

#crear una nueva columna con las valores de mobilediversity
mobile <- mobile %>% 
  mutate(mobilediversity)
view(mobile)

#cheque que mis valores son correctos y son incluir mastocarpus
mobilediversityfake <- diversity(mobile, index = "shannon")
view(mobilediversityfake)

#3. Hacer graficos de diversidad de especies y cubierta porcentual de Mastocarpus
#grafico por sessiles y mobiles
#grafico por sessile
ggplot(sessile, aes(x = sessilediversity, y = Mastocarpus_latissimus)) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de sesiles", y = "Cubierta Porcentual (%)", title = "Cubierta Porcentual de Mastocarpus latissimus con \n diversidad de especies sesiles") +
  theme_classic() 

#grafico por mobiles
ggplot(mobile, aes(x = mobilediversity, y = Mastocarpus_latissimus)) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de moviles", y = "Cubierta Porcentual (%)", title = "Cubierta Porcentual de Mastocarpus latissimus con \n diversidad de especies moviles") +
  theme_classic() 

#moviles y sessiles
p5 <- ggplot(sessile, aes(x = sessilediversity, y = Mastocarpus_latissimus)) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de sesiles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() 
p6 <- ggplot(mobile, aes(x = mobilediversity, y = Mastocarpus_latissimus)) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de moviles", y = "", title = "") +
  theme_classic() 

p5 + p6 + plot_annotation(title = "Cubierta Porcentual de Mastocarpus latissimus con diversidad \nde especies")

#4. Jugar/analizar con los datos ------
#4a. que occuiría si evitar los ceros del Mastocarpus?
install.packages("dplyr")
library(dplyr)
#evitar los ceros en sessiles
#mutate cambiar los valores en una columna, ifelse puede cambiar un valor de que es TRUE (NA) o FALSO 
sessile <- sessile %>% 
  mutate(Mastocarpus_latissimus = ifelse(Mastocarpus_latissimus == 0, NA, Mastocarpus_latissimus))
view(sessile$Mastocarpus_latissimus)

#hacer un grafico sin los ceros de mastocarpus
ggplot(sessile, aes(x = sessilediversity, y = Mastocarpus_latissimus )) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de sesiles", y = "Cubierta Porcentual (%)", title = "Cubierta Porcentual de Mastocarpus latissimus con \n diversidad de especies sesiles") +
  theme_classic() 
#evitar los ceros en moviles
mobile <- mobile %>% 
  mutate(Mastocarpus_latissimus = ifelse(Mastocarpus_latissimus == 0, NA, Mastocarpus_latissimus))
view(mobile$Mastocarpus_latissimus)
#hacer un grafico sin los ceros de mastocarpus
ggplot(mobile, aes(x = mobilediversity, y = Mastocarpus_latissimus)) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de moviles", y = "Cubierta Porcentual (%)", title = "Cubierta Porcentual de Mastocarpus latissimus con \n diversidad de especies moviles") +
  theme_classic() 

#mostrar graficos por ambos espeices sin ceros
p7 <- ggplot(sessile, aes(x = sessilediversity, y = Mastocarpus_latissimus )) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de sesiles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() 
p8 <- ggplot(mobile, aes(x = mobilediversity, y = Mastocarpus_latissimus)) +
  geom_smooth() +
  geom_point() +
  labs(x = "diversidad de moviles", y = "", title = "") +
  theme_classic() 
p7 + p8 + plot_annotation(title = "Cubierta Porcentual de Mastocarpus latissimus con diversidad \nde especies")

#------Modelos Lineales----------------
#modelo lineal de sessiles
oneway.model <- lm(Mastocarpus_latissimus ~ sessilediversity, data = sessile)
summary(oneway.model)
print(oneway.model)

#graficar 
p9 <- ggplot(sessile, aes(x = sessilediversity, y = Mastocarpus_latissimus )) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "diversidad de sesiles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() +
  annotate("text", x=0.53, y=109, label="y = 60.11 - 36.65x") +
  annotate("text", x=0.75, y=99, label="Adjusted R-sqaured = 0.07217") +
  annotate("text", x=0.47, y=89, label="p = 0.0859")

#modelo lineal de moviles
oneway.model2 <- lm(Mastocarpus_latissimus ~ mobilediversity, data = mobile)
summary(oneway.model2)
print(oneway.model2)

#graficar 
p10 <- ggplot(mobile, aes(x = mobilediversity, y = Mastocarpus_latissimus )) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "diversidad de moviles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() +
  annotate("text", x=0.50, y=105, label="y = 60.52 - 26.63x") +
  annotate("text", x=0.75, y=95, label="Adjusted R-sqaured = 0.04653") +
  annotate("text", x=0.53, y=85, label="p = 0.136")

p9 + p10 + plot_annotation(title = "Cubierta Porcentual de Mastocarpus latissimus con diversidad \nde especies")

#----Mastocarpus modelo linea con tiempo  ------
#modelo linea entre Mastocarpus y tiempo
#anadir una columna para la fecha a d2
d2$Year <- as.character(d2$Year)
d2$Month <- as.character(d2$Month)
d2$Day <- as.character(d2$Day)

d2 <- d2 %>% 
  mutate(new_date2 = as.Date(paste(d2$Year, d2$Month, d2$Day, sep = "-"), format = "%Y-%m-%d"))

#ver los resultados
print(head(d2))
view(d2)
names(d2)

#hacer una modelo entre Mastocarpus y tiempo
oneway.model3 <- lm(Mastocarpus_latissimus ~ new_date2, data = d2)
summary(oneway.model3)
print(oneway.model3)

#hacer un plot del modelo
ggplot(d2, aes(x = new_date2, y = Mastocarpus_latissimus)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Cubierta Porcentual Media de Mastocarpus latissimus \ncon el tiempo") +
  theme_classic() +
  scale_x_date(
    limits = c(as.Date("2023-10-01"), as.Date("2025-01-20")), 
    breaks = c(seq(as.Date("2023-10-13"), as.Date("2025-01-13"), by = "3 months"))) +
  annotate("text", x=as.Date("2024-07-13"), y=105,label="y = 256.98018 - 0.01237x") +
  annotate("text", x=as.Date("2024-07-13"), y=95, label="Adjusted R-sqaured = -0.003741") +
  annotate("text", x=as.Date("2024-07-13"), y=85, label="p = 0.45")
 
#hacer un modelo con el control y treatment como grupos
group_model <- d2 %>% 
  group_by(Treatment) %>% 
  do({
    model = lm(Mastocarpus_latissimus ~ new_date2, data = .)
    tidy(model)
  })
print(group_model)
#view results
summary(group_model)
print(group_model)
install.packages("broom")
library(broom)

#hacer el grafico
ggplot(d2, aes(x = new_date2, y = Mastocarpus_latissimus, color = Treatment)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Fecha", y = "Cubierta Porcentual Media (%)", title = "Cubierta Porcentual Media de Mastocarpus latissimus \ncon el tiempo") +
  theme_classic() +
  scale_x_date(
    limits = c(as.Date("2023-10-01"), as.Date("2025-01-20")), 
    breaks = c(seq(as.Date("2023-10-13"), as.Date("2025-01-13"), by = "3 months"))) +
  annotate("text", x=as.Date("2024-07-13"), y=105, label="Control: p = 0.498") +
  annotate("text", x=as.Date("2024-07-07"), y=95, label="Pulse: p = 0.129")

#----hacer un modelo linea por sessiles y distingue entre control y pulse ----
#add treatment column to sessile data
sessile_with_treatment <- sessile %>%
  mutate(Treatment = d2$Treatment)
view(sessile_with_treatment)

#hacer un modelo linea
group_model2 <- sessile_with_treatment %>% 
  group_by(Treatment) %>% 
  do({
    model2 = lm(Mastocarpus_latissimus ~ sessilediversity, data = .)
    tidy(model2)
  })
print(group_model2)

#graficar
p11 <- ggplot(sessile_with_treatment, aes(x = sessilediversity, y = Mastocarpus_latissimus, color = Treatment)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "diversidad de sesiles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() +
  theme(legend.position = "none") +
  annotate("text", x=0.47, y=109, label="Control: p = 0.431") +
  annotate("text", x=0.47, y=99, label="Pulse: p = 0.429")

#----hacer un modelo linea por moviles y distingue entre control y pulse ----
#add treatment column to movil data
mobile_with_treatment <- mobile %>%
  mutate(Treatment = d2$Treatment)
view(mobile_with_treatment)

group_model3 <- mobile_with_treatment %>% 
  group_by(Treatment) %>% 
  do({
    model3 = lm(Mastocarpus_latissimus ~ mobilediversity, data = .)
    tidy(model3)
  })
print(group_model3)

#graficar
p12 <- ggplot(mobile_with_treatment, aes(x = mobilediversity, y = Mastocarpus_latissimus, color = Treatment)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "diversidad de moviles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() +
  annotate("text", x=0.47, y=109, label="Control: p = 0.157") +
  annotate("text", x=0.47, y=99, label="Pulse: p = 0.342 ")
p11 + p12 + plot_annotation(title = "Cubierta Porcentual de Mastocarpus latissimus con \ndiversidad de especies")

#---modelo linea entre Mastocarpus y Maezaella---
model4 <- lm(Mastocarpus_latissimus ~ Mazzaella_laminarioides, data = d2)
summary(model4)
print(model4)

#graficar
ggplot(d2, aes(x = Mazzaella_laminarioides, y = Mastocarpus_latissimus)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Mastocarpus latissimus", y = "Mazzaella laminarioides", title = "Relacion entre la Cubierta Porcentual de Mazzaella laminarioides \nen Mastocarpus latissimus") +
  theme_classic() +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x=60, y=100, label="y = 14.8245 - 0.1371x") +
  annotate("text", x=65, y=90, label="Adjusted R-squared = 0.01674") +
  annotate("text", x=53, y=80, label="p = 0.0891")

#riqueza de especies + modelos lineas
mobile <- mobile %>% 
  mutate(
    species_richness = apply(select(mobile, Acanthina_monodon:Phymactis_sp), 1, function(row) sum(row > 0))
  )
view(mobile)

model5 <- lm(Mastocarpus_latissimus ~ species_richness, data = mobile)
summary(model5)
print(model5)

sessile <- sessile %>% 
  mutate(
    species_richness = apply(select(sessile, Lessonia_spicata:Pink_sponge, -Mastocarpus_latissimus), 1, function(row) sum(row > 0))
  )
view(sessile)

model6 <- lm(Mastocarpus_latissimus ~ species_richness, data = sessile)
summary(model6)
print(model6)

#-----graficar-----
p13 <- ggplot(sessile, aes(x = species_richness, y = Mastocarpus_latissimus )) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "riqueza de sesiles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() +
  annotate("text", x=4, y=100, label="y = 103.97 - 21.79x") +
  annotate("text", x=4, y=110, label="Adjusted R-squared = 0.5642") +
  annotate("text", x=4, y=90, label="p < 0.001")

p14 <- ggplot(mobile, aes(x = species_richness, y = Mastocarpus_latissimus )) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "riqueza de moviles", y = "Cubierta Porcentual (%)", title = "") +
  theme_classic() +
  annotate("text", x=2, y=100, label="y = 61.90 - 7.58x") +
  annotate("text", x=2, y=110, label="Adjusted R-squared = 0.02087") +
  annotate("text", x=2, y=90, label="p = 0.217149")

p13 + p14 +plot_annotation(title="Cubierta Porcentual de Mastocarpus latissimus con \nriqueza de especies")

setwd("/Users/maggie/Documents/Pasantía Primavera 2025/R Studio Graphs/Pasantía Chile")

#graficar cubierta porcentual de mastocarpus y riqueza de sessiles con control y pulso
#add treatment column to sessil data
sessile <- mutate(sessile, Treatment = d2$Treatment)
View(sessile)

#graficar
ggplot(sessile, aes(x = species_richness, y = Mastocarpus_latissimus, color = Treatment)) +
  geom_smooth() +
  geom_point() +
  labs(x = "riqueza de sessiles", y = "Cubierta Porcentual de Mastocarpus(%)", title = "Cubierta Porcentual de Mastocarpus latissimus con \nriqueza de sessiles (sin ceros)") +
  theme_classic() 

#-------------Sicronia------------
#riqueza de sessiles en los diferentes sitios con tiempo
#1. anadir nueva fecha
sessile <- mutate(sessile, new_date = d2$new_date2)
View(sessile)

#2. anadir sitios ("PBLA", "CALF", "CHEU", "CHAI")
sessile <- mutate(sessile, site = d2$Site)
View(sessile)

#3 evitar species_richness2 columna para claridad
sessile <- sessile %>% select(-species_richness2) 
View(sessile)

#3 graficar por sitio
#a. PBLA
  #filtrar
sessile_PBLA <- sessile %>% 
  filter(site == c("PBLA"))
View(sessile_PBLA)

  #graficar
ggplot(sessile_PBLA, aes(x = new_date, y = species_richness, color = Treatment)) +
  geom_smooth() +
  geom_point() +
  labs(x = "Fecha", y = "Riqueza de Sessiles", title = "") +
  theme_classic() 

#b CALF
  #filtrar
sessile_CALF <- sessile %>% 
  filter(site == c("CALF"))
View(sessile_CALF)

  #graficar
ggplot(sessile_CALF, aes(x = new_date, y = species_richness, color = Treatment)) +
  geom_smooth() +
  geom_point() +
  labs(x = "Fecha", y = "Riqueza de Sessiles", title = "") +
  theme_classic()

#c CHEU
  #filtrar
sessile_CHEU <- sessile %>% 
  filter(site == c("CHEU"))
View(sessile_CHEU)

  #graficar
ggplot(sessile_CHEU, aes(x = new_date, y = species_richness, color = Treatment)) +
  geom_smooth() +
  geom_point() +
  labs(x = "Fecha", y = "Riqueza de Sessiles", title = "") +
  theme_classic()

#d CHAI
  #filtrar
sessile_CHAI <- sessile %>% 
  filter(site == c("CHAI"))
View(sessile_CHAI)

  #graficar
ggplot(sessile_CHAI, aes(x = new_date, y = species_richness, color = Treatment)) +
  geom_smooth() +
  geom_point() +
  labs(x = "Fecha", y = "Riqueza de Sessiles", title = "") +
  theme_classic()

#chequar datos porque los tratimientos no ver correcto
ggplot(sessile, aes(x= new_date, y = species_richness, color = Treatment)) +
  geom_smooth() +
  geom_point() +
  labs(x = "Fecha", y = "Riqueza de Sessiles", title = "") +
  theme_classic()
