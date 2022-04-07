# Bases de datos de indicadores económicos en R
# 9 de abril de 2022

# Conceptos
# Operaciones básicas
2+2
5*2
10-2

# Secuencias
1:10
5:50
seq(1,10,1)
seq(5,50,5)

# Objetos
x <- 5
x
y <- 2
y
x+y
w <- x+y
w

# Vectores
x <- c(4,3,6,7)
x
mean(x)
sum(x)

# Cargar libreria especializada en base de datos
library(tidyverse) # Comentar

# ENIGH 2020
# Cargar base de datos
library(haven)
enigh <- read_sav("dataset/concentradohogar.sav")
enigh

# Estructura de la base de datos
glimpse(enigh)
View(enigh)

# Mostrar etiquetas
enigh <- as_factor(enigh)

# Verbos de la libreria dplyr
# Seleccionar columnas
select(enigh,3,10:13,23,8)
select(enigh,ubica_geo,factor,sexo_jefe:tot_integ,ing_cor)

# Crear base ingresos del hogar
ingresos <- select(enigh,3,10:12,8,23:24,44,47,55,56)

# Mostrar objeto
ingresos

# Filter: filtrar filas
ingresos
filter(ingresos,
       sexo_jefe=="Mujer")
filter(ingresos,
       sexo_jefe=="Mujer" & edad_jefe>=65)

# arrange: ordenar
ingresos
arrange(ingresos,edad_jefe)
arrange(ingresos,-ing_cor)

# Estadísticas
ingresos
summarise(ingresos,media_ingreso=mean(ing_cor))
summarise(ingresos,media_ingreso=weighted.mean(ing_cor,factor))
summarise(ingresos,
          media_ingreso=weighted.mean(ing_cor,factor),
          media_ingtrab=weighted.mean(ingtrab,factor),
          media_rentas=weighted.mean(rentas,factor))   # Cuadro 3.5

# Multiples operaciones
summarise(ingresos,
          across(.cols=ing_cor:otros_ing,
                 .fns=~weighted.mean(.x,factor),
                 .names = "media_{.col}"))

# Variables categóricas
ingresos
count(ingresos,
      sexo_jefe)
count(ingresos,
      educa_jefe)

# Con factor de expansión
count(ingresos,
      sexo_jefe,wt=factor)
count(ingresos,
      educa_jefe,wt=factor)

# Base de datos del FMI
# Formatos ancho y largo
# xlsx
library(readxl)
fmi <- 
  read_excel("dataset/WEOOct2021all.xlsx",
             na = c(""," ","--","n/a"),
             trim_ws = TRUE)
fmi

# Seleccionar columnas
fmi <- 
  select(fmi,2:4,10:56)

# Eliminar espacios en blanco
x <- c("columna 1","columna 1 codigo","columna 2")
x
str_replace_all(x,
                pattern = "\\s",
                replacement = "_")

# Nombre de columnas
colnames(fmi)

colnames(fmi) <- 
  str_replace_all(colnames(fmi),
                  pattern = "\\s",
                  replacement = "_")
fmi

# De formato ancho a largo
pivot_longer(fmi,`1980`:`2026`)

# Renombrar columnas
pivot_longer(fmi,`1980`:`2026`,
             names_to="periodo",
             values_to="valor")

# Recodificar columna
fmi <- 
  pivot_longer(fmi,`1980`:`2026`,
               names_to="periodo",
               values_to="valor",
               names_transform=list(periodo=as.integer),
               values_drop_na=TRUE)
fmi

# Tasa de crecimiento del PIB (NGDP_RPCH), inflación (PCPIPCH), tasa de desempleo (LUR)
# Paridad del poder adquisitivo (PPPPC), PIB corriente en dólares actuales (NGDPD)
# Población (LP), pib_nom (NGDP),pib_cons (NGDP_R), NGAP_NPGDP (Output gap)
fmi_master <- 
  filter(fmi,WEO_Subject_Code %in% c("LUR","LP","NGDP","NGDP_R","NGDPDPC",
                                     "NGDPD","PCPIE","PPPPC","NGDP_RPCH",
                                     "PCPIPCH","NGAP_NPGDP"))

# Cambio de nombres de columnas
fmi_master
colnames(fmi_master) <- 
  c("iso","clave","pais","periodo","valor")

# Brecha del producto y tasa de desempleo
fmi_eeuu <- 
  filter(fmi_master,pais=="United States" & clave %in% c("LUR","NGAP_NPGDP"))
fmi_eeuu

# De filas a columnas
fmi_eeuu <- 
  pivot_wider(fmi_eeuu,names_from = clave,values_from = valor)

# Gráfico
par(mar=c(5,4,4,5)+.1)
plot(fmi_eeuu$periodo,fmi_eeuu$NGAP_NPGDP,
     type = "l",col = "blue", ylab = "Brecha del producto", xlab = "Periodo",
     main = "Estados Unidos")
par(new=TRUE)
plot(fmi_eeuu$periodo,fmi_eeuu$LUR,
     type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Desempleo",side=4,line=3)
legend(x=1983,y=4,legend=c("Brecha del producto","Desempleo"),
       col=c("blue", "red"), lty=1:1, cex=0.8)

# Caso de México
# Brecha del producto y tasa de desempleo
fmi_mexico <- 
  filter(fmi_master,pais=="Mexico" & clave %in% c("LUR","NGAP_NPGDP"))

View(fmi_mexico)

# APIS
# INEGI
# http://www3.inegi.org.mx//sistemas/api/indicadores/v1/tokenVerify.aspx
library(inegiR)

# Token personal
token <- "6d59f944-0246-5328-c042-d49ad277fbb9"

# PIB a precios de 2013
pib <- 
  inegi_series(493621,token)
pib

# Ordenar fechas
arrange(pib,date)
pib <- 
  arrange(pib,date)

# Gráfica
plot(pib$date,pib$values,type="l")

# Base de datos de COVID-19
load("concentrado_covid_2022.RData")
concentrado_covid_2022
range(concentrado_covid_2022$FECHA_ACTUALIZACION)
concentrado_covid_2022 %>% 
  group_by(FECHA_ACTUALIZACION) %>% 
  summarise(total=sum(CASOS))

# Fin