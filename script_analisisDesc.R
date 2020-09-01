# Información de SNIIM - Tortilla - por días del año 2020
## http://www.economia-sniim.gob.mx/TortillaAnualPorDia.asp?Cons=D&prod=T&Anio=2020&preEdo=Cd&Formato=Xls&submit=Ver+Resultados
## Revisar preprocesamiento de archivo raw respecto a BD:
### Dos archivos: Uno por tortillerías y otro por autoservicios
### Agregar columna 3: Establecimiento
### Quitar renglón PPP
### Replicar nombres de estados

# Cargar paquetes
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Leer archivos
precios_por_ciudad_tortillerias <- read.csv("raw/precios_2020_ciudad_tortillerias_20ago.csv")
precios_por_ciudad_autoservicios <- read.csv("raw/precios_2020_ciudad_autoservicios_20ago.csv")

# Cambiar a formato largo
head(precios_por_ciudad_tortillerias)
precios_por_ciudad_tortillerias_l <- precios_por_ciudad_tortillerias %>% pivot_longer(cols = starts_with("x"),
                                     names_prefix = "X",
                                     names_to = "Fecha",
                                     values_to = "Precio",
                                     values_drop_na = FALSE)
precios_por_ciudad_autoservicios_l <- precios_por_ciudad_autoservicios %>% pivot_longer(cols = starts_with("x"),
                                                                           names_prefix = "X",
                                                                           names_to = "Fecha",
                                                                           values_to = "Precio",
                                                                           values_drop_na = FALSE)
# Juntar archivos
# View(precios_por_ciudad_tortillerias_l)
# View(precios_por_ciudad_autoservicios_l)
precios_por_ciudad <- rbind(precios_por_ciudad_tortillerias_l,precios_por_ciudad_autoservicios_l)
head(precios_por_ciudad)

# Quitar valores fuera de rango
max(precios_por_ciudad$Precio,na.rm = T)
min(precios_por_ciudad$Precio,na.rm = T)
precios_por_ciudad<-precios_por_ciudad %>% filter(Precio < 40)

# COnvertir a fechas en formato POSIX
precios_por_ciudad <- 
  precios_por_ciudad %>% mutate(dia = substring(Fecha,1,2), mes = substring(Fecha,4,6))
mes <- precios_por_ciudad$mes
mes_num <- case_when(mes == "ene" ~ "01",
          mes == "feb" ~ "02",
          mes == "mar" ~ "03",
          mes == "abr" ~ "04",
          mes == "may" ~ "05",
          mes == "jun" ~ "06",
          mes == "jul" ~ "07",
          mes == "ago" ~ "08")
precios_por_ciudad <- 
  precios_por_ciudad %>% mutate(mes = mes_num, Fecha = dmy(paste(dia,mes,"2020",sep="/")))

# Generar estadísticas relevantes
## Precio promedio por estado/establecimiento 2020
precios_por_ciudad %>% group_by(Establecimiento,Estado) %>% 
  summarise(Precio_prom = mean(Precio, na.rm = T)) %>% 
  pivot_wider(names_from = Establecimiento, values_from = Precio_prom)

## Precio promedio por día
precios_por_ciudad %>% group_by(Establecimiento, Fecha) %>% 
  summarise(Precio_prom = mean(Precio, na.rm = T)) %>% 
  ggplot(aes(x = Fecha, y = Precio_prom, color = Establecimiento)) +
  geom_line() +
  geom_point()+
  scale_color_manual(values=c("#999999", "#E69F00"))

## Precios promedio por mes
precios_por_ciudad %>% group_by(Establecimiento, mes) %>% 
  summarise(Precio_prom = mean(Precio, na.rm = T)) %>% 
  ggplot(aes(x = mes, y = Precio_prom, color = Establecimiento)) +
  geom_point()+
  scale_color_manual(values=c("#999999", "#E69F00"))

# Generar visualizaciones a nivel nacional
ggplot(precios_por_ciudad, aes(x = Precio,  fill = Establecimiento)) + 
  geom_histogram(binwidth = 0.5,  alpha = 0.5, position = "identity") +
  scale_x_continuous(breaks = c(9:25), limits = c(9,25))+
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  theme(legend.position="top", axis.title.y=element_blank())


## Gráfico de precio promedio por estado
### Reordenar estados por diferencias
Estados_por_dif <- precios_por_ciudad %>% group_by(Estado, Establecimiento) %>%
  summarise(Precio_promedio = mean(Precio, na.rm = T)) %>% 
  pivot_wider(names_from = Establecimiento, values_from = Precio_promedio) %>%
  mutate(Diferencia = Tortillería - Autoservicio) %>%  arrange(Diferencia) %>%
  select(Estado)

precios_por_ciudad$Estado <- factor(precios_por_ciudad$Estado,levels = Estados_por_dif[[1]])
pd <- position_dodge(0.4) # move them .05 to the left and right

precios_por_ciudad_resumen <- precios_por_ciudad %>% group_by(Estado, Establecimiento) %>%
  summarise(Precio_promedio = mean(Precio, na.rm = T), 
            Precio_sd = sd(Precio, na.rm = T),
            Precio_obs = n(),
            Ult_precio = tail(Precio,1),
            Precio_max = max(Precio),
            Precio_min = min(Precio)) %>%
  mutate(Precio_promedio_SE = Precio_sd / sqrt(Precio_obs)) 

plot <- precios_por_ciudad_resumen %>%
  ggplot(aes(x = Estado, y = Precio_promedio, color = Establecimiento))+
  geom_point(size=1.2, fill="white", position = pd)+
  geom_errorbar(aes(ymin=Precio_min, ymax=Precio_max), width=.4,position = pd)+
  scale_y_continuous(breaks = c(9:23), limits = c(9,23))+
  scale_color_manual(values=c("#999999", "#E69F00")) +
  #geom_point(aes(y = Ult_precio,fill = Establecimiento), size = 1.2, shape = 8)+
  coord_flip()+
  theme_bw() +
  labs(title = "¿A cuánto el kilo de tortilla?",
       x = "Estado", y = "Precio |Min - Promedio - Max|",
       subtitle = "Datos de enero a agosto 2020. Precio promedio y rango de precios por Estado",
       caption = "Con información del SNIIM (Sistema Nacional de Información e Integración de Mercados), SE. \n 
       Orden: descendente, mayor diferencia de precios entre establecimientos")+ 
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),
        panel.border = element_blank()) 

plot

png("gráficos/Precio_Prom_Edo_2020.png", width = 3200, height = 1900, units = "px",res = 300)
plot
dev.off()

## Precio por estado - mes
for(estado in unique(precios_por_ciudad$Estado)){
  print(estado)
  plot <- ggplot(precios_por_ciudad %>% filter(Estado == estado), aes(x = mes, y = Precio, fill = Establecimiento))+
    geom_boxplot(position = position_dodge(1))+
    scale_fill_manual(values=c("#999999", "#E69F00")) +
    theme(legend.position="top", axis.title.y=element_blank())+
    ggtitle(estado)
  print(plot)
}

ggplot(precios_por_ciudad, aes(x = Estado, y = Precio, fill = Establecimiento))+
  geom_boxplot(position = position_dodge(1))+
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme(legend.position="top", axis.title.y=element_blank()) +
  coord_flip()

