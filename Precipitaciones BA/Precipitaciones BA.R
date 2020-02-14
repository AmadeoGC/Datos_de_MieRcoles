library(tidyverse)
library(lubridate)


estaciones <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/estaciones.csv")
locale = readr::locale(encoding = "latin1")
meteo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/meteo.csv", na = "-99.9")

#BD
estaciones
meteo

#Unir BD
(datos <- meteo %>% 
  inner_join(estaciones, by = "id_estacion")
)


write_csv2(datos, "xx.csv")


datos %>% 
  filter(provincia=="Buenos Aires") %>% 
  group_by(fecha) %>% 
  summarize(n = n_distinct(id_estacion))
  


#ultimo año con registro
datos %>% 
  filter(provincia == "Buenos Aires") %>% 
  mutate(año =  year(fecha),
         mes = month(fecha)) %>% 
   summarize(x = max(año, na.rm = TRUE))




  
  
  


datos %>% 
  filter(provincia == "Buenos Aires") %>% 
  mutate(año =  year(fecha),
         mes = month(fecha),
         fecha.mes = ymd(paste0(año,"-",ifelse(mes<10,paste0("0",mes),mes),"-01"))) %>% 
  group_by(fecha.mes) %>% 
  summarize(promedio = mean(precipitacion, na.rm = TRUE))





datos %>% 
  filter(provincia == "Buenos Aires", fecha>="2000-01-01") %>% 
  mutate(año =  year(fecha),
         mes = month(fecha),
         fecha.mes = ymd(paste0(año,"-",ifelse(mes<10,paste0("0",mes),mes),"-01"))) %>% 
  group_by(fecha.mes) %>% 
  drop_na(precipitacion) %>% 
  #mutate(precipitacion2 = ifelse(is.na(precipitacion), 0 , precipitacion),
  #  total.prep = sum(precipitacion2)) %>% 
  summarize(precip.final = sum(precipitacion, na.rm = TRUE)) %>% 
  ggplot(aes(fecha.mes, precip.final)) +
  geom_line(aes(alpha=fecha.mes)) +
  #geom_point(shape=21, fill="black",color="white", size=2) +
  geom_smooth() +
  theme_minimal() +
  theme(legend.position = "none")

  
  
  
(datos_2012 <- datos %>% 
  filter(provincia == "Buenos Aires") %>% 
  mutate(año =  year(fecha),
         mes = month(fecha),
         semana = week(fecha),
         fecha.mes = ymd(paste0(año,"-",ifelse(mes<10,paste0("0",mes),mes),"-01"))) %>% 
  filter(año==2012) %>% 
  group_by(semana) %>% 
  drop_na(precipitacion) %>% 
  #mutate(precipitacion2 = ifelse(is.na(precipitacion), 0 , precipitacion),
  #  total.prep = sum(precipitacion2)) %>% 
  summarize(precip.final.2012 = sum(precipitacion, na.rm = TRUE))
)


library(ggtext) 
library(scales)

(graf_BA <- datos %>% 
  filter(provincia == "Buenos Aires") %>% 
  mutate(año =  year(fecha),
         mes = month(fecha),
         semana = week(fecha),
         fecha.mes = ymd(paste0(año,"-",ifelse(mes<10,paste0("0",mes),mes),"-01"))) %>% 
  filter(between(año, 2000, 2011)) %>% 
  group_by(año, semana) %>% 
  drop_na(precipitacion) %>% 
  summarize(precip.final = sum(precipitacion, na.rm = TRUE)) %>% 
  group_by(semana) %>% 
  summarize(precip.min = min(precip.final),
           precip.mean = mean(precip.final),
           precip.max = max(precip.final)) %>% 
  ggplot(aes(x=semana)) +
  geom_ribbon(aes(ymin=precip.min, ymax=precip.max), fill="#D49797", alpha=.3) +
  geom_line(aes(y=precip.mean), color="#D49797", alpha=.4) +
  geom_line(data = datos_2012, aes(x=semana, y=precip.final.2012), color="#1f77b4", size=1.1) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(1,53,2), expand = c(0.01, 0.01)) +
  labs(title="Precipitación semanal en la provincia de Buenos Aires",
       subtitle = "Comparación registros año <span style='color:#1f77b4'>**2012**</span> en relación a los máximos y mínimos durante el periodo <span style='color:#D49797'>**2000-2011**</span>",
       caption = "@amadeob12 | Fuente: http://www.cima.fcen.uba.ar/ClarisLPB/",
       x="\nSemanas",
       y="Precipitaciones (mm)\n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line.x = element_line(size = .8),
        axis.ticks.x = element_line(),
        plot.title = element_text(size=16, color="grey30", face="bold"),
        plot.subtitle = element_markdown(size=12, color = "grey40"),
        plot.caption = element_text(size = 9, color= "grey75"))
)

ggsave("Precip_BA.png", graf_BA, width = 26, height = 13, units = "cm", dpi=600)






library(magick)
library(magrittr) 
#Cargar gráfico como imagen
plot <- image_read("Precip_BA.png")

(gif_1 <- image_read ("https://media.giphy.com/media/3ohs87YZvG2aEAA75e/giphy.gif")%>% 
  image_scale ("100")%>% 
  image_rotate (0) 
)


# seleccionar imagen de fondo (gráfico)
background <- image_background(image_scale(plot, "1200"), "white", flatten = TRUE)
# Combinar
frames <- image_composite(background, gif_1, offset = "+980+20")
# Animación
animation <- image_animate(frames, fps = 5)
print(animation)
#Save gif
image_write(animation, "precip_BA_gif1.gif")







