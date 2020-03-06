library(tidyverse)
library(janitor)
library(lubridate)
library(ggtext)


confirmados <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
muertes <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recuperados <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")



head(confirmados)
names(confirmados)


(bd_casos <- confirmados %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "fecha", values_to = "casos", values_drop_na = TRUE) %>% 
  clean_names() %>% 
  #separate(fecha, into = c("fecha", "hora"), sep = " ") %>% 
  mutate(fecha_ok = mdy(fecha)) %>% 
  arrange(fecha_ok)
)




#base (prueba) previa para calcular casos acumulados y casos nuevo x día
bd_casos %>% 
  group_by(fecha_ok) %>% 
  summarize(n_casos = sum(casos)) %>% 
  #summarize(total = max(n_casos)) %>% #hay dias con varios registros, como los datos son acumulados se debe dejar el último registro del día usando max()
  #mutate(casos.nuevos.x = c(0,diff(total))) %>%  #https://stackoverflow.com/questions/35169423/error-when-using-diff-function-inside-of-dplyr-mutate (idea original)
  mutate(casos.nuevos = c(n_casos[1],diff(n_casos)))


#Grafico de casos nuevos y acumulados (global) ----
(p1 <- bd_casos %>% 
  group_by(fecha_ok) %>% 
  summarize(n_casos = sum(casos)) %>% 
  mutate(casos.nuevos.dia = c(n_casos[1],diff(n_casos))) %>% 
   #gráfico
  ggplot() +
    #Casos acumuldos a la fecha x
  geom_line(aes(fecha_ok, n_casos), color="firebrick", lwd=.8) +
  geom_point(aes(fecha_ok, n_casos), shape=21, color="firebrick", fill="white",size=2) +
    #casos nuevos x día
  geom_line(aes(fecha_ok, casos.nuevos.dia), color="steelblue", lwd=.8) +
  geom_point(aes(fecha_ok, casos.nuevos.dia), shape=21, color="steelblue", fill="white",size=2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.02,0.02), breaks = seq(0,1000000,5000)) +
  labs(title = "**Coronavirus COVID-19:** Número de <span style='color:steelblue'>**nuevos casos**</span> y <span style='color:firebrick'>**total de casos acumulados**</span> diariamente",
       subtitle = str_glue("Casos confirmados a nivel global desde el primer registro el 21 de enero 2020. *Actualización {now()}*"),
       y="casos (n°)\n",
       x="\ndías",
       caption = "@amadeob12 | Fuente: CSSE at Johns Hopkins University") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.line.x = element_line(size=.8),
        axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_markdown(color = "grey10"),
        plot.subtitle = element_markdown(color = "grey30"),
        plot.caption = element_text(size=9, color="grey70")) +
  geom_label(aes(x = ymd("2020-02-10")+0.6 , y = 24500, label = "Cambio en metodología para\n contabilizar nuevos casos \n(OMS)"),  hjust = 0.5, vjust = 0.5, 
             lineheight = 1, fill = "grey97", label.size = NA, size = 3) +
  geom_curve(aes(x = ymd("2020-02-10")+0.9, y = 19000, xend = ymd("2020-02-12")+0.8, yend = 15500), colour = "grey50", curvature = 0.1, size=0.3,
             arrow = arrow(length = unit(0.03, "npc")))
)  

ggsave("Covid-19_casos_general.png", p1, width = 30, height = 15, units = "cm", dpi=500)





#Grafico de casos nuevos y acumulados (China vs. Otros) ----
(p2 <- bd_casos %>%
  mutate(country_dif = if_else(country_region=="Mainland China", "Mainland China", "Other")) %>% 
  group_by(country_dif, fecha_ok) %>% 
  summarize(n_casos = sum(casos)) %>% 
  mutate(casos.nuevos.dia = c(n_casos[1],diff(n_casos))) %>% 
  #gráfico
  ggplot() +
  #Casos acumuldos a la fecha x
  geom_line(aes(fecha_ok, n_casos), color="firebrick", lwd=.8) +
  geom_point(aes(fecha_ok, n_casos), shape=21, color="firebrick", fill="white",size=2) +
  #casos nuevos x día
  geom_line(aes(fecha_ok, casos.nuevos.dia), color="steelblue", lwd=.8) +
  geom_point(aes(fecha_ok, casos.nuevos.dia), shape=21, color="steelblue", fill="white",size=2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.02,0.02)) +
  labs(title = "**Coronavirus COVID-19:** Número de <span style='color:steelblue'>**nuevos casos**</span> y <span style='color:firebrick'>**total de casos acumulados**</span>",
       subtitle = str_glue("Casos confirmados desde el primer registro el 21 de enero 2020. *Actualización {now()}*  
                           *Eje y se muestra en diferentes escalas*"),
       y="casos (n°)\n",
       x="\ndías",
       caption = "@amadeob12 | Fuente: CSSE at Johns Hopkins University") +
  facet_wrap(~country_dif, scale = "free_y") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.line.x = element_line(size=.8),
        axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_markdown(color = "grey10", size=13),
        plot.subtitle = element_markdown(color = "grey30"),
        plot.caption = element_text(size=9, color="grey70"),
        strip.background.x = element_rect(fill = "grey90", color="grey90"))
)

ggsave("Covid-19_casos_comparacion.png", p2, width = 30, height = 15, units = "cm", dpi=500)




(p2_b <- bd_casos %>%
    mutate(country_dif = if_else(country_region=="Mainland China", "Mainland China", "Other")) %>% 
    group_by(country_dif, fecha_ok) %>% 
    summarize(casos_acumulados = sum(casos)) %>% 
    mutate(casos_nuevos_dia = c(casos_acumulados[1],diff(casos_acumulados))) %>% 
    gather(casos_acumulados:casos_nuevos_dia, key = "tipo_caso", value = "num") %>% 
    #gráfico
    ggplot(aes(fecha_ok, num, color=country_dif)) +
    geom_line(lwd=.8)+
    geom_point(shape=21, fill="white", size=2)+
    scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.02,0.02)) +
    scale_color_manual(values = c("gold3", "cyan4")) +
    labs(title = "**Coronavirus COVID-19:** Número de casos nuevos y acumulados por día en <span style='color:gold3'>**China**</span> y el <span style='color:cyan4'>**resto del mundo**</span>",
         subtitle = str_glue("Casos confirmados desde el primer registro el 21 de enero 2020. *Actualización {now()}*  
                           *Eje y se muestra en diferentes escalas*"),
         y="casos (n°)\n",
         x="\ndías",
         caption = "@amadeob12 | Fuente: CSSE at Johns Hopkins University") +
    facet_wrap(~tipo_caso, scale = "free_y") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.line.x = element_line(size=.8),
          axis.ticks.x = element_line(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          plot.title = element_markdown(color = "grey10", size=13),
          plot.subtitle = element_markdown(color = "grey30"),
          plot.caption = element_text(size=9, color="grey70"),
          strip.background.x = element_rect(fill = "grey90", color="grey90"),
          legend.position = "none")
)

  
ggsave("Covid-19_casos_comparacion_2.png", p2_b, width = 30, height = 15, units = "cm", dpi=500)


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#MUERTES
(bd_muertes <- muertes %>% 
    pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "fecha", values_to = "muertes", values_drop_na = TRUE) %>% 
    clean_names() %>% 
    #separate(fecha, into = c("fecha", "hora"), sep = " ") %>% 
    mutate(fecha_ok = mdy(fecha),
           muertes_ok = if_else(is.na(muertes), 0, muertes)) %>% 
    arrange(fecha_ok)
)

(muertes.fecha <- bd_muertes %>% 
  group_by(fecha_ok) %>% 
  summarize(n_muertes = sum(muertes)) %>% 
  #summarize(total.muertes = max(n_muertes)) %>% #hay dias con varios registros, como los datos son acumulados se debe dejar el último registro del día usando max()
  mutate(muertes.x.dia = c(n_muertes[1],diff(n_muertes)))
)



#RECUPERADOS
(bd_recuperados <- recuperados %>% 
    pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "fecha", values_to = "recuperados", values_drop_na = TRUE) %>% 
    clean_names() %>% 
    #separate(fecha, into = c("fecha", "hora"), sep = " ") %>% 
    mutate(fecha_ok = mdy(fecha),
           recuperados_ok = if_else(is.na(recuperados), 0, recuperados)) %>% 
    arrange(fecha_ok)
)

#prueba de cálculo de casos (ojo con fechas... hay mas de 1 dato por fecha)
(recuperados.fecha <- bd_recuperados %>% 
  group_by(fecha_ok) %>% 
  summarize(n_recuperados = sum(recuperados)) %>% 
  #summarize(total.rec = max(n_recuperados)) %>% #hay dias con varios registros, como los datos son acumulados se debe dejar el último registro del día usando max()
  mutate(recuperados.x.dia = c(n_recuperados[1],diff(n_recuperados)))
)



#Unir BD muertes y recuperados
#Gráfico razon entre recuperados y fallecidos ----
(p3 <- recuperados.fecha %>% 
  left_join(muertes.fecha, by = "fecha_ok") %>% 
  #select(fecha_ok, starts_with("total")) %>% 
  mutate(razon = round(n_recuperados/n_muertes, 1),
         texto = str_glue("{n_recuperados}\n /\n {n_muertes}")) %>% 
  ggplot() +
  geom_line(aes(fecha_ok, razon), color="green4", lwd=.8) +
  geom_point(aes(fecha_ok, razon), shape=21, color="green4", fill="white",size=2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0.03, 0.03)) +
  scale_y_continuous(expand = c(0.08,0.08), breaks=seq(0,100000,1)) +
  labs(title = "**Coronavirus COVID-19:** Relación entre el número de pacientes recuperados y fallecidos",
       subtitle = str_glue("Cálculo en base a valores acumulados en cada día. *Actualización {now()}*"),
       y="Razón (recuperados/fallecidos)\n",
       x="\ndías",
       caption = "@amadeob12 | Fuente: CSSE at Johns Hopkins University") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.line.x = element_line(size=.8),
        axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_markdown(color = "grey20", size=14),
        plot.subtitle = element_markdown(color = "grey40"),
        plot.caption = element_text(size=9, color="grey70"),
        strip.background.x = element_rect(fill = "grey90", color="grey90"))+
  geom_text(aes(fecha_ok, razon, label=texto), size=2)
)  

ggsave("Covid-19_razon_rec_muertes.png", p3, width = 30, height = 15, units = "cm", dpi=500)
  






###
###  Control de Procesos ================================================================================================
###

#LINK -> https://cran.r-project.org/web/packages/qcc/vignettes/qcc_a_quick_tour.html
#LINK2 -> https://luca-scr.github.io/qcc/reference/qcc.html

#install.packages("qcc", dependencies = TRUE)
library(qcc)



#======================================
# GRÁFICOS DE CONTROL DE SHEWART
#======================================

## 1.- CARTA DE CONTROL X
## [Periodo Previo a PCC-Caligus]
old <- qcc.options()
qcc.options(old) # restore old defaults

#customizar
qcc.options(cex=.8)
qcc.options("violating.runs" = list(col = "orange", pch=20))

#
###BBDD GENERAL
#
(datos_qcc <- bd_casos %>% 
    group_by(fecha_ok) %>% 
    summarize(n_casos = sum(casos)) %>% 
    mutate(casos.nuevos = c(n_casos[1],diff(n_casos))) %>% 
    select(-n_casos)
)
#hist(datos_qcc$casos.nuevos)
dim(datos_qcc)


qcc(datos_qcc$casos.nuevos, type = "xbar.one", axes.las =1, labels = datos_qcc$fecha_ok, add.stats=FALSE)

#punto fuera de control
datos_qcc_sin_13feb <- datos_qcc[-c(23,24), ]
qcc(datos_qcc_sin_13feb$casos.nuevos, type = "xbar.one", axes.las =1, labels = datos_qcc_sin_13feb$fecha_ok, add.stats=FALSE)




#
### BBDD CHINA vs. Otros
#
(datos_qcc_2 <- bd_casos %>% 
    mutate(country_dif = if_else(country_region=="Mainland China", "Mainland China", "Other")) %>% 
    group_by(country_dif, fecha_ok) %>% 
    summarize(n_casos = sum(casos)) %>% 
    mutate(casos.nuevos = c(n_casos[1],diff(n_casos))) %>% 
    select(-n_casos)
)


qcc(datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$casos.nuevos, type = "xbar.one", axes.las =1, labels = datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$fecha_ok)
qcc(datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$casos.nuevos[-c(23,24)], type = "xbar.one", axes.las =1, labels = datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$fecha_ok[-c(23,24)])

qcc(datos_qcc_2[datos_qcc_2$country_dif=="Other", ]$casos.nuevos, type = "xbar.one", axes.las =1, labels = datos_qcc_2[datos_qcc_2$country_dif=="Other", ]$fecha_ok)


## [Simulación de periodos de calibración y seguimiento entre China y resto del mundo]
qcc(datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$casos.nuevos[1:21], type = "xbar.one", 
    newdata=datos_qcc_2[datos_qcc_2$country_dif=="Other", ]$casos.nuevos[32:nrow(datos_qcc_2[datos_qcc_2$country_dif=="Other", ])],
         add.stats=TRUE)


## [Simulación de periodos de calibración y seguimiento entre China primeros 21 días y china desde el día 25 en adelante]
q1 <- qcc(datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$casos.nuevos[1:21], type = "xbar.one", 
    newdata=datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$casos.nuevos[25 : nrow(datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]) ],
    add.stats=TRUE, confidence.level=0.99)

class(q1)
summary(q1)

plot(q1, chart.all=FALSE)

(warn.limits = limits.xbar(q1$center, q1$std.dev, q1$sizes, 2))
plot(q1, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")



#CUSUM
cusum(datos_qcc_2[datos_qcc_2$country_dif=="Mainland China", ]$casos.nuevos[-c(23,24)], decision.interval = 4, se.shift = 1)










g1<- qcc(BDqcc[1:52,], type = "xbar", axes.las =2)
summary(g1)
# Desv. Est.
g1$std.dev
# Límites de control
g1$limits
g2$limits
# Línea central de la gráfica de control
g1$center 
g1$nsigmas
#Violaciones y corridas
g1$violations

## [Periodo Posterior a PCC-Caligus]
g2<- qcc(BDqcc[53:95,], type = "xbar")
summary(g2)
## [Simulación de periodos de calibración y seguimiento]
g3<- qcc(BDqcc[1:52,], type = "xbar", newdata=BDqcc[53:95,],add.stats=FALSE)
g3b<- qcc(BDqcc[1:52,], type = "xbar", newdata=BDqcc[53:95,],add.stats=FALSE,chart.all=FALSE)
g4<- qcc(BDqcc[53:95,], type = "xbar", newdata=BDqcc[1:52,],add.stats=FALSE)


## 2.- CARTA DE CONTROL S 
s1<- qcc(BDqcc[1:52,], type = "S")
s2<- qcc(BDqcc[53:95,], type = "S")
qcc(BDqcc[1:52,], type = "S", newdata=BDqcc[53:95,],add.stats=FALSE)