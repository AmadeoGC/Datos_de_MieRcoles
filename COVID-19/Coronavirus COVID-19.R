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
  separate(fecha, into = c("fecha", "hora"), sep = " ") %>% 
  mutate(fecha_ok = mdy(fecha)) %>% 
  arrange(fecha_ok)
)




#base (prueba) previa para calcular casos acumulados y casos nuevo x día
bd_casos %>% 
  group_by(fecha_ok, hora) %>% 
  summarize(n_casos = sum(casos)) %>% 
  summarize(total = max(n_casos)) %>% #hay dias con varios registros, como los datos son acumulados se debe dejar el último registro del día usando max()
  #mutate(casos.nuevos.x = c(0,diff(total))) %>%  #https://stackoverflow.com/questions/35169423/error-when-using-diff-function-inside-of-dplyr-mutate (idea original)
  mutate(casos.nuevos = c(total[1],diff(total)))


#Grafico de casos nuevos y acumulados (global) ----
(p1 <- bd_casos %>% 
  group_by(fecha_ok, hora) %>% 
  summarize(n_casos = sum(casos)) %>% 
  summarize(n_casos = max(n_casos)) %>% 
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
  labs(title = "**Coronavirus COVID-19:** Número de <span style='color:steelblue'>**nuevos casos**</span> y <span style='color:firebrick'>**total de casos acumulados**</span> en cada unidad de tiempo",
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
  geom_label(aes(x = ymd("2020-02-10")+0.6 , y = 23000, label = "Cambio en metodología para\n contabilizar nuevos casos \n(OMS)"),  hjust = 0.5, vjust = 0.5, 
             lineheight = 1, fill = "grey97", label.size = NA, size = 3) +
  geom_curve(aes(x = ymd("2020-02-10")+0.9, y = 19500, xend = ymd("2020-02-12")+0.8, yend = 15500), colour = "grey50", curvature = 0.1, size=0.3,
             arrow = arrow(length = unit(0.03, "npc")))
)  

ggsave("Covid-19_casos_general.png", p1, width = 30, height = 15, units = "cm", dpi=500)





#Grafico de casos nuevos y acumulados (China vs. Otros) ----
(p2 <- bd_casos %>%
  mutate(country_dif = if_else(country_region=="Mainland China", "Mainland China", "Other")) %>% 
  group_by(country_dif, fecha_ok, hora) %>% 
  summarize(n_casos = sum(casos)) %>% 
  summarize(n_casos = max(n_casos)) %>% 
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
       subtitle = str_glue("Casos confirmados desde el primer registro el 21 de enero 2020. *Actualización {now()}*\n"),
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

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#MUERTES
(bd_muertes <- muertes %>% 
    pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "fecha", values_to = "muertes", values_drop_na = TRUE) %>% 
    clean_names() %>% 
    separate(fecha, into = c("fecha", "hora"), sep = " ") %>% 
    mutate(fecha_ok = mdy(fecha),
           muertes_ok = if_else(is.na(muertes), 0, muertes)) %>% 
    arrange(fecha_ok)
)

(muertes.fecha <- bd_muertes %>% 
  group_by(fecha_ok, hora) %>% 
  summarize(n_muertes = sum(muertes)) %>% 
  summarize(total.muertes = max(n_muertes)) %>% #hay dias con varios registros, como los datos son acumulados se debe dejar el último registro del día usando max()
  mutate(muertes.x.dia = c(total.muertes[1],diff(total.muertes)))
)



#RECUPERADOS
(bd_recuperados <- recuperados %>% 
    pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "fecha", values_to = "recuperados", values_drop_na = TRUE) %>% 
    clean_names() %>% 
    separate(fecha, into = c("fecha", "hora"), sep = " ") %>% 
    mutate(fecha_ok = mdy(fecha),
           recuperados_ok = if_else(is.na(recuperados), 0, recuperados)) %>% 
    arrange(fecha_ok)
)

#prueba de cálculo de casos (ojo con fechas... hay mas de 1 dato por fecha)
(recuperados.fecha <- bd_recuperados %>% 
  group_by(fecha_ok, hora) %>% 
  summarize(n_recuperados = sum(recuperados)) %>% 
  summarize(total.rec = max(n_recuperados)) %>% #hay dias con varios registros, como los datos son acumulados se debe dejar el último registro del día usando max()
  mutate(recuperados.x.dia = c(total.rec[1],diff(total.rec)))
)



#Unir BD muertes y recuperados
#Gráfico razon entre recuperados y fallecidos ----
(p3 <- recuperados.fecha %>% 
  left_join(muertes.fecha, by = "fecha_ok") %>% 
  select(fecha_ok, starts_with("total")) %>% 
  mutate(razon = round(total.rec/total.muertes, 1),
         texto = str_glue("{total.rec}\n /\n {total.muertes}")) %>% 
  ggplot() +
  geom_line(aes(fecha_ok, razon), color="green4", lwd=.8) +
  geom_point(aes(fecha_ok, razon), shape=21, color="green4", fill="white",size=2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0.03, 0.03)) +
  scale_y_continuous(expand = c(0.05,0.05), breaks=seq(0,100000,1)) +
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
  geom_text(aes(fecha_ok, razon, label=texto), size=3.1)
)  

ggsave("Covid-19_razon_rec_muertes.png", p3, width = 30, height = 15, units = "cm", dpi=500)
  




