#dia 4

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)

inp <- "/Users/dhjs/Documents/projects/30dias_R_2020/datos"
out <- "/Users/dhjs/Documents/projects/30dias_R_2020/imagenes"

tr <- read.csv(paste(inp, "2019.csv", sep = "/"))

clean <- tr %>% 
  mutate(
    registro = ymd(registro),
    fecha_respuesta = ymd(fecha_respuesta),
    
    mes_registro = month(registro),
    mes_respuesta = month(fecha_respuesta),
    
    respondido = ifelse(is.na(archivo_respuesta), 0, 1)
  ) %>% 
  filter(str_detect(ente_obligado, "Alcaldía")) %>% 
  select(-solicitud_de_informacion) %>% 
  group_by(ente_obligado, mes_registro) %>% 
  summarise(
    total = n(),
    respon = sum(respondido)
  ) %>% 
  ungroup() %>% 
  mutate(
    porc = respon/total,
    mes_registro = factor(mes_registro, 
                          levels = c(1:12)), 
    ente_obligado = str_remove_all(ente_obligado, "Alcaldía ")
  )

ggplot(clean, aes( x = mes_registro, y = porc)) +
  geom_bar(stat = "identity", fill = "#3b929a") +
  facet_wrap(. ~ ente_obligado) +
  labs(title = "% de solicitudes de información respondidas", 
       subtitle = "2019", x = "", y = "", 
       caption = "Fuente: INFOMEX CDMX") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Enero", "Febrero", "Marzo", "Abril",
                              "Mayo", "Junio", "Julio", "Agosto",
                              "Septiembre", "Octubre", "Noviembre",
                              "Diciembre")) +
  theme(axis.text.x = element_text(angle = 90))
ggsave("Grafica_dia_4.png", path = out, dpi = 300, width = 5, height = 10)
