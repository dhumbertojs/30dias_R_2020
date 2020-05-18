#dia 7

library(dplyr)
library(ggridges)
library(scales)

inp <- "/Users/dhjs/Documents/projects/30dias_R_2020/datos"
out <- "/Users/dhjs/Documents/projects/30dias_R_2020/imagenes"

datos <- read.csv(paste(inp, "afluencia-preliminar-en-transporte-publico.csv", 
                        sep = "/"), fileEncoding = "UTF-8")

datos1 <- datos %>% 
  filter(ORGANISMO == "STC") %>% 
  mutate(
    LINEA.SERVICIO = factor(LINEA.SERVICIO,
                            levels = c("L1", "L2", "L3", "L4", 
                                       "L5", "L6", "L7", "L8",
                                       "L9", "LA", "LB", "L12")),
    DÍA.SEMANA = factor(DÍA.SEMANA, 
                        levels = c("Lunes", "Martes", "Miércoles", 
                                   "Jueves", "Viernes", "Sábado", "Domingo"))
    
  )

ggplot(datos1, aes(x = AFLUENCIA.TOTAL..Cifras.preliminares., y = LINEA.SERVICIO)) +
  geom_density_ridges() +
  facet_wrap(. ~ MES) +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  labs(title = "Densidad de las distintas estaciones del metro",
       subtitle = "Por línea del metro",
       x = "", y = "", 
       caption = "Fuente: Datos Abiertos de la Ciudad de México")
ggsave("Grafica_dia_7_a.png", path = out, dpi = 300)

ggplot(datos1, aes(x = AFLUENCIA.TOTAL..Cifras.preliminares., y = DÍA.SEMANA)) +
  geom_density_ridges() +
  facet_grid(MES ~ .) +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  labs(title = "Densidad de las distintas estaciones del metro",
       subtitle = "Por día de la semana",
       x = "", y = "", 
       caption = "Fuente: Datos Abiertos de la Ciudad de México")
ggsave("Grafica_dia_7_b.png", path = out, dpi = 300)

ggplot(datos, aes(x = AFLUENCIA.TOTAL..Cifras.preliminares., y = ORGANISMO)) +
  geom_density_ridges() +
  facet_wrap(. ~ MES) +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  labs(title = "Densidad de los distintos medios de transporte",
       subtitle = "Ciudad de México",
       x = "", y = "", 
       caption = "Fuente: Datos Abiertos de la Ciudad de México")
ggsave("Grafica_dia_7_c.png", path = out, dpi = 300)