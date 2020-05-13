#Dia 2

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(janitor)
library(stringr)

inp <- "/Users/dhjs/Documents/projects/30dias_R_2020/datos"
out <- "/Users/dhjs/Documents/projects/30dias_R_2020/imagenes"
list.files(inp)

datos <- read_excel(paste(inp, "SIMBAD_57193_20200512035919611.xlsx", sep = "/"), skip = 3)

datos <- clean_names(datos) %>% 
  filter(clave <= 33) %>% 
  select(clave, nombre,
         matrimonios_6, divorcios_7,
         matrimonios_11, divorcios_12,
         matrimonios_16, divorcios_17,
         matrimonios_21, divorcios_22,
         matrimonios_26, divorcios_27,
         matrimonios_31, divorcios_32,
         matrimonios_36, divorcios_37) %>% 
  rename(
    matrimonios_10 = matrimonios_6, divorcios_10 = divorcios_7,
    divorcios_11 = divorcios_12,
    matrimonios_12 = matrimonios_16, divorcios_12 = divorcios_17,
    matrimonios_13 = matrimonios_21, divorcios_13 = divorcios_22,
    matrimonios_15 = matrimonios_26, divorcios_15 = divorcios_27,
    matrimonios_16 = matrimonios_31, divorcios_16 = divorcios_32,
    matrimonios_17 = matrimonios_36, divorcios_17 = divorcios_37
  ) %>% 
  pivot_longer(
    c(matrimonios_10, divorcios_10, matrimonios_11, divorcios_11, matrimonios_12, divorcios_12,  
      matrimonios_13, divorcios_13, matrimonios_15, divorcios_15, matrimonios_16, divorcios_16,
      matrimonios_17, divorcios_17),
    names_to = "variable",
    values_to = "valores"
  ) %>% 
  mutate(
    year = sapply(strsplit(variable, "_"), "[", 2),
    year = as.numeric(paste0(20, year)),
    variable = str_remove_all(variable, "_.."),
    variable = factor(variable, 
                      levels = c("matrimonios", "divorcios"))
  )

datos %>% 
  group_by(clave, nombre) %>% 
  summarise(mean = mean(valores, na.rm = T)) %>% 
  arrange(desc(mean))

top_10 <- c("15", "09", "14", "30", "11", "19")

datos %>% 
  filter(variable == "matrimonios" & clave %in% top_10) %>% 
  ggplot(aes(x = year, y = valores, group = nombre)) +
  geom_line(aes(color = nombre), size = 1.5) +
  geom_point(aes(color = nombre), size = 2) +
  scale_x_continuous(breaks = 2010:2017) +
  scale_y_continuous(breaks = seq(25000, 75000, by = 10000)) +
  theme(legend.position = "none") +
  annotate("text", x = 2011, y = 63000, label = "Nuevo León") +
  annotate("text", x = 2011, y = 43000, label = "Veracruz") +
  annotate("text", x = 2015, y = 38000, label = "Jalisco") +
  annotate("text", x = 2011, y = 36000, label = "CDMX") +
  annotate("text", x = 2012, y = 33000, label = "Guanajuato") +
  annotate("text", x = 2011, y = 23000, label = "EdoMex") +
  labs(title = "Total de matrimonios por entidad federativa",
       x = "", y = "", 
       caption = "Fuente: Estadísticas vitales (SIMBAD)")
ggsave("Grafica_dia_2_a.png", path = out, dpi = 300)

datos %>% 
  filter(variable == "divorcios" & clave %in% top_10) %>% 
  ggplot(aes(x = year, y = valores, group = nombre)) +
  geom_line(aes(color = nombre), size = 1.5) +
  geom_point(aes(color = nombre), size = 2) +
  scale_x_continuous(breaks = 2010:2017) +
  theme(legend.position = "bottom") +
  labs(title = "Total de divorcios por entidad federativa",
       x = "", y = "",
       caption = "Fuente: Estadísticas vitales (SIMBAD)"
       )
ggsave("Grafica_dia_2_b.png", path = out, dpi = 300)