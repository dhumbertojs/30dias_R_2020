#Dia 1

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
  group_by(year, variable) %>% 
  summarise(
    total = mean(valores, na.rm = T)
  ) %>% 
  ggplot(aes(x = year, y = total, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = 2010:2017) +
  labs(title = "Promedio por año de matrimonios y divorcios en México",
       subtitle = "2010 al 2017",
       x = "", y = "", fill = "", 
       caption = "Fuente: SIMBAD de INEGI\nEl año 2014 no está en su datos") +
  theme(legend.position = "bottom")
ggsave("Grafica_dia_1.png", path = out, dpi = 300)