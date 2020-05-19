#dia 8

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(janitor)
library(stringr)
library(scales)

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
    variable = str_remove_all(variable, "_..")
  ) %>% 
  pivot_wider(
    names_from = variable,
    values_from = valores
  )

ggplot(datos, aes(x = matrimonios, y = divorcios)) +
  geom_density_2d() +
  geom_jitter(alpha = 0.3) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  facet_wrap(. ~ year) +
  labs(title = "Gráfica de contornos", 
       subtitle = "Maatrimonios y divorcios en México", 
       x = "", y = "", 
       caption = "Funte: SIMBAD") +
  theme_classic()
ggsave("Grafica_dia_8.png", path = out, dpi = 300, height = 7, width = 10)
