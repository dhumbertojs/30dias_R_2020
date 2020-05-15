#dia 3

library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(scales)
library(ggrepel)

inp <- "/Users/dhjs/Documents/projects/30dias_R_2020/datos/felicidad"
out <- "/Users/dhjs/Documents/projects/30dias_R_2020/imagenes"

gdp <- read.csv(paste(inp, "API_NY.GDP.MKTP.CD_DS2_en_csv_v2_988718.csv", sep = "/"), skip =3)
gdp <- gdp %>% 
  select(Country.Name, X2018) %>% 
  rename(gdp = X2018)

percent <- read.csv(paste(inp, "fcd174bf-1a86-4cd9-9894-30e4957dd1c0_Data.csv", sep = "/"))
percent <- percent %>% 
  select(Country.Name, X2018..YR2018.) %>% 
  rename(percent = X2018..YR2018.) %>% 
  mutate(percent = as.numeric(percent)/100)

gini <- read.csv(paste(inp, "API_SI.POV.GINI_DS2_en_csv_v2_988343.csv", sep = "/"), skip = 3)
gini <- gini %>% 
  select(Country.Name, X2018) %>% 
  rename(gini = X2018)

happy <- read_excel(paste(inp, "Chapter2OnlineData.xls", sep = "/")) %>% 
  filter(Year == 2018) %>% 
  clean_names() %>% 
  select(country_name, life_ladder) %>% 
  rename(Country.Name = country_name) 

fin <- happy %>% 
  left_join(gini) %>% 
  left_join(percent) %>% 
  left_join(gdp) %>% 
  filter(!is.na(gini))

ggplot(fin, aes(x = gdp, y = percent, color = gini, label = Country.Name)) +
  geom_jitter(aes(size = life_ladder)) +
  scale_color_viridis_c() +
  scale_y_continuous(labels = percent_format(), breaks = seq(0, .9, .05)) +
  scale_x_continuous(labels = comma) +
  geom_text_repel() +
  labs(
    title = "GDP(PPP) y %PEA",
    subtitle = "GINI y felicidad",
    x = "", y = "", color = "GINI",
    caption = "Fuente: Datos del Banco Mundial\nWorl Happiness Report"
  ) +
  guides(size = F) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("Grafica_dia_3.png", path = out, dpi = 300)  
