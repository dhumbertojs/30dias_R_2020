#dia 6
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

inp <- "/Users/dhjs/Documents/projects/30dias_R_2020/datos"
out <- "/Users/dhjs/Documents/projects/30dias_R_2020/imagenes"

mun <- read.csv(paste(inp, "municipios.csv", sep = "/"))

mun <- mun %>% 
  mutate(
    X2019 = sapply(strsplit(X2019, "_"), "[", 1)
  )

data <- mun %>% 
  group_by(Estado, X2019) %>% 
  count() %>%
  rename(count = n) %>% 
  ungroup() %>% 
  group_by(Estado) %>% 
  mutate(
    fraction = count/sum(count),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(X2019, "\n value: ", count)
  ) %>% 
  filter(Estado == "Estado_de_Mexico")

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=X2019)) +
  geom_rect() +
  geom_label_repel(x=4, aes(y=labelPosition, label=label), size=4) +
  scale_fill_manual(values = c(
    "PAN" = "#153588",
    "PRI" = "#E13A27",
    "PRD" = "#F6D626",
    "PT" = "#DE1830",
    "PVEM" = "#159C4A",
    "MC" = "#F08B34",
    "Morena" = "#B6261E",
    "PNA" = "#49A3AB",
    "VR" = "#e31c73"
  )) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(. ~ Estado) +
  labs(
    title = "Municipios gobernados por partido político",
    subtitle = "Estado de México, 2019",
    caption = "Fuente: Centro de Estudios Alonso Lujambio"
  )
