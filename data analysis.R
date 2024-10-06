library(ggplot2)
library(tidyverse)
library(dplyr)
library(ratdat)
library(scales)
library(agridat)

banco_de_dados <- read.csv("banco de dados_finalizado.csv")
view(banco_de_dados)

banco_de_dados <- banco_de_dados %>%
  filter(!is.na(new_analitical_method)) %>%
  filter(!is.na(new_detection_method)) %>%
  filter(!is.na(new_environment)) %>%
  filter(!is.na(method)) %>%
  filter(!is.na(D)) %>%
  select(family, species, genus, D, Log10.D, SE, new_detection_method, new_analitical_method, method, bodymass_g_pantheria, new_environment, Lat, Long, Altitude, annual_precipitation, sampling_effort, country)
view(banco_de_dados)

with(banco_de_dados, tapply(D, list(new_detection_method, new_analitical_method), length))
table(banco_de_dados$method)

banco_de_dados$method <- as.factor(banco_de_dados$method)
banco_de_dados$new_environment <- as.factor(banco_de_dados$new_environment)
banco_de_dados$new_analitical_method <- as.factor(banco_de_dados$Lat)
banco_de_dados$new_detection_method <- as.factor(banco_de_dados$Altitude)
banco_de_dados$new_analitical_method <- as.factor(banco_de_dados$annual_precipitation)

t <- lm(log(D) ~ method + 
          log(banco_de_dados$bodymass_g_pantheria/1000) + bodymass_g_pantheria +
          new_environment +
          abs(Lat) + Lat + log10(Altitude) +Altitude + 
          annual_precipitation + I(annual_precipitation^2), 
        data = banco_de_dados)
summary(t)
plot(t)
