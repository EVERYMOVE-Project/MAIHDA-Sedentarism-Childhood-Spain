## Project: Childhood Inequities of Sedentarism
## Script: Data Cleaning INE Datasets
## Finalized: July 28 2025
## Edited: July 22 2026

## Load libraries ----
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
library(scales)
library(labelled)
library(readr)
library(readxl)
library(segmented)
library(glmmTMB)
library(sjPlot)

setwd("~/UAH/PhD Documents/INEdatos/Analysis")

# Databases: ####
### ENSE 2003 - INFANT03.txt, HOGAR03.txt, ADULTO03.txt
### ENSE 2006 - INFANT06.txt, HOGAR06.txt, ADULTO06.txt
### ENSE 2012 - INFANT12.txt, HOGAR12.txt, MicrodatoAdultos.txt
### ENSE 2017 - MICRODAT.CM.txt, MICRODAT.CH.txt, MICRODAT.CA.txt
### ESDE 2023 - INFANT23.RData, HOGAR23.RData, ADULTOS23.RData

## ENSE 2003 ####
## menores
menores2003 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2003ENSE/Infantil-ENSE-2003/codebook_menores2003.xlsx")
names_menores2003 <- menores2003$VARIABLE
width_menores2003  <- menores2003$LONGITUD %>% as.numeric

menores2003 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2003ENSE/Infantil-ENSE-2003/INFANT03.txt", col_positions = fwf_widths(widths = width_menores2003, col_names = names_menores2003))
rm(names_menores2003, width_menores2003)

## hogar
hogar2003 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2003ENSE/Hogar-ENSE-2003/codebook_hogar2003.xlsx")
names_hogar2003 <- hogar2003$VARIABLE
width_hogar2003  <- hogar2003$LONGITUD %>% as.numeric

hogar2003 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2003ENSE/Hogar-ENSE-2003/HOGAR03.txt", col_positions = fwf_widths(widths = width_hogar2003, col_names = names_hogar2003))
rm(names_hogar2003, width_hogar2003)

## adultos (16-18)
adultos2003 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2003ENSE/Adultos-ENSE-2003/codebook_adultos2003.xlsx")
names_adultos2003 <- adultos2003$VARIABLE
width_adultos2003  <- adultos2003$LONGITUD %>% as.numeric

adultos2003 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2003ENSE/Adultos-ENSE-2003/ADULTO03.txt", col_positions = fwf_widths(widths = width_adultos2003, col_names = names_adultos2003))
rm(names_adultos2003, width_adultos2003)

## ENSE 2006 ####
menores2006 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2006ENSE/Infantil-ENSE-2006/codebook_menores2006.xlsx")
names_menores2006 <- menores2006$VARIABLE
width_menores2006  <- menores2006$LONGITUD %>% as.numeric

menores2006 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2006ENSE/Infantil-ENSE-2006/INFANT06.txt", col_positions = fwf_widths(widths = width_menores2006, col_names = names_menores2006))
rm(names_menores2006, width_menores2006)

## hogar
hogar2006 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2006ENSE/Hogar-ENSE-2006/codebook_hogar2006.xlsx")
names_hogar2006 <- hogar2006$VARIABLE
width_hogar2006  <- hogar2006$LONGITUD %>% as.numeric

hogar2006 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2006ENSE/Hogar-ENSE-2006/HOGAR06.txt", col_positions = fwf_widths(widths = width_hogar2006, col_names = names_hogar2006))
rm(names_hogar2006, width_hogar2006)

## adultos (16-18)
adultos2006 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2006ENSE/Adulto-ENSE-2006/codebook_adultos2006.xlsx")
names_adultos2006 <- adultos2006$VARIABLE
width_adultos2006  <- adultos2006$LONGITUD %>% as.numeric

adultos2006 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2006ENSE/Adulto-ENSE-2006/ADULTO06.txt", col_positions = fwf_widths(widths = width_adultos2006, col_names = names_adultos2006))
rm(names_adultos2006, width_adultos2006)

## ENSE 2011 ####
menores2012 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2012ENSE/codebook_menores2011.xlsx")
names_menores2012 <- menores2012$VARIABLE
width_menores2012  <- menores2012$LONGITUD %>% as.numeric

menores2012 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2012ENSE/datos_ensalud12/INFANT12.txt", col_positions = fwf_widths(widths = width_menores2012, col_names = names_menores2012))
rm(names_menores2012, width_menores2012)

## hogar
hogar2012 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2012ENSE/codebook_hogar2011.xlsx")
names_hogar2012 <- hogar2012$VARIABLE
width_hogar2012  <- hogar2012$LONGITUD %>% as.numeric

hogar2012 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2012ENSE/datos_ensalud12/HOGAR12.txt", col_positions = fwf_widths(widths = width_hogar2012, col_names = names_hogar2012))
rm(names_hogar2012, width_hogar2012)

## adultos (16-18)
adultos2011 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2012ENSE/codebook_adultos2011.xlsx")
names_adultos2011 <- adultos2011$VARIABLE
width_adultos2011  <- adultos2011$LONGITUD %>% as.numeric

adultos2011 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2012ENSE/datos_ensalud12/ADULTO12.txt", col_positions = fwf_widths(widths = width_adultos2011, col_names = names_adultos2011))
rm(names_adultos2011, width_adultos2011)

## ENSE 2017 ####
menores2017 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2017ENSE/codebook_menores2017.xlsx")
names_menores2017 <- menores2017$VARIABLE
width_menores2017  <- menores2017$LONGITUD %>% as.numeric

menores2017 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2017ENSE/Menores_ENSE17/MICRODAT.CM.txt", col_positions = fwf_widths(widths = width_menores2017, col_names = names_menores2017))
rm(names_menores2017, width_menores2017)

## hogar
hogar2017 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2017ENSE/codebook_hogar2017.xlsx")
names_hogar2017 <- hogar2017$VARIABLE
width_hogar2017  <- hogar2017$LONGITUD %>% as.numeric

hogar2017 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2017ENSE/HOGAR_ENSE17/MICRODAT.CH.txt", col_positions = fwf_widths(widths = width_hogar2017, col_names = names_hogar2017))
rm(names_hogar2017, width_hogar2017)

## adultos (16-18)
adultos2017 <- read_excel("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2017ENSE/codebook_adultos2017.xlsx")
names_adultos2017 <- adultos2017$VARIABLE
width_adultos2017  <- adultos2017$LONGITUD %>% as.numeric

adultos2017 <- read_fwf("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2017ENSE/Adultos_ENSE2017/MICRODAT.CA.txt", col_positions = fwf_widths(widths = width_adultos2017, col_names = names_adultos2017))
rm(names_adultos2017, width_adultos2017)

## ENSE 2023 ####
## menores
load("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2023ESdE/ESdEmenor_2023/R/INFANT23.RData")
menores2023 <- Microdatos

## hogar
load("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2023ESdE/ESdEhogar_2023/R/HOGAR23.RData")
hogar2023 <- Microdatos
rm(Microdatos, Metadatos)

## adultos (16-18)
load("~/UAH/PhD Documents/INEdatos/Analysis/Raw Data/2023ESdE/ESdEadulto_2023/R/ADULTO23.RData")
adultos2023 <- Microdatos
rm(Microdatos, Metadatos)

# Select variables ####
## ENSE 2003 ####
# menores2003
ense2003m <- menores2003 %>% dplyr::select(
  NIDENTIF, N_INF, SEXO, EDAD, SUJ_ENTR, SPCLASE, FACTOR, D_ACFISO,
  PESO, ALTURA, TMUNI, CCAA)

# rename with matching order ## do not have to run this code again
ense2003m <- ense2003m %>% rename(
  id = NIDENTIF,
  n_inf = N_INF,
  sexo = SEXO,
  edad = EDAD,
  n_orden_menor = SUJ_ENTR,
  clase = SPCLASE,
  factor = FACTOR,
  sedentarismo = D_ACFISO,
  peso = PESO,
  altura = ALTURA,
  tamano = TMUNI,
  ccaa = CCAA
)

# hogar2003
hogar2003 <- hogar2003 %>% 
  rename(nacionalidad = NACION, 
         id = NIDENTIF, n_inf = NORDEN)


# join home-level info directly to child ID
ense2003 <- ense2003m %>%
  left_join(hogar2003 %>% select(id, nacionalidad), by = "id")

# new survey variable
ense2003 <- ense2003 %>% 
  mutate(survey = "2003")

### FINAL DATABASE 2003 AGES 0 to 15
save(ense2003, file = "ense2003.Rdata")

## ENSE 2006 ####
# menores2006
ense2006m <- menores2006 %>%
  select(NIDENTIF, SEXO, EDAD, NORDEN, SPCLASE, FACTOR, G1_57,
         K95, K96, MSIMC, TMUNI, ESTRATO, CCAA, P4_0_2) %>%
  rename(
    id = NIDENTIF,
    sexo = SEXO,
    edad = EDAD,
    n_orden_menor = NORDEN,
    clase = SPCLASE,
    factor = FACTOR,
    sedentarismo = G1_57,
    peso = K95,
    altura = K96,
    imc = MSIMC, # n = 915 missing
    tamano = TMUNI,
    estrato = ESTRATO,
    ccaa = CCAA,
    n_inf = P4_0_2
  )
summary(ense2006m$nacionalidad)
hogar2006 <- hogar2006 %>%
  rename(
    nacionalidad = B3,
    id = NIDENTIF,
    n_inf = NORDEN
  )

# join home-level info directly to child ID
ense2006 <- ense2006m %>%
  left_join(hogar2006 %>% select(id, nacionalidad), by = "id")

# new survey variable
ense2006 <- ense2006 %>% 
  mutate(survey = "2006")

### FINAL DATABASE 2006 AGES 0 to 15
save(ense2006, file = "ense2006.Rdata")

## ENSE 2011 ####
# menores2012
ense2011m <- menores2012 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, A7_2m, CLASE_PR, FACTORMENOR, K61,
  J57, J58, IMCm, ESTRATO, CCAA, A2_1b)

# rename relevant variables
ense2011m <- ense2011m %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a,
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = A7_2m,
  clase = CLASE_PR, # 333 missing
  factor = FACTORMENOR,
  sedentarismo = K61, # nine entries with 9 
  peso = J57, # three entries with 999
  altura = J58, # seven entries with 999
  imc = IMCm,
  estrato = ESTRATO,
  ccaa = CCAA,
  nacionalidad = A2_1b
)

ense2011m %>% 
  count(peso = 999)
# adultos2011
adultos2011 <- adultos2011 %>% select(
  IDENTHOGAR, SEXOa, EDADa, PROXY_2b, A7_2a, CLASE_PR, FACTORADULTO, U129, ESTRATO,
  R102, R103, IMCa, CCAA, E2_1b
)

# rename relevant variables
adultos2011 <- adultos2011 %>% rename( # n = 21,007
  id = IDENTHOGAR,
  n_inf = PROXY_2b, # 0.379% missing n = 23
  sexo = SEXOa,
  edad = EDADa,
  n_orden_menor = A7_2a,
  clase = CLASE_PR, # 93.5% missing n = 19,635 (all 566 of 16-18 are NA)
  factor = FACTORADULTO,
  sedentarismo = U129, # 100% missing n = 21,007
  estrato = ESTRATO,
  peso = R102, # 99.3% missing n = 20,855
  altura = R103,
  imc = IMCa, # 9.02% missing n = 547
  ccaa = CCAA,
  nacionalidad = E2_1b # 9.27% missing n = 562
)

# join adultos <= 15 and hogar databases
adultos2011$edad <- as.numeric(adultos2011$edad) # to be able to filter
ense2011m$edad <- as.numeric(ense2011m$edad) # to match variable type

ense2011a <- adultos2011 %>% 
  filter(edad <= 15)

# merge menores and adultos <= 15 databases
# check variable names 
names(ense2011m)
names(ense2011a)

# ensure variables all same type
ense2011a$n_inf <- as.character(ense2011a$n_inf)
ense2011m$n_orden_menor <- as.character(ense2011m$n_orden_menor)
ense2011m$peso <- as.numeric(ense2011m$peso)

# combined menores and adults aged 0-15
ense2011 <- bind_rows(ense2011m, ense2011a)

# new survey variable
ense2011 <- ense2011 %>% 
  mutate(survey = "2011")

### FINAL DATABASE 2011 AGES 0 to 15
save(ense2011, file = "ense2011.Rdata")

## ENSE 2017 ####
# menores2017
ense2017m <- menores2017 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, A7_2m, CLASE_PR, FACTORMENOR, K61,
  J57, J58, IMCm, CCAA, A2_1b)
# rename relevant variables
ense2017m <- ense2017m %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a,
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = A7_2m,
  clase = CLASE_PR,
  factor = FACTORMENOR,
  sedentarismo = K61,
  peso = J57,
  altura = J58,
  imc = IMCm,
  ccaa = CCAA,
  nacionalidad = A2_1b
)

# hogar2017
hogar2017 <- hogar2017 %>% rename(n_inf = NORDEN_Ai, estrato = ESTRATO, 
                                  id = IDENTHOGAR)

# adultos2017
adultos2017 <- adultos2017 %>% select(
  IDENTHOGAR, SEXOa, EDADa, PROXY_2b, A7_2a, CLASE_PR, FACTORADULTO, T112,
  S110, S109, IMCa, CCAA, E2_1b
)

# rename relevant variables
adultos2017 <- adultos2017 %>% rename(
  id = IDENTHOGAR,
  n_inf = PROXY_2b, # 97% missing n = 22,404
  sexo = SEXOa,
  edad = EDADa,
  n_orden_menor = A7_2a,
  clase = CLASE_PR,
  factor = FACTORADULTO,
  sedentarismo = T112,
  peso = S110,
  altura = S109,
  imc = IMCa,
  ccaa = CCAA,
  nacionalidad = E2_1b
)

# join databases
# menores and hogar
ense2017m <- ense2017m %>%
  left_join(hogar2017 %>% select(id, n_inf, estrato), join_by("id", "n_inf")) 
summary(ense2017m$estrato) 

# join adultos <= 15 and hogar databases
adultos2017$edad <- as.numeric(adultos2017$edad) # to apply filter
ense2017m$edad <- as.numeric(ense2017m$edad)

# join databases for estrato despite creating duplicates (get missing of estrato because many n_inf missing)
ense2017a <- adultos2017 %>% 
  filter(edad <= 15) %>% 
  left_join(hogar2017 %>% 
              select(id, estrato), join_by("id")) %>%
  distinct(id, .keep_all = TRUE)  # Keep only one row per id

# merge menores and adultos <= 15 databases
# check variable names 
names(ense2017m)
names(ense2017a)

# check all same variable types
ense2017m$n_orden_menor <- as.character(ense2017m$n_orden_menor)
ense2017m$altura <- as.numeric(ense2017m$altura)

# combined menores (all <= 15)
ense2017 <- bind_rows(ense2017m, ense2017a)

# new survey variable
ense2017 <- ense2017 %>% 
  mutate(survey = "2017")

### FINAL DATABASE 2017 AGES 0 to 15
save(ense2017, file = "ense2017.Rdata")

## ENSE 2023 ####
# menores2023
ense2023m <- menores2023 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, NORDENm, FACTORMENOR, K2m,
  J1m, J2m, IMC, CCAA, A4_2m)

# rename relevant variables
ense2023m <- ense2023m %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a, # n = 2 missing
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = NORDENm,
  factor = FACTORMENOR,
  sedentarismo = K2m,
  peso = J1m,
  altura = J2m,
  imc = IMC, # n = 339 missing prior to merge
  ccaa = CCAA,
  nacionalidad = A4_2m
)

# adultos2023
adultos2023 <- adultos2023 %>% select(
  IDENTHOGAR, SEXOa, EDADa, PROXY_2b, NORDENa, FACTORADULTO, O2, 
  N2, N1, IMC, CCAA, A2a_2)

# rename relevant variables
adultos2023 <- adultos2023 %>% rename(
  id = IDENTHOGAR,
  n_inf = PROXY_2b,
  sexo = SEXOa,
  edad = EDADa,
  n_orden_menor = NORDENa,
  factor = FACTORADULTO,
  sedentarismo = O2,
  peso = N2,
  altura = N1,
  imc = IMC,
  ccaa = CCAA,
  nacionalidad = A2a_2
)

# hogar2023
hogar2023 <- hogar2023 %>% rename(n_inf = NORDEN, estrato = ESTRATO, clase = CLASE_PR,
                                  id = IDENTHOGAR)
# join menores and hogar databases
ense2023m <- ense2023m %>%
  left_join(hogar2023 %>% select(id, n_inf, estrato, clase), join_by("id", "n_inf"))

# join adultos <= 15 and hogar databases
ense2023a <- adultos2023 %>% 
  filter(edad <= 15) %>% 
  left_join(hogar2023 %>% 
              select(id, estrato, clase), 
            join_by("id")) %>%
  distinct(id, .keep_all = TRUE)  # Keep only one row per id

# merge menores and adultos <= 15 databases
# check variable names 
# names(ense2023m)
# names(ense2023a)

# combined menores and adults (all <= 15)
ense2023 <- bind_rows(ense2023m, ense2023a)

# new survey variable
ense2023 <- ense2023 %>% 
  mutate(survey = "2023")

### FINAL DATABASE 2023 AGES 0 to 15
save(ense2023, file = "ense2023.Rdata")
# Homogenize variables ####

# Check missingness ####
# Missing values table
## ense2003
na_table_2003 <- ense2003 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2003)*100)

print(na_table_2003, n = 14)

## ense2006
na_table_2006 <- ense2006 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2006)*100)
print(na_table_2006, n = 17)

## ense2011
na_table_2011 <- ense2011 %>% 
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2011)*100)
print(na_table_2011, n = 15)

## raw adultos2011 data missingness
na_table_2011a <- adultos2011 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(adultos2011)*100)
print(na_table_2011a, n = 577)

## ense2017
na_table_2017 <- ense2017 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2017)*100)
print(na_table_2017, n = 15)

## ense2023
na_table_2023 <- ense2023 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2023)*100)
print(na_table_2023, n = 15)

# Label Definitions ####
ccaa_labels <- c(
  "Andalusia", "Aragon", "Asturias", "Balearic Islands", "Canary Islands",
  "Cantabria", "Castile and Leon", "Castilla-La Mancha", "Catalonia",
  "Valencian Community", "Extremadura", "Galicia", "Madrid", "Murcia",
  "Navarre", "Basque Country", "La Rioja", "Ceuta and Melilla")

clase_labels <- c(
  "Class I",
  "Class II",
  "Class III",
  "Class IV",
  "Class V",
  "Class VI")

tamano_labels <- c(
  "≤2,000 inhabitants",
  "2,001–10,000 inhabitants",
  "10,001–50,000 inhabitants",
  "50,001–100,000 inhabitants",
  "100,001–400,000 inhabitants",
  "400,001–1,000,000 inhabitants",
  ">1,000,000 inhabitants")

PA_labels <- c(
  "Does not exercise",
  "Occasional physical or sports activity",
  "Physical activity several times a month",
  "Sports or physical training several times a week"
)

estrato_labels <- c(
  "Municipalities with more than 500,000 inhabitants",
  "Provincial capital municipalities (except the prior)",
  "Municipalities with more than 100,000 inhabitants (except the prior)",
  "Municipalities with 50,000 to 100,000 inhabitants (except the prior)",
  "Municipalities with 20,000 to 50,000 inhabitants (except the prior)",
  "Municipalities with 10,000 to 20,000 inhabitants",
  "Municipalities with less than 10,000 inhabitants"
)

# Data Cleaning ####
## ENSE 2003 ####
ense2003_rename <- ense2003 %>%
  mutate(
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 6), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = factor(as.numeric(ccaa), levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 ~ "Spanish", 
      nacionalidad == 6 ~ "Foreign"
    ),
    nacionalidad = factor(nacionalidad),
    
    # Occupation class
    clase = na_if(clase, 7),
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr = cume_dist(as.numeric(clase)),
    
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III")),
    clase_tr_4 = cume_dist(as.numeric(clase_3)),
    
    clase_tr_2 = {
      # counts per class
      class_counts <- as.numeric(table(clase))
      # fraction of total population
      class_props <- class_counts / sum(class_counts)
      # cumulative proportion
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase)] - class_props[as.numeric(clase)]/2
    },
    
    clase_tr_3 = {
      # counts per class (within this dataset; if this dataset is one year, it's per year)
      class_counts <- as.numeric(table(clase_3))
      class_props <- class_counts / sum(class_counts)
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase_3)] - class_props[as.numeric(clase_3)]/2
    },
    
    # Municipality size
    tamano = factor(
      tamano,
      levels = 1:7,
      labels = tamano_labels,
      ordered = TRUE
    ),
    
    # Urban/rural
    urb_rur = case_when(
      as.numeric(tamano) %in% c(1, 2) ~ "Rural", # <10,000 habitantes
      as.numeric(tamano) == 3 ~ "Semi-urban", # <50,000 habitantes
      as.numeric(tamano) %in% c(4, 5, 6, 7) ~ "Urban", # > 50,000 habitantes a 1,000,000
      TRUE ~ NA_character_
    ),
    
    # Convert urb_rur to an ordered factor
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban")),
    
    # Anthropometry
    peso = as.numeric(na_if(peso, "999")), 
    altura = as.numeric(na_if(altura, "999")),
    imc_num = round(peso / (altura / 100)^2, 2),
    imc = NA, # placeholder
    
    obesity = NA,
    overweight = NA,
    
    # Sedentarism
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes",
      sedentarismo == 2 ~ "No"
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    row_id = row_number()
  )

## ENSE 2006 ####
ense2006_rename <- ense2006 %>% 
  mutate( 
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 6), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = factor(as.numeric(ccaa), levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 | nacionalidad == 3 ~ "Spanish",
      nacionalidad == 2 ~ "Foreign",
      nacionalidad == 9 ~ NA_character_
    ),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr=cume_dist(as.numeric(clase)),
    
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III")),
    clase_tr_4 = cume_dist(as.numeric(clase_3)),
    
    clase_tr_2 = {
      # counts per class
      class_counts <- as.numeric(table(clase))
      # fraction of total population
      class_props <- class_counts / sum(class_counts)
      # cumulative proportion
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase)] - class_props[as.numeric(clase)]/2
    },
    
    clase_tr_3 = {
      # counts per class (within this dataset; if this dataset is one year, it's per year)
      class_counts <- as.numeric(table(clase_3))
      class_props <- class_counts / sum(class_counts)
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase_3)] - class_props[as.numeric(clase_3)]/2
    },
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban")),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),
    
    # tamano
    tamano = factor(
      tamano,
      levels = 1:7,
      labels = tamano_labels,
      ordered = TRUE
    ),
    
    # # Urban/rural
    # urb_rur = case_when(
    #   as.numeric(tamano) %in% c(1, 2) ~ "Rural",
    #   as.numeric(tamano) == 3 ~ "Semi-urbano",
    #   as.numeric(tamano) %in% c(4, 5, 6, 7) ~ "Urbano",
    #   TRUE ~ NA_character_
    # ),
    
    # # Convert urb_rur to an ordered factor
    # urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban"), ordered = TRUE),
    
    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("998", "999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("998", "999"), NA, altura)),
    imc = na_if(imc, 9),  # Treat 9 ("Not reported") as missing
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),

    # Physical Activity
    sedentarismo = na_if(as.numeric(sedentarismo), 9),
    # PA = factor(sedentarismo, levels = 1:4, labels = PA_labels), 
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4 ~ "No" # not sedentary
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    row_id = row_number()
  )

## ENSE 2011 ####
ense2011_rename <- ense2011 %>% 
  mutate( 
    # Weights and age
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 2), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = as.numeric(ccaa),
    ccaa = ifelse(ccaa == 19, 18, ccaa),  # Merge Melilla and Ceuta
    ccaa = factor(ccaa, levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad %in% c(2, 6) ~ "Spanish",
      nacionalidad == 1 ~ "Foreign",
      TRUE ~ NA_character_
    ),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr=cume_dist(as.numeric(clase)),
    
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III")),
    clase_tr_4 = cume_dist(as.numeric(clase_3)),
    
    clase_tr_2 = {
      # counts per class
      class_counts <- as.numeric(table(clase))
      # fraction of total population
      class_props <- class_counts / sum(class_counts)
      # cumulative proportion
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase)] - class_props[as.numeric(clase)]/2
    },
    
    clase_tr_3 = {
      # counts per class (within this dataset; if this dataset is one year, it's per year)
      class_counts <- as.numeric(table(clase_3))
      class_props <- class_counts / sum(class_counts)
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase_3)] - class_props[as.numeric(clase_3)]/2
    },
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban")),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),

    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("998", "999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("998", "999"), NA, altura)),
    imc = na_if(imc, 9),  # Treat 9 ("Not reported") as missing
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),

    # Physical Activity
    sedentarismo = na_if(as.numeric(sedentarismo), 9),
    PA = factor(sedentarismo, levels = 1:4, labels = PA_labels), 
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      (sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4) ~ "No", # not sedentary
      sedentarismo == 9 ~ NA_character_
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    
    n_orden_menor = as.numeric(n_orden_menor),
    row_id = row_number()
  )

## ENSE 2017 ####
ense2017_rename <- ense2017 %>% 
  mutate( 
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 2), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = as.numeric(ccaa),
    ccaa = ifelse(ccaa == 19, 18, ccaa),  # Merge Melilla and Ceuta
    ccaa = factor(ccaa, levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 ~ "Foreign",
      nacionalidad == 2 ~ "Spanish"),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr=cume_dist(as.numeric(clase)),
    
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III")),
    clase_tr_4 = cume_dist(as.numeric(clase_3)),
    
    clase_tr_2 = {
      # counts per class
      class_counts <- as.numeric(table(clase))
      # fraction of total population
      class_props <- class_counts / sum(class_counts)
      # cumulative proportion
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase)] - class_props[as.numeric(clase)]/2
    },
    
    clase_tr_3 = {
      # counts per class (within this dataset; if this dataset is one year, it's per year)
      class_counts <- as.numeric(table(clase_3))
      class_props <- class_counts / sum(class_counts)
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase_3)] - class_props[as.numeric(clase_3)]/2
    },
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban")),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),
    
    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("998", "999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("998", "999"), NA, altura)),
    # Combine underweight and normal weight into one category
    imc = as.numeric(imc),
    imc = na_if(imc, 9),
    imc = case_when(
      imc %in% c(1, 2) ~ 1,  # Combine Peso insuficiente and Normopeso
      imc == 3 ~ 2,          # Sobrepeso
      imc == 4 ~ 3           # Obesidad
    ),
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),
    
    # Physical Activity
    sedentarismo = case_when(
      sedentarismo == 8 | sedentarismo == 9 ~ NA_real_,
      TRUE ~ as.numeric(sedentarismo)
    ),
    PA = factor(sedentarismo, levels = 1:4, labels = PA_labels),
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4 ~ "No",  # not sedentary
      sedentarismo == 8 | sedentarismo == 9 ~ NA_character_
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    
    n_orden_menor = as.numeric(n_orden_menor),
    row_id = row_number()
  )

## ENSE 2023 ####
ense2023_rename <- ense2023 %>% 
  mutate( 
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 2), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = as.numeric(ccaa),
    ccaa = ifelse(ccaa == 19, 18, ccaa),  # Merge Melilla and Ceuta
    ccaa = factor(ccaa, levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 ~ "Foreign",
      nacionalidad == 2 ~ "Spanish"),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = case_when(
      clase == 8 ~ NA_real_,
      clase == 9 ~ NA_real_,
      TRUE ~ as.numeric(clase)
    ),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr = cume_dist(as.numeric(clase)),
    
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III")),
    clase_tr_4 = cume_dist(as.numeric(clase_3)),
    
    clase_tr_2 = {
      # counts per class
      class_counts <- as.numeric(table(clase)) # remove class names keep values
      # fraction of total population
      class_props <- class_counts / sum(class_counts) # what fraction of the population is in each class, therefore can know % of children per class
      # cumulative proportion
      cum_props <- cumsum(class_props) # cumulative proportions, from Class I to Class VI, "covering X % of children per class"
      # midpoint per class
      cum_props[as.numeric(clase)] - class_props[as.numeric(clase)]/2 
      # as.numeric(clase) converts class labels into indices , so R knows which class segment each child belongs to
      # for example: class_props = c(0.21, 0.45, 0.34)
      #              cum_props = c(0.21, 0.66, 1.00)
      # want the midpoint of each segment: 
                     # Class I: 0.00 - 0.21
                     # Class II: 0.21 - 0.66
                     # Class III: 0.66 - 1.00
      #                Midpoint = (end of the segment - width)/2
      # class_props[as.numeric(clase)]/2 is half the width of the class segment
      # subtracting this value from the end givess the midpoint of the segment
    },
    
    clase_tr_3 = {
      class_counts <- as.numeric(table(clase_3))
      class_props <- class_counts / sum(class_counts)
      cum_props <- cumsum(class_props)
      # midpoint per class
      cum_props[as.numeric(clase_3)] - class_props[as.numeric(clase_3)]/2
    },
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban")),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),
    
    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("999"), NA, altura)),
    # Combine underweight and normal weight into one category
    imc = as.numeric(imc),
    imc = na_if(imc, 9),
    imc = case_when(
      imc %in% c(1, 2) ~ 1,  # Combine Peso insuficiente and Normopeso
      imc == 3 ~ 2,          # Sobrepeso
      imc == 4 ~ 3           # Obesidad
    ),
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),
    
    # Physical Activity
    sedentarismo = na_if(as.numeric(sedentarismo), 9),
    PA = factor(sedentarismo, levels = 1:4, labels = PA_labels), 
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4 ~ "No" # not sedentary
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    
    n_orden_menor = as.numeric(n_orden_menor),
    row_id = row_number()
  )

# Check missingness prior to joining ####
# Missing values table
## ense2003
na_table_2003 <- ense2003_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2003_rename)*100)
print(na_table_2003, n = 21)

## ense2006
na_table_2006 <- ense2006_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2006_rename)*100)
print(na_table_2006, n = 22)

## ense2011
na_table_2011 <- ense2011_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2011_rename)*100)
print(na_table_2011, n = 21)

## ense2017
na_table_2017 <- ense2017_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2017_rename)*100)
print(na_table_2017, n = 21)

## ense2023
na_table_2023 <- ense2023_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2023_rename)*100)
print(na_table_2023, n = 21)

# Join surveys ####
ense_list <- list(ense2003_rename, ense2006_rename, ense2011_rename, ense2017_rename, ense2023_rename)

# join all datasets together
joined <- bind_rows(ense_list) ## 0 to 15 without dropping NAs

# New Age Categorization
joined <- joined %>% 
  mutate(
    edad_cat = case_when(
      edad >= 6 & edad <= 11 ~ "6-11",
      edad >= 12 & edad <= 15 ~ "12-15"
    ),
    edad_cat3 = case_when(
      edad >= 6 & edad <= 9 ~ "6-9",
      edad >= 10 & edad <= 12 ~ "10-12",
      edad >= 13 & edad <= 15 ~ "13-15"
    ),
    edad_cat = factor(edad_cat, levels = c("6-11", "12-15")),
    edad_cat3 = factor(edad_cat3, levels = c("6-9", "10-12", "13-15"))
  )

joined <- joined %>%
  mutate(
    NUTS1 = case_when(
      # Noroeste
      ccaa %in% c("Galicia", "Asturias", "Cantabria") ~ "North-West",
      # Noreste
      ccaa %in% c("Basque Country", "Navarre", "La Rioja", "Aragon") ~ "North-East",
      # Madrid
      ccaa == "Madrid" ~ "Madrid",
      # Centre
      ccaa %in% c("Castile and Leon", "Castilla-La Mancha", "Extremadura") ~ "Centre",
      # East
      ccaa %in% c("Catalonia", "Valencian Community", "Balearic Islands") ~ "East",
      # South
      ccaa %in% c("Andalusia", "Murcia", "Ceuta and Melilla") ~ "South",
      # Canary Islands
      ccaa == "Canary Islands" ~ "Canary Islands",
      TRUE ~ NA_character_
    ),
    NUTS1 = factor(NUTS1)
  )
save(joined, file = "joined.RData")

joined_6 <- joined %>%  ## 6 to 15 without dropping NAs
  filter(edad >= 6)
save(joined_6, file = "joined_6.RData")

joined_clean_maihda <- joined_6 %>% 
  drop_na(edad, sexo, sedentarismo, clase, clase_tr_2, survey)

# New Social Class Categorization for MAIHDA (6 to 15)
joined_clean_maihda <- joined_clean_maihda %>% 
  mutate(
    clase_2 = case_when(
      clase %in% c("Class I", "Class II", "Class III") ~ "Non-Manual Workers",
      clase %in% c("Class IV", "Class V", "Class VI") ~ "Manual Workers",
      TRUE ~ NA_character_
    ),
    clase_2 = factor(clase_2, levels = c("Non-Manual Workers", "Manual Workers"))
  )

# New survey2 variable Pre-Post inflection
joined_clean_maihda <- joined_clean_maihda %>% 
  mutate(survey2 = case_when(
    survey %in% c("2003", "2006", "2011") ~ "Pre",
    survey %in% c("2017", "2023") ~ "Post",
    TRUE ~ NA_character_
  ),
  survey2 = factor(survey2, levels = c("Post", "Pre"))
  )

save(joined_clean_maihda, file = "joined_clean_maihda.RData") ## FINAL CLEAN DATABASE FOR MAIHDA
