# Libraries ####

library(sf)
library(tidyverse)

# Meta Data ####

# Spatial Data
# https://data.humdata.org/dataset/cod-ab-ven?

# Electoral Data
# https://resultadosconvzla.com/

# Upload Files ####

ven_02 <- read.csv("data_master/data_basic/RESULTADOS_2024_CSV_V2.csv", header = TRUE, encoding = "UTF-8")
ven_02_sf_estado <- sf::st_read("data_master/ven_shape/ven_admbnda_adm1_ine_20210223.shp")
ven_02_sf_municipio <- sf::st_read("data_master/ven_shape/ven_admbnda_adm2_ine_20210223.shp")
ven_02_sf_parroquia <- sf::st_read("data_master/ven_shape/ven_admbnda_adm3_ine_20210223.shp")
binder <- read.csv("data_master/data_basic/binder.csv", header = TRUE)
clave_candidato <- read.csv("data_master/data_basic/clave_candidato.csv", header = TRUE)

# Basic Join ####

ven_02_sf_estado <- left_join(ven_02_sf_estado, binder, by = c("ADM1_ES" = "name_in_sp"))

# 1. Data by State ####
all_state <-ven_02 %>% 
  group_by(EDO) %>% 
  summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)

# 2. State ####
this_state <- "EDO. TACHIRA"
each_state <- ven_02 %>% 
  filter(EDO == this_state) %>% 
  summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)

# 3. Data by Municipality in one State ####
this_state <- "EDO. ANZOATEGUI"
all_municipality <- ven_02 %>% 
  filter(EDO == this_state) %>% 
  group_by(MUN) %>% 
  summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)

# 4. Municipality ####
this_state <- "EDO. ZULIA"
this_municipality <- "MP. MARA"
each_municipality <- ven_02 %>% 
  filter(EDO == this_state & MUN == this_municipality) %>% 
  summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)

# 5. Parish ####
this_state <- "EDO. FALCON"
this_municipality <- "MP. ZAMORA"
this_parish <- "PQ. LA SOLEDAD"
each_parish <- ven_02 %>% 
  filter(EDO == this_state & MUN == this_municipality & PAR == this_parish) %>% 
  summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)

# Functions ####

# Percentage Function
percentage_function <- function(df){
  votos_validos <- df$VOTOS_VALIDOS
  df <- as.vector(df[2:ncol(df)])
  df <- as.data.frame(lapply(df, function(x) round(x/votos_validos*100,2)))
  df <- pivot_longer(df[,2:ncol(df)],   everything(), cols_vary = "slowest", names_to = "Candidato", values_to = "Porcentaje") %>% 
    left_join(., clave_candidato, by = c("Candidato" = "Clave")) %>% 
    select(Candidato.y, Porcentaje) %>% 
    rename(Candidato = Candidato.y)
  return(df)
}

the_graphic <- function(df){
  region_graph <- ggplot(df[df$Candidato == "Edmundo González Urrutia" | df$Candidato == "Nicolás Maduro Moros",], 
                         aes(x = reorder(Candidato, Porcentaje), y = Porcentaje, fill = Candidato, label = Porcentaje))+
    geom_col(width = 0.75)+
    geom_text(aes(y=15), fontface="bold", color="white", size = 16) +
    scale_fill_manual(values = c("Edmundo González Urrutia" = "blue", "Nicolás Maduro Moros" = "red"))+
    coord_flip()+
    theme(
      axis.title = element_blank(),
      axis.text= element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 12, color = "black", face = "bold"),
      plot.background = element_rect(fill="white"),
      panel.background = element_rect(fill="white"),
      panel.grid = element_line(color="white")
    )
  return(region_graph)
}



