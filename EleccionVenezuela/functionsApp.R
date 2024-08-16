# Functions ####

# Percentage Function ####
percentage_function <- function(df){
  clave_candidato <- read.csv("clave_candidato.csv", header = TRUE)
  votos_validos <- df$VOTOS_VALIDOS
  df <- as.vector(df[2:ncol(df)])
  df <- as.data.frame(lapply(df, function(x) round(x/votos_validos*100,2)))
  df <- pivot_longer(df[,2:ncol(df)],   everything(), cols_vary = "slowest",, names_to = "Candidato", values_to = "Porcentaje") %>% 
    left_join(., clave_candidato, by = c("Candidato" = "Clave")) %>% 
    select(Candidato.y, Porcentaje) %>% 
    rename(Candidato = Candidato.y)
  return(df)
}

# Bar Graphic Function ####
the_graphic <- function(df, entidad){
  region_graph <- ggplot(df[df$Candidato == "Edmundo González Urrutia" | df$Candidato == "Nicolás Maduro Moros",], 
                         aes(x = reorder(Candidato, Porcentaje), y = Porcentaje, fill = Candidato, label = Porcentaje))+
    geom_col(width = 0.75)+
    geom_text(aes(y=5), fontface="bold", color="white", size = 12) +
    labs(title = entidad)+
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
      panel.grid = element_line(color="white"),
      title = element_text(size = 32)
    )
  return(region_graph)
}


# Unique Names Function ####
names_unique <- function(entidad){
  these_names <- unique(entidad)
  return(these_names)
}

