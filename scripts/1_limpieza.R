
### Price tracker ### ====

# Librerías ====
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(rvest)
library(htmlunit)
library(lubridate)

# Candado para crear csv inicial
if(file.exists("./data/2_output/track_data.csv") == F){
  track_data <- data.frame(producto = character(),
                           fecha = col_date(format = "%F"),
                           precio = numeric())
  
  write_csv(track_data, "./data/2_output/track_data.csv")
}

# Cargar datos de amazon ====
products_url <- read.csv("./data/0_raw/products_url.csv", header = T)
url <- products_url$url[1]
product <- read_html(url)


# Limpieza - Precio ====
price <- product %>%
  html_nodes(xpath = '//*[(@id = "priceblock_ourprice")]') %>%
  html_text() %>%
  str_extract(., "([0-9]),(\\d)+") %>%
  str_remove(., ",") %>%
  as.numeric()

# "Base de datos" ====

# Nuevo precio
new_price <- data.frame(producto = products_url$producto, 
                        fecha = Sys.Date(),
                        precio = price)

# Añadir nuevo precio al csv "track_data"
track_data <- read_csv("./data/2_output/track_data.csv", col_names = T)
track_data <- rbind(track_data, new_price)

# Si se duplica un valor: se elimina
if(nrow(track_data) > 1){
  if(track_data$fecha[nrow(track_data)] == track_data$fecha[nrow(track_data) - 1]){
    track_data <- head(track_data, -1)
  } 
} else {
  next}

# Se escribe el csv para ser guardado con el precio nuevo
write_csv(track_data, "./data/2_output/track_data.csv")

# Visualización ====
track_data %>%
  ggplot(aes(x = as_date(fecha), y = precio)) +
  geom_point(color = ifelse(track_data$precio < max(track_data$precio), "#FD9A01", "#232F3F")) +
  geom_text(aes(label = track_data$precio), vjust = -1, size = 3) +
  geom_line() +
  scale_y_continuous(limits = c(1200, 1500)) +
  labs(x = "Fecha",
       y = "Precio",
       title = "Adata Swordfish - Price Track") + 
  ggthemes::theme_clean() + 
  theme(legend.position = "none")


