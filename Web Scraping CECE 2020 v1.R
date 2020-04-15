##########################################################
#                  WEB SCRAPING MEDIANTE R               #
#	                CONFERENCIAS CECE - UBA                #
#            FACULTAD DE CIENCIAS ECONÓMICAS             #
#              UNIVERSIDAD DE BUENOS AIRES               #
##########################################################

# Expositores: Martin Masci - Rodrigo Del Rosso
# Moderador  : Diego Parras
# Jueves 16 de Abril de 2020

# Limpiar la consola
rm(list = ls())

# Instalar la librería
install.packages("rvest")
# de igual forma con el resto de los paquetes

# Invocar la librería
library(rvest)
library(robotstxt)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)

###############################
## CASO DE ESTUDIO AMAZON MX ##
###############################

# Asignar la url
url <- "https://www.amazon.com.mx/b/ref=s9_acsd_hfnv_hd_bw_bAcAgpX_ct_x_ct00_w?_encoding=UTF8&node=9725407011&pf_rd_m=AVDBXBAVVSXLQ&pf_rd_s=merchandised-search-4&pf_rd_r=Q47SQG6GBGGECT1SXNZY&pf_rd_t=101&pf_rd_p=8cc61471-874f-5f7f-96d7-e2b634466523&pf_rd_i=9725377011"

# Preguntar a robots.txt si esta permitido bajar esto devuelve TRUE/FALSE
paths_allowed(paths = c(url))

# Obtener el código html de la página web
pagina_web <- read_html(url)

# Asignar la clase
css_producto <- "a.a-link-normal.s-access-detail-page.s-color-twister-title-link.a-text-normal"

# Obtener el código html que contiene el nombre del producto
producto_html <- html_nodes(pagina_web,css_producto)
producto_texto <- html_text(producto_html)

# Exhibir los datos
producto_texto

length(producto_texto)
tail(producto_texto)

# Clase CSS del producto
css_precio <- "span.a-size-base.a-color-price.s-price.a-text-bold"

# Obtener el contenido de la clase en código html
precio_html <- html_nodes(pagina_web,css_precio)

# Limpiar el código para obtener el texto
precio_texto <- html_text(precio_html)

# Eliminar el signo de peso
precio_limpio <- gsub("\\$","",precio_texto)

# Eliminar la coma
precio_limpio <- gsub(",","",precio_limpio)

# Consultar tipo de dato
data.class(precio_limpio)  ## character

length(precio_limpio)
tail(precio_limpio)

# Transformamos a numérico 
precio_numerico <- as.numeric(precio_limpio)

# Unimos los datos
productos <- data.frame(Producto = producto_texto[-1], 
                        Precio = precio_numerico)

# Para mostrar la gráfica por precio
barplot(precio_numerico)

gc()

##########################
## CASO DE ESTUDIO IMDB ##
##########################

# Le pregunta a robots.txt si esta permitido bajar esto devuelve TRUE/FALSE

url = "https://www.imdb.com/search/title?groups=top_250&sort=user_rating"
paths_allowed(paths = c(url))

# Leer el HTML
imdb <- read_html(url)

# formato html, aca ya esta toda la info

imdb

#Tomemos el html y busquemos la categoría títulos (ver filmina)
#imdb es una función, le estoy pasando argumentos a traves del operador %>%
# le paso el nodo donde esta la info que quiero y le pido el texto de ahí

imdb %>%
  html_nodes(".lister-item-content h3 a") %>%
  html_text() -> movie_title

#ver los títulos
movie_title


#ahora quiero el año, como viene en texto lo quiero reformatear as.Date

imdb %>%
  html_nodes(".lister-item-content h3 .lister-item-year") %>%
  html_text() %>%
  str_sub(start = 2, end = 5) %>%
  as.Date(format = "%Y") %>%
  year() -> movie_year

movie_year

#Ahora quiero la duración, reformateo a número 

imdb %>%
  html_nodes(".lister-item-content p .runtime") %>%
  html_text() %>%
  str_split(" ") %>%
  map_chr(1) %>%
  as.numeric() -> movie_runtime

movie_runtime

#genero

imdb %>%
  html_nodes(".lister-item-content p .genre") %>%
  html_text() %>%
  str_trim() -> movie_genre

movie_genre

#rating

imdb %>%
  html_nodes(".ratings-bar .ratings-imdb-rating") %>%
  html_attr("data-value") %>% 
  as.numeric() -> movie_rating

movie_rating

#ahora cambia un poco porque estoy pidiendo un atributo, cuanta gente voto por los ratings

imdb %>%
  html_nodes(xpath = '//meta[@itemprop="ratingCount"]') %>% 
  html_attr('content') %>% 
  as.numeric() -> movie_votes

movie_votes


#otro atributo, recaudación, pero reformateado

imdb %>%
  html_nodes(xpath = '//span[@name="nv"]') %>%
  html_text() %>%
  str_extract(pattern = "^\\$.*") %>%
  na.omit() %>%
  as.character() %>%
  append(values = NA, after = 30) %>%
  append(values = NA, after = 46) %>%
  str_sub(start = 2, end = nchar(.) - 1) %>%
  as.numeric() -> movie_revenue

movie_revenue

# junto todo
top_50 <- tibble(title = movie_title, release = movie_year, 
                 `runtime (mins)` = movie_runtime, genre = movie_genre, rating = movie_rating, 
                 votes = movie_votes, `revenue ($ millions)` = movie_revenue)

top_50

#Exportar a excel
write.csv2(top_50,"top50.csv")

gc()

#########################
## CASO DE ESTUDIO BNA ##
#########################


url = "htpps://www.bna.com.ar/Personas"
paths_allowed(paths = c(url))

bna <- read_html(url)

gc()
