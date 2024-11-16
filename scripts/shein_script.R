#Instalación paquetes faltantes
install.packages(c("readcsv","tidyr", "janitor", "naniar", "stringr", "lubridate"))


#Importamos librerías que usaremos
require(readr)
require(readcsv)
require(tidyr)
require(janitor)
require(naniar)
require(stringr)
require(lubridate)
require(dplyr)
require(tidyverse)

#Importamos bases de datos: Consideraremos para este proyecto 21 bases de datos de productos Shein
#(con más de 80.000 productos), están en formato CSV y estan delimitados por "," en una sola columna.

us_appliance <- read_csv("shein_databases/us-shein-appliances-3987.csv")
us_automotive <- read_csv("shein_databases/us-shein-automotive-4110.csv")
us_baby_and_maternity <- read_csv("shein_databases/us-shein-baby_and_maternity-4433.csv")


#Vemos que importando una por una nos demoramos mucho: Intentaremos otra cosa, para eso definimos:
carpeta <- "shein_databases"
archivos_csv <- list.files(path = carpeta, pattern = "*.csv", full.names =  TRUE)


# Cargar cada archivo en un dataframe con un nombre correspondiente
for (archivo in archivos_csv) {
  # Eliminamos la extensión .csv y mantenemos el nombre base del archivo para nombrar el dataframe
  nombre_archivo <- gsub(".csv$", "", basename(archivo))  
  #crea un dataframe con el nombre del archivo sin la extensión .csv y se le asigna el contenido de ese archivo CSV usando read_csv().
  assign(nombre_archivo, read_csv(archivo))
}

#Ahora cargamos todos las bases de datos por separado con su respectivo nombre y de manera masiva
#Haciendo una exploración rápida nos damos cuentas que gran parte de las bases de datos comparten
#variables, sin embargo, algunas tienen unas extras, etc. Además vemos que los nombres de los datos
#comienzan por us-shein y terminan con 4 dígitos. Eliminaremos esto para dejar los DF con el nombre
#directo de su categoría y además agregamos una variable llamada "categoria" que será respectivo
#a cada data frame para luego combinarlos en solo uno


for (archivo in archivos_csv) {
  # Obtener el nombre del archivo sin la extensión .csv
  nombre_archivo <- gsub(".csv$", "", basename(archivo))  # Eliminar la extensión .csv
  
  # Extraer la categoría usando expresión regular (todo entre 'us-shein-' y '-XXXX')
  categoria <- gsub("us-shein-(.*)-\\d{4}$", "\\1", nombre_archivo)  # Esto extrae y almacena la categoría
  
  # Leer el archivo CSV para cada unoy lo almacenamos en un objeto llamado df
  df <- read_csv(archivo)
  
  # Añadir la columna 'categoria'
  df$categoria <- categoria
  
  # Asignar el dataframe al entorno con el nombre de la categoría
  assign(categoria, df)  # Aquí se asigna el dataframe con el nombre de la categoría
  }

#Podemos ver que se cargaron correctamente los data frame con sus respectivos nombres de categoría
# y también a cada uno se le agregó la variable "Categoria" para que cuando los unamos
#podamos identificar a que categoría corresponden. El único problema acá fue que se creó un DF
#extra, ya que el ciclo for identificó la ultima base de dato como un data frame y la crea. 

#La eliminaremos para no tener confusiones

remove(df)


#Ahora tenemos nuestras 21 bases de datos con sus nombres limpios referentes a la categoría con una
#columna extra que hace referencia a esta.

names(appliances)
names(automotive)

glimpse(appliances)
glimpse(automotive)

#Combinamos las bases con bind_rows() y creamos una base completa que tenga toda la info
#Veremos un analisis general de sus variables, si existen duplicados o valores faltantes

base_completa <- bind_rows(appliances,automotive,baby_and_maternity,bags_and_luggage,
                           beauty_and_health,curve,electronics,home_and_kitchen,home_textile,
                           jewelry_and_accessories,kids,mens_clothes,office_and_school_supplies,
                           pet_supplies,shoes,sports_and_outdoors,swimwear,tools_and_home_improvement,
                           toys_and_games,underwear_and_sleepwear,womens_clothing)


view(base_completa)

names(base_completa)


#Primero vemos los nombres de nuestras variables y nos damos cuenta que algunos estan separados con 
#guion (-), otras con guion bajo (_), algunos tienen más de uno e incluso algunos no tienen 
#nada o espacios vación, haremos esta correción:

names(base_completa) <- gsub("-", "_", names(base_completa))
names(base_completa)


#Ahora eliminaremos los guin bajo(_) dobles:

names(base_completa) <- gsub("__", "_", names(base_completa))
names(base_completa)

#Ahora agregaremos guion bajo (_) a los espacios vacios:
names(base_completa) <- gsub(" ", "_", names(base_completa))
names(base_completa)

#Ahora vemos que los titulos estan limpios, o al menos se ven ordenados:
#Ahora analizaremos las clases de las variables y cambiaremos de ser necesario

glimpse(base_completa)

#Vemos que todas estan en formato character, a excepción de la variable color_count
#que es de tipo double

#Dejaremos como character (tal cual): goods_title_link_jump, goods_title_link_jump_href, rank_title,
#rank_sub , selling_proposition, goods_title_link, categoria, blackfridaybelts_bg_src,
#blackfridaybelts_content y product_locatelabels_img_src

#Cambiaremos la variable Price a numeric:

miss_var_summary(base_completa) #Vemos que hay sólo 2 missing para la variable price


#Le quitamos el signo $ a la variable price
base_completa$price <- gsub("\\$", "", base_completa$price)

#La cambiamos a tipo numeric:
#base_completa$price <- as.numeric(base_completa$price) 

#Cuando hacemos el cambio a numeric aparece un mensaje Warning que dice que se han introducido
#valores NA por coerción. Esto significa que hay datos de la variable price que estan causando
#problemas y debemos revisarlos antes de cambiar la variable a numeric 

valores_problema <- base_completa %>%
  filter(is.na(as.numeric(price))) %>%
  select(price)

#Esto nos devuelve un Tible de 25 observaciones que están causando problemas:
view(valores_problema)

#Vemos que las "," estan causando un posible problema
base_completa <- base_completa %>%
  mutate(price = gsub(",", "", price))

#Ahora convertimos la variable price a numeric
base_completa$price <- as.numeric(base_completa$price) 

#Vemos que se haya efectuado el cambio de tipo de variable y que los NA siguen siendo los mismos
#2 que eran en un comienzo
glimpse(base_completa)
miss_var_summary(base_completa) 

#Efectivamente cambiamos la variable a tipo numerico (en este caso dbl). Ahora cambiaremos
#variable Discount: Eliminamos el signo % y mantenemos signo negativo

base_completa$discount <- gsub("%", "", base_completa$discount)

#Cambiamos la variable a tipo numerico
base_completa$discount <- as.numeric(base_completa$discount)

glimpse(base_completa)


#Hemos dejado las variables con la clase correspondiente. 
#Ahora veremos un resumen de los datos faltante en la BD

miss_var_summary(base_completa)
skimr::skim(base_completa)

#viendolo graficamente:
gg_miss_var(base_completa) +
  labs(title = "Valores Faltantes por Variable",
       x = "Variables",
       y = "Cantidad de Valores Faltantes") +
  theme_minimal()

#Acá podemos ver que existen variables con más del 90% de datos faltantes. Dependiendo
#del objetivo del proyecto podemos tomar diferentes decisiones, en algunos casos, cuando los 
#datos faltantes son superiores al 60% podemos decidir descartar la variable ya que hay
#mucha información perdida.

#En este caso eliminaremos la variable goods_title_link_jump_href la cual hace referencia a la
#URL del producto y tenemos un 99,2% de valores faltantes. De igual forma eliminaremos la variable
#goods_title_link_jump que hace referencia al nombre del producto o descripción pero antes 
#asignaré los valores de estos a los datos faltantes de la variable goods_title_link que son 678
#(en caso que existan)

#Vemos si es que existen datos de goods_title_link_jump que puedan ser usados en los 
#datos faltantes de la variable goods_title_link
missing_goods_title_link <- base_completa %>%
  filter(is.na(goods_title_link)) %>%
  select(goods_title_link, goods_title_link_jump)


#Si existen, por lo que rellenamos con los valores:
base_completa <- base_completa %>%
  mutate(goods_title_link = ifelse(
    is.na(goods_title_link),                  # Acción logica (Si es NA)
    goods_title_link_jump,                    # yes = Reemplazar con el valor de goods_title_link_jump
    goods_title_link                          # No = Si no es NA, dejar el valor original
  ))

#Vemos nuevamente los NA
miss_var_summary(base_completa)

#Pudimos reducir los valores faltantes de goods_title_link de 673 a 14.
#Ahora eliminamos las variables con sobre 90%

base_completa <- base_completa %>% 
  select(-goods_title_link_jump, -goods_title_link_jump_href,
                         -product_locatelabels_img_src, -blackfridaybelts_bg_src,
                         -blackfridaybelts_content)

#Ahora nos quedamos con 8 variables:
view(base_completa)

#Vemos nuevamente el resumen de datos faltantes
miss_var_summary(base_completa)

#De las 8 variables que tenemos, vemos que las variables rank_title y rank_sub tienen un 82,2%
# de datos faltantes. Dependiendo de la importancia de la variable decidimos que hacer. 

#Primero vemos que nos dice la variable: rank title o que valores almacena
head(base_completa$rank_title, n = 20)
tail(base_completa$rank_title, n = 20)

table(base_completa$rank_title, useNA = "ifany")
unique(base_completa$rank_title)

#Acá podemos ver que nos dice si el producto se encuentra dentro de los 10 más vendidos (Best sellers)
#sin embargo, vemos que hay valores (informacion) que se repiten o son redundate porque 
#estan mal escritos.Por ejemplo, existe "#9 Best Seller" y también "#9 Best Sellers" con una "s" 
# al final.Dejaremos la información repetida como un único valor y asignaremos los NA como "Unknown"

base_completa <- base_completa %>%
  mutate(rank_title = recode(rank_title,
                             "#1 Best Seller" = "#1 Best Sellers",
                             "#2 Best Seller" = "#2 Best Sellers",
                             "#3 Best Seller" = "#3 Best Sellers",
                             "#4 Best Seller" = "#4 Best Sellers",
                             "#5 Best Seller" = "#5 Best Sellers",
                             "#6 Best Seller" = "#6 Best Sellers",
                             "#7 Best Seller" = "#7 Best Sellers",
                             "#8 Best Seller" = "#8 Best Sellers",
                             "#9 Best Seller" = "#9 Best Sellers",
                             "#10 Best Seller" = "#10 Best Sellers",
                             .missing = "Unknown"))


#Revisamos que los cambios se hayan efectuados:
table(base_completa$rank_title, useNA = "ifany")
View(base_completa)

#Ahora analizamos la variable "rank_sub":

table(base_completa$rank_sub)
unique(base_completa$rank_sub)

#Vemos que relación existe entre la variable y categoría:
categoria_rank_sub <- base_completa %>% 
  group_by(categoria) %>% 
  select(rank_sub)

view(categoria_rank_sub)

#Podemos deducir que esta variable hace referencia a una "subcategoria" o algo más específico
#Esta variable tiene demasiados valores diferentes, por lo que no nos entrega mucha información.
#En este caso eliminaremos la variable y nos quedaremos con categoría ya que podemos agrupar de una
#forma más ordenada:

base_completa <- base_completa %>% 
  select(-rank_sub)

#Nos quedamos con 7 variables.

#Ahora vemos la variable color_count: Recordemos que esta variable tenia el 76% con valores perdidos
#Nuevamente queda a criterio si eliminamos o no esta variable. Para este caso la dejaremos y aquellos
#valores con NA los dejaremos con "0" asumiendo que el producto no tiene colores disponibles

summary(base_completa$color_count)

#Vemos los colores filtrando por valores distintos de NA (para ver que valores tenemos)
color <- base_completa %>%
  filter(!is.na(color_count)) %>%
  select(color_count)

table(color)

#Reemplazamos nuestros valores NA, por valores de 0
base_completa_color <- base_completa %>% 
  mutate(color_count = replace_na(color_count, 0))

#Comparamos los valores faltantes de nuestra base real con los NA editados. Tendrían que ser los mismos
#para confirmar que se reemplazaron bien.
miss_var_summary(base_completa)   #Tenemos 62.362 valores faltantes

#Contamos los reemplazos realizados en nuestra base "guia" para ver que sean iguales. Son 62.362
base_completa_color %>% 
  filter(color_count == 0) %>% 
  count()

#Sobreescribimos en nuestra base_completa
base_completa <-  base_completa %>% 
  mutate(color_count = replace_na(color_count, 0))

view(base_completa)
miss_var_summary(base_completa)


#Ahora vemos nuestra variable selling_proposition que tiene 27.741 datos faltantes (33.8%):
table(base_completa$selling_proposition)

#Vemos que esta variable sería como un "slogan" de venta para cada producto. Dependiendo
#lo que buscamos decidimos si dejar o no esta variable. En este caso reemplazaremos los 
#valores faltantes con un texto "No proposition"

base_completa <- base_completa %>% 
  mutate(selling_proposition = replace_na(selling_proposition, "No proposition"))

base_completa %>% 
  filter(selling_proposition == "No proposition") %>% 
  count()

#Se reemplazaron los 27.741 datos correctos
table(base_completa$selling_proposition)
miss_var_summary(base_completa)

#Ahora veremos la variable "discount". Asumiremos que los valores NA, no tienen descuentos, por
#lo que asignaremos un numero 0 haciendo referencia a que no existe descuento

#Revisamos que los valores se encuentren en un rango entre -1 a -100 (1 a 100% descuento)
summary(base_completa$discount)
unique(base_completa$discount)

discount_frecuency <- base_completa %>% 
  filter(!is.na(discount)) %>% 
  count(discount)

view(discount_frecuency)

#Reemplazamos los valores NA, por 0 ya que no tienen descuento.

base_completa <- base_completa %>% 
  mutate(discount = replace_na(discount, 0))

#Vemos nuestras variables nuevamente: Solo la variable price y goods_title_link tienen valores
#perdidos (25 y 14 respectivamente, los cuales representan 0.03% y 0.01%)
miss_var_summary(base_completa)


#Analizaremos ahora la variable de los productos:
#Vemos los nombres de productos (goods_title_link) que tienen 2 o más valores. O sea que están
# repetidos, para solo quedarnos con uno de cada uno.
productos_repetidos <- base_completa %>%
  count(goods_title_link) %>% 
  filter(n > 1)

#Tenemos 8.395 productos que al estan repetidos 2 o más veces.


#Vemos los productos que tienen más de 1 precio los cuales son 1545
productos_repetidos_con_precio_variable <- base_completa %>%
  group_by(goods_title_link) %>%
  summarise(n_precio_distinto = n_distinct(price)) %>%
  filter(n_precio_distinto > 1)

8395-1545

#O sea tengo 6850 productos que están repetidos pero solo tienen un mismo precio.

productos_repetidos_con_precio_fijo <- base_completa %>%
  group_by(goods_title_link) %>%
  filter(n_distinct(price) == 1) %>%
  count(goods_title_link) %>%
  filter(n > 1)


#Entonces, sabiendo esto, eliminaremos todos los duplicados de los productos que tienen el mismo 
#precio (6850 observaciones o productos)

base_completa_sin_duplicados_precio_fijo <- base_completa %>%
  filter(goods_title_link %in% productos_repetidos_con_precio_fijo$goods_title_link) %>%
  distinct(goods_title_link, .keep_all = TRUE)

#Corroboramos que se hayan eliminado todos los duplicados: 6850
base_completa_sin_duplicados_precio_fijo %>%
  count(goods_title_link) %>%
  filter(n > 1)

#Ahora debemos analizar los productos_repetidos_con_precio_variable o en otras palabras
#los prodcutos repetidos con diferentes precios que en total son 1545.
#Podemos ver que los productos tienen entre 2 a 11 diferentes precios

max(productos_repetidos_con_precio_variable$n_precio_distinto)
min(productos_repetidos_con_precio_variable$n_precio_distinto)


#Vemos el detalle de los productos repetidos con precios diferentes
productos_con_precio_variable_detalle <- base_completa %>%
  filter(goods_title_link %in% productos_repetidos_con_precio_variable$goods_title_link) %>%
  select(goods_title_link, price, categoria) %>%  
  arrange(goods_title_link, price) 


#Decidimos quedarnos con los precios más altos de cada producto
base_completa_sin_duplicados_precio_variable_maximo <- base_completa %>%
  filter(goods_title_link %in% productos_repetidos_con_precio_variable$goods_title_link) %>%
  arrange(goods_title_link, desc(price)) %>%
  distinct(goods_title_link, .keep_all = TRUE)


#Una vez vemos que se realizan las eliminaciones correctamente, aplicaremos esto en 
#nuestra base_completa o la base original que veniamos trabajando, que tiene toda la data.


# Primero, eliminamos los productos con precios fijos repetidos y los reemplazamos con los correctos
base_completa_actualizada <- base_completa %>%
  filter(!(goods_title_link %in% productos_repetidos_con_precio_fijo$goods_title_link)) %>%
  bind_rows(base_completa_sin_duplicados_precio_fijo)


# Luego, actualizamos los productos con precios variables, dejando solo el precio máximo
base_completa_final <- base_completa_actualizada %>%
  filter(!(goods_title_link %in% productos_repetidos_con_precio_variable$goods_title_link)) %>%
  bind_rows(base_completa_sin_duplicados_precio_variable_maximo)

base_completa_final %>% 
  group_by(goods_title_link) %>% 
  count() %>% 
  filter(n == 1)


#Ahora que tenemos todos los productos unicos. Vemos nuevamente: 

miss_var_summary(base_completa_final)

#Solo tenemos 1 producto sin nombre y los 2 que teníamos sin precio anteriormente se eliminaron 
#con la condición anterior ya que correspondian a productos repetidos, los cuales adquirieron
#el precio máximo.

base_completa_final %>% 
  filter(is.na(goods_title_link))


#Vemos que ahora solo tenemos 1 dato faltante para la variable goods_title_link o producto
#por lo que la eliminaremos directamente. Actualmente tenemos 71.481 observaciones

base_completa_final <- na.omit(base_completa_final)

#Eliminamos el dato faltante y ahora tenemos 71.480 observaciones
#Ahora podemos ver que nuestra base no tiene datos faltantes, hemos eliminado los duplicados,
#arreglado las clases de sus variables, agregado y quitado variable según relevancia, etc.

miss_var_summary(base_completa_final)


#El siguiente paso muchas veces se recomienda hacerlo al principio, pero en este caso lo haré
#ahora: Reordenaermos el orden de las columnas y asignaremos otros nombres a sus variables
#para que al momento de descargar el excel sea más entendible para otras personas

base_completa_final <- base_completa_final %>% 
  rename(ranking = rank_title,
         precio = price,
         descuento = discount,
         slogan_venta = selling_proposition,
         nombre_producto = goods_title_link,
         cantidad_colores = color_count)

names(base_completa_final)

#Reordenamos:
base_completa_final <- base_completa_final %>% 
  select(nombre_producto,categoria,precio,descuento,cantidad_colores,ranking,slogan_venta)

View(base_completa_final)


#Ahora para terminar exportaremos los cambios que hemos hecho a un archivo .xlsx para
#poder manipularla fuersa del ambiente R o consola (directamente en excel)

install.packages("writexl")

require(writexl)

write_xlsx(base_completa_final, path = "C:/Users/rodol/Desktop/Proyectos/Data cleaning and transformation/Data_Cleaning_Project/base_completa_final.xlsx")
