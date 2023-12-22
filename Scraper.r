library(rvest)
library(wordcloud)
library(RColorBrewer)
library(stringr)

#Conectarse al sitio web
url <- "https://cienciasdelsur.com/"
html <- read_html(url) 

class_elements <- html %>% html_nodes(".td-module-thumb") #busco el contenido de esta clase en especifica

a_elements <- class_elements %>% html_nodes("a") #busco todos los enlaces de la clase anterior

enlaces <- a_elements %>% html_attr("href") #guardo en una lista todos los enlaces

texto <- "" #creo una variable vacia para concatenar los articulos
i <- 5 #inicio un contador en 5 para iterar los enlaces
writeLines(texto, "EXTRACCION_TEXTOS.txt") #creo el archivo donde se guardaran los articulos

while (i <= length(enlaces)) { #con este ciclo recorro y scrapeo todos los enlaces
  url <- enlaces[[i]] #obtengo cada enlace
  html <- read_html(url) #obtengo el contenido del html
  titulo <- html %>% #se extrae el titulo del articulo
  html_nodes("h1.entry-title") %>%
  html_text(trim = TRUE) #elimina espacios al inicio y al final
  articulo <- html %>% #se extrae el articulo de cada enlace
  html_nodes("div.td-post-content.tagdiv-type") %>%
  html_text(trim = TRUE) #elimina espacios al inicio y al final
  texto <- paste(titulo,articulo, sep = "\n") #concateno el titulo con su articulo
  file_conn <- file("EXTRACCION_TEXTOS.txt", "a") #abro el archivo y escribo en el los articulos
  writeLines(texto, file_conn, sep = "\n", useBytes = TRUE)
  close(file_conn)
  texto <- "" #reinicio la variable para el siguente enlace
  i <- i + 1 #paso al siguente enlace
}

text <- readLines("EXTRACCION_TEXTOS.txt") # leo el archivo donde se guardaron los articulos

text <- tolower(paste(text, collapse = " ")) # uno todas las líneas en una única cadena y  las convertierto a minúsculas

palabras <- unlist(strsplit(text, "\\W+")) # separo las cadenas mediante a la expresion regular y les guardo en un vector

palabras <- na.omit(palabras) # elimo las palabras vacías

# excluyo las palabras no significativas
excluir <- c("yo", "tú", "él", "ella", "nosotros", "vosotros", "ellos", "ellas","de","la",
"en","y","el","que","a","del","las","los","se","una","para","es","por","un","con","no","como",
"más","al","su","o","este","author","lo","com","https","son","también","qué","cienciasdelsur",
"sobre","sus","e","m","box","ejemplo","desde","esta","entre","puede","fue","sí","sin","ser","myradio","tab",
"además","está","esto","uno","alejandra","pero","foto","otros","5","active","debe","id",
"forma","hay","parte","tiene","1","ha","nos","través","muy","gran","donde","hasta","falta","todo",
"the","sosa","cómo","así","ya","te","si","document","cuando","compartir","classlist","acceso","tienen",
"estas","cada","actualmente","queryselector","mabid","label","estos","otra","benítez","4","vez",
"dos","ni")

palabras <- palabras[!(palabras %in% excluir)]
frecuencias <- table(palabras) # cuento la frecuencia de cada palabra
palabras_ordenadas <- sort(frecuencias, decreasing = TRUE) # ordeno las palabras por frecuencia descendente
palabras_top <- head(names(palabras_ordenadas), 50) # seleccionar las primeras 50 palabras
palabras_frecuencias <- as.data.frame(cbind(palabras_top, palabras_ordenadas[palabras_top])) # creo una tabla con las palabras y sus frecuencias

# escribo las palabras y sus frecuencias en un archivo
write.table(palabras_frecuencias, file = "FRECUENCIA_PALABRAS.txt", 
            col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)

# imprimo las palabras y sus frecuencias
for (i in 1:length(palabras_top)) {
  cat(paste(palabras_top[i], ":", palabras_ordenadas[palabras_top[i]], "\n"))
}

# creo la nube de palabras
wordcloud(words = palabras_top, freq = palabras_ordenadas[palabras_top], 
          min.freq = 1, max.words = 50, random.order = FALSE, 
          rot.per = 0.20, colors = brewer.pal(8, "Dark2"),scale = c(9, 2))       


#Item 2

#Conectarse al sitio web
url <- "https://cienciasdelsur.com/"
html <- read_html(url)

class_elements <- html %>% html_nodes(".td-module-thumb") #busco el contenido de esta clase en especifica

a_elements <- class_elements %>% html_nodes("a") #busco todos los enlaces de la clase anterior

links <- a_elements %>% html_attr("href")

palabra_compuesta <- readline(prompt = "Ingrese su palabra compuesta: ")
apariciones <- 0 # variable para almacenar todas las apariciones de la palabra compuesta
i <- 5 #inicio un contador en 5 para iterar los enlaces
#el ciclo es practicamente igual al primer ciclo donde extraemos los articulos
while (i <= length(links)) {
    url <- links[[i]]
    html <- read_html(url)
    text <- html_text(html)
    if (str_detect(text, palabra_compuesta)) {  # Busco la palabra compuesta en el texto de la página
        apariciones <- apariciones + 1 #si se encuentra aumenta el contador
    }
    i <- i + 1
}

# mostrar todas las apariciones encontradas de la palabra compuesta
if (apariciones > 0) {
  cat(paste0('Se encontraron ', apariciones, ' apariciones de la palabra compuesta: ', palabra_compuesta, "\n"))
} else {
  cat('No se encontraron apariciones de la palabra compuesta.', "\n")
}