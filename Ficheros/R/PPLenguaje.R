#Librerias necesarias para el tratamiento del corpus
library("NLP");library("tm");
library("wordcloud");library("RColorBrewer")

limpieza <- function(bolsa){
  #Se transforma la letra a minuscula
  bolsa = tm_map(bolsa,content_transformer(base::tolower))
  #Se borran las stopword, los simbolos de puntuacion y los numeros
  bolsa = tm_map(bolsa, removeWords, stopwords("spanish"))
  bolsa = tm_map(bolsa, removePunctuation)
  bolsa = tm_map(bolsa, removeNumbers)
  #Se quitan los espacios
  bolsa = tm_map(bolsa,tm::stripWhitespace)
  return(bolsa)
}

#Creacion del corpus de la descripcion
vs_D <- VectorSource(df$Description)
c_D <- VCorpus(vs_D, readerControl =  list( reader = reader(vs_D),
                                           language = "es"))

#Se limpia el corpus de la descripcion
c_D <- limpieza(c_D)

n_c_D <- tm_map(c_D, PlainTextDocument)
dtm_D <- TermDocumentMatrix(c_D)
m_D <- as.matrix(dtm_D)
v_D <- sort(rowSums(m_D),decreasing=TRUE)
d_D <- data.frame(word = names(v_D),freq=v_D)

#Nube de paabras de la descripcion
wordcloud(words = d_D$word, freq = d_D$freq, min.freq = 30,
          random.order = F, colors = brewer.pal(8, "Dark2"))

#Creación de la lista de frecuencias
fr = 0
fr_D = NULL
for(i in 1:769){
  c = m_D[,i]
  c = as.numeric(c)
  fr = 0
  for(j in 1:2770){
    fr= c[j]+fr
  }
  fr_D = cbind(fr_D,fr)
}
rm(i);rm(j)
fr_D = as.numeric(fr_D[1,])

#Creacion del corpus del resumen
vs_R <- VectorSource(df$Summary)
c_R <- VCorpus(vs_R, readerControl =  list( reader = reader(vs_R),
                                         language = "es"))
#Se limpia el corpus del resumen
c_R <- limpieza(c_R)

n_c_R <- tm_map(c_R, PlainTextDocument)
dtm_R <- TermDocumentMatrix(c_R)
m_R <- as.matrix(dtm_R)
v_R <- sort(rowSums(m_R),decreasing=TRUE)
d_R <- data.frame(word = names(v_R),freq=v_R)

#Nube de palabras del resumen
wordcloud(words = d_R$word, freq = d_R$freq, min.freq = 30,
          random.order = F, colors = brewer.pal(8, "Dark2"))
#Creación de la lista de frecuencias
fr = 0
fr_R = NULL
for(i in 1:769){
  c = m_R[,i]
  c = as.numeric(c)
  fr = 0
  for(j in 1:994){
    fr= c[j]+fr
  }
  fr_R = cbind(fr_R,fr)
}
fr_R = as.numeric(fr_R[1,])
rm(i);rm(j)