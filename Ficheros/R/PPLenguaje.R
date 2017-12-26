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
vs_CR <- VectorSource(df$Description)
c_CR <- VCorpus(vs_CR, readerControl =  list( reader = reader(vs_CR),
                                           language = "es"))

#Se limpia el corpus de la descripcion
c_CR <- limpieza(c_CR)

n_c_CR <- tm_map(c_CR, PlainTextDocument)
dtm_CR <- TermDocumentMatrix(c_CR)
m_CR <- as.matrix(dtm_CR)
v_CR <- sort(rowSums(m_CR),decreasing=TRUE)
d_CR <- data.frame(word = names(v_CR),freq=v_CR)

#Nube de paabras de la descripcion
wordcloud(words = d_CR$word, freq = d_CR$freq, min.freq = 30,
          random.order = F, colors = brewer.pal(8, "Dark2"))

#Creacion del corpus del resumen
vs_R <- VectorSource(df$Resumen)
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