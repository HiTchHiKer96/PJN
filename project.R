# ładowaie bibliotek
library(tm)
library(hunspell)
library(lsa)
library(topicmodels)
library(wordcloud)
library(dendextend)
library(corrplot)
library(flexclust)
library(proxy)

# zmiana katalogu roboczego
work_dir <- "G:\\R things\\PJN"
setwd(work_dir)

# definicja lokalizacji katalogów funkcyjnych
input_dir <- ".\\data"
scripts_dir <- ".\\scripts"
output_dir <- ".\\results"
workspaces_dir <- ".\\workspaces"

# utworzenie katalogów wyjściowych
dir.create(output_dir, showWarnings = F)
dir.create(workspaces_dir, showWarnings = F)

# zdefiniowanie funkcji do tworzenia ścieżek dostępu do plików
create_path <- function(parent, child) 
  paste(parent, child, sep = "\\")

# zdefiniowanie funkcji do tworzenia nazw plików
create_filename <- function(name, extension) 
  paste(name, extension, sep = ".")

# utworzenie korpusu dokumentów
corpus_dir <- create_path(
  input_dir,
  "Literatura - streszczenia - oryginal"
)
corpus <- VCorpus(
  DirSource(
    corpus_dir,
    "UTF-8",
    #"*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

# wstępne przetwarzanie dokumentów
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
stoplist_file <- create_path(
  input_dir,
  "stopwords_pl.txt"
)
stoplist <- readLines(stoplist_file, encoding = "UTF-8")

corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

# zdefiniowanie funkcji do usuwania pojedynczych znaków
remove_char <- content_transformer(
  function(text, char) gsub(char, "", text)
)
corpus <- tm_map(corpus, remove_char, intToUtf8(8722))
corpus <- tm_map(corpus, remove_char, intToUtf8(190))
corpus <- tm_map(corpus, remove_char, "”")
corpus <- tm_map(corpus, remove_char, "„")



# zdefiniowanie funkcji do usuwania rozszerzeń z nazw plików
cut_extensions <- function(document, ext){
  meta(document, "id") <- gsub(
    paste("\\.", ext, "$", sep = ""),
    "",
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cut_extensions, "txt")

# zdefiniowanie funkcji usuwającej podział na akapity w tekście
paste_paragraphs <- function(text){
  paste(text, collapse = " ")
}

corpus <- tm_map(corpus, content_transformer(paste_paragraphs))
corpus <- tm_map(corpus, stripWhitespace)

# zdefiniowanie funkcji do lematyzacji
polish <- dictionary("pl_PL")
lemmatize <- function(text){
  parsed_text <- hunspell_parse(text, dict = polish)
  lemmatized_text <- hunspell_stem(parsed_text[[1]], dict = polish)
  for (i in 1:length(lemmatized_text)) {
    if (length(lemmatized_text[[i]]) == 0) 
      lemmatized_text[i] <- parsed_text[[1]][i]
    if (length(lemmatized_text[[i]]) >  1) 
      lemmatized_text[i] <- lemmatized_text[[i]][1]
  }
  new_text <- paste(lemmatized_text, collapse = " ")
  return(new_text)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

# zapisanie wstępnie przetworzonego korpusu do plików
preprocessed_dir <- create_path(
  input_dir,
  "Literatura - streszczenia - przetworzone"
)
dir.create(preprocessed_dir, showWarnings = F)
writeCorpus(corpus, preprocessed_dir)


##################################################################################
# frequency_matrix.R


# zdefiniowanie funkcji do usuwania rozszerzeń z nazw plików
cut_extensions <- function(document, ext){
  meta(document, "id") <- gsub(
    paste("\\.", ext, "$", sep = ""),
    "",
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cut_extensions, "txt")

# utworzenie macierzy częstości
tdm_tf_all <- TermDocumentMatrix(corpus)
tdm_bin_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdm_tfidf_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdm_tf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdm_tfidf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

dtm_tf_all <- DocumentTermMatrix(corpus)
dtm_bin_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
dtm_tfidf_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
dtm_tf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
dtm_tfidf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)


# added !
tdm_tf_3_15 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(3,15)
    )
  )
)
tdm_tfidf_3_15 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(3,15)
    )
  )
)
tdm_tf_5_10 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(5,10)
    )
  )
)
tdm_tfidf_5_10 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(5,10)
    )
  )
)



dtm_tf_3_15 <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(3,15)
    )
  )
)
dtm_tfidf_3_15 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(3,15)
    )
  )
)
dtm_tf_5_10 <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(5,10)
    )
  )
)
dtm_tfidf_5_10 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(5,10)
    )
  )
)


# konwersja macierzy rzadkich do postaci macierzy klasycznych
tdm_tf_all_m <- as.matrix(tdm_tf_all)
tdm_bin_all_m <- as.matrix(tdm_bin_all)
tdm_tfidf_all_m <- as.matrix(tdm_tfidf_all)
tdm_tf_2_16_m <- as.matrix(tdm_tf_2_16)
tdm_tfidf_2_16_m <- as.matrix(tdm_tfidf_2_16)
dtm_tf_all_m <- as.matrix(dtm_tf_all)
dtm_bin_all_m <- as.matrix(dtm_bin_all)
dtm_tfidf_all_m <- as.matrix(dtm_tfidf_all)
dtm_tf_2_16_m <- as.matrix(dtm_tf_2_16)
dtm_tfidf_2_16_m <- as.matrix(dtm_tfidf_2_16)

# added !
tdm_tf_3_15_m <- as.matrix(tdm_tf_3_15)
tdm_tfidf_3_15_m <- as.matrix(tdm_tfidf_3_15)
tdm_tf_5_10_m <- as.matrix(tdm_tf_5_10)
tdm_tfidf_5_10_m <- as.matrix(tdm_tfidf_5_10)

dtm_tf_3_15_m <- as.matrix(dtm_tf_3_15)
dtm_tfidf_3_15_m <- as.matrix(dtm_tfidf_3_15)
dtm_tf_5_10_m <- as.matrix(dtm_tf_5_10)
dtm_tfidf_5_10_m <- as.matrix(dtm_tfidf_5_10)





################################################################################## 
# pca.R




# added !
matrix_table <- list(dtm_tfidf_2_16, dtm_tfidf_3_15, dtm_tfidf_5_10)

for( mt in matrix_table){
  # analiza głównych składowych
  pca <- prcomp(mt)
  
  # przygotowanie danych do wykresu
  legend <- paste(
    paste(
      "d",
      1:length(rownames(mt)),
      sep = ""
    ),
    rownames(mt),
    sep = " - "
  )
  
  x <- pca$x[,1]
  y <- pca$x[,2]
  
  # rysowanie wykresu w przestrzeni 2-wymiarowej
  plot(
    x, 
    y, 
    main = "Analiza głównych składowych",
    xlab = "PC1",
    ylab = "PC2",
    # xlim = c(-0.03,0.03),
    # ylim = c(0.03,0.06),
    col = "darkmagenta"
  )
  text(
    x,
    y, 
    paste(
      "d",
      1:length(rownames(mt)),
      sep = ""
    ),
    pos = 4,
    col = "darkmagenta"
  )
  legend(
    "bottom",
    legend,
    cex = 0.5,
    text.col = "darkmagenta"
  )
  
}







# analiza głównych składowych
pca <- prcomp(dtm_tfidf_2_16)

# przygotowanie danych do wykresu
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  rownames(dtm_tfidf_2_16),
  sep = " - "
)

x <- pca$x[,1]
y <- pca$x[,2]

# rysowanie wykresu w przestrzeni 2-wymiarowej
plot(
  x, 
  y, 
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  # xlim = c(-0.03,0.03),
  # ylim = c(0.03,0.06),
  col = "darkmagenta"
)
text(
  x,
  y, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  pos = 4,
  col = "darkmagenta"
)
legend(
  "bottom",
  legend,
  cex = 0.5,
  text.col = "darkmagenta"
)

# zapis wykresu do pliku .png
pca_file <- create_path(
  output_dir,
  "pca.png"
)
png(pca_file)
plot(
  x, 
  y, 
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  col = "darkmagenta"
)
text(
  x,
  y, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  pos = 4,
  col = "darkmagenta"
)
legend(
  "bottom",
  legend,
  cex = 0.5,
  text.col = "darkmagenta"
)
dev.off()


##################################################################################
# lsa.R


# analiza ukrytych wymiarów semantycznych
# dekompozycja według wartości osobliwych 
lsa <- lsa(tdm_tf_2_16_m)

# przygotowanie danych do wykresu
coord_docs <- lsa$dk%*%diag(lsa$sk)
coord_terms <- lsa$tk%*%diag(lsa$sk)

terms_importance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
important_terms <- names(tail(sort(terms_importance),30))
own_terms <- c("eustachy", "harry", "ron", "hermiona", "dumbledore",
               "umbridge", "syriusz", "łucja", "zuzanna", "piotr",
               "edmund", "aslana", "narnii", "bell", "edward",
               "jacob", "wampir", "czarownica", "czarodziej")

coord_important_terms <- coord_terms[important_terms,]
coord_own_terms <- coord_terms[own_terms,] #Coś jest nie tak !

coord_plot_terms <- coord_important_terms

legend <- paste(
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_m)),
    sep = ""
  ),
  colnames(tdm_tf_2_16_m),
  sep = " - "
)

x1 <- coord_docs[,1]
y1 <- coord_docs[,2]

x2 <- coord_plot_terms[,1]
y2 <- coord_plot_terms[,2]

# rysowanie wykresu w przestrzeni 2-wymiarowej
plot(
  x1, 
  y1, 
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  # xlim = c(-25,5),
  # ylim = c(0,20),
  col = "darkmagenta"
)
text(
  x1,
  y1, 
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_m)),
    sep = ""
  ),
  pos = 4,
  col = "darkmagenta"
)
points(
  x2, 
  y2,
  pch = 2,
  col = "darkslateblue"
)
text(
  x2,
  y2, 
  rownames(coord_plot_terms),
  pos = 4,
  col = "darkslateblue"
)
legend(
  "topleft",
  legend,
  cex = 0.5,
  text.col = "darkmagenta"
)

# zapis wykresu do pliku .png
lsa_file <- create_path(
  output_dir,
  "lsa.png"
)
png(lsa_file)
plot(
  x1, 
  y1, 
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  # xlim = c(-25,5),
  # ylim = c(0,20),
  col = "darkmagenta"
)
text(
  x1,
  y1, 
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_m)),
    sep = ""
  ),
  pos = 4,
  col = "darkmagenta"
)
points(
  x2, 
  y2,
  pch = 2,
  col = "darkslateblue"
)
text(
  x2,
  y2, 
  rownames(coord_plot_terms),
  pos = 4,
  col = "darkslateblue"
)
legend(
  "topleft",
  legend,
  cex = 0.5,
  text.col = "darkmagenta"
)
dev.off()



##################################################################################
# lda.R





# zdefiniowanie lokalizacji katalogu na wykresy
topics_dir <- create_path(
  output_dir,
  "topics"
)
dir.create(topics_dir, showWarnings = F)

# analiza ukrytej alokacji Dirichlet'a
n_topics <- 4
lda <- LDA(
  dtm_tf_all,
  n_topics,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100, 
    iter = 3000
  )
)
results <- posterior(lda)

# przygotowanie do wykresów
cols <- c("darkslateblue", "darkseagreen", "lightskyblue", "khaki", "darkred", "darkorange")

# prezentacja tematów
for (topic_no in 1:n_topics) {
  topic_file <- create_path(
    topics_dir,
    create_filename(
      topic_no,
      "png"
    )
  )
  png(topic_file)
  par(mai = c(1,2,1,1))
  terms <- tail(sort(results$terms[topic_no,]),20)
  barplot(
    terms, 
    horiz = TRUE,
    las = 1, 
    main = paste("Temat", topic_no),
    xlab = "Prawdopodobieństwo",
    col = cols[topic_no]
  )
  dev.off()
}

# prezentacja dokumentów
for (doc_no in 1:length(lda@documents)) {
  doc_file <- create_path(
    topics_dir,
    create_filename(
      rownames(results$topics)[doc_no],
      "png"
    )
  )
  png(doc_file)
  par(mai = c(1,2,1,1))
  topics <- results$topics[doc_no,]
  barplot(
    topics, 
    #horiz = TRUE,
    las = 1, 
    main = rownames(results$topics)[doc_no],
    xlab = "Prawdopodobieństwo",
    col = cols
  )
  dev.off()
}

# udział tematów w słowach
words_1 <- c("czas", "czarodziej", "czarownica", "wampir")
round(results$terms[,words_1], 2)

words_2 <- c("albus", "harry", "aslan", "bell")
round(results$terms[,words_2], 2)






##################################################################################
# keywords.R




# zdefiniowanie lokalizacji katalogu na chmury tagów
clouds_dir <- create_path(
  output_dir,
  "clouds"
)
dir.create(clouds_dir, showWarnings = F)

# waga tf jako miara ważności słów
for (doc_no in 1:length(rownames(dtm_tf_all_m))) {
  print(rownames(dtm_tf_all_m)[doc_no])
  print(head(sort(dtm_tf_all_m[doc_no,], decreasing = T)))
}

# waga tfidf jako miara ważności słów
for (doc_no in 1:length(rownames(dtm_tfidf_all_m))) {
  print(rownames(dtm_tfidf_all_m)[doc_no])
  print(head(sort(dtm_tfidf_all_m[doc_no,], decreasing = T)))
}

# prawdopodobieństwo w LDA jako miara ważności słów
for (doc_no in 1:length(lda@documents)) {
  terms_importance <- c(results$topics[doc_no,]%*%results$terms)
  names(terms_importance) <- colnames(results$terms)
  print(rownames(results$topics)[doc_no])
  print(head(sort(terms_importance, decreasing = T)))
}

# chmury tagów
for (doc_no in 1:length(corpus)) {
  cloud_file <- create_path(
    clouds_dir,
    create_filename(
      corpus[[doc_no]]$meta$id,
      "png"
    )
  )
  png(cloud_file)
  par(mai = c(0,0,0,0))
  wordcloud(
    corpus[doc_no],
    max.words = 100,  # domyślnie 200 ale wywala wtedy błąd bo coś się nie mieści...
    colors = brewer.pal(8, "YlGnBu")
  )
  dev.off()
}



##################################################################################
# clustering.R





# analiza skupień
# metoda hierarchiczna 
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odległości (euclidean, jaccard, cosine)
# 3. sposób wyznaczania odległości skupień (completen single, ward.D2)

# przygotowywanie eksperymentów
doc_names <- rownames(dtm_tf_all)
doc_count <- length(doc_names)
legend <- paste(
  paste(
    "d",
    1:doc_count,
    sep = ""
  ),
  doc_names,
  sep = " - "
)
rownames(dtm_tf_all_m) <- paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_bin_all_m) <- paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_tfidf_2_16_m) <- paste(
  "d",
  1:doc_count,
  sep = ""
)
clusters_pattern <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5) # każda liczba to seria książek, jest ich tyle co książek z danej serii
cols <- c("lightskyblue", "darkseagreen", "hotpink","darkred", "khaki", "darkorange", "darkslateblue")
cols_pattern <- c()
for (doc_no in 1:doc_count) {
  cols_pattern[doc_no] <- cols[clusters_pattern[doc_no]]
}
names(clusters_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)
names(cols_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)

# eksperyment 1
dist_matrix_1 <- dist(dtm_tf_all_m, method = "euclidean")
h_clust_1 <- hclust(dist_matrix_1, method = "complete")
plot(h_clust_1)
barplot(h_clust_1$height, names.arg = (doc_count-1):1)
dend_1 <- as.dendrogram(h_clust_1)
clusters_count_1 <- find_k(dend_1)$k
cols_dend_1 <- colour_branches(
  dend_1,
  k = clusters_count_1,
  col = cols
)
plot(cols_dend_1)
legend(
  "topright",
  legend,
  cex = 0.5
)
cols_dend_1 <- colour_branches(
  dend_1,
  col = cols_pattern[dend_1 %>% labels],
)
plot(cols_dend_1)
legend(
  "topright",
  legend,
  cex = 0.5
)
clusters_1 <- cutree(
  h_clust_1,
  k = clusters_count_1
)
clusters_matrix_1 <- matrix(
  0,
  doc_count,
  clusters_count_1
)
rownames(clusters_matrix_1) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_1[doc_no, clusters_1[doc_no]] <- 1
}
corrplot(clusters_matrix_1)

# eksperyment 2
dist_matrix_2 <- dist(dtm_tf_all_m, method = "jaccard")
h_clust_2 <- hclust(dist_matrix_2, method = "single")
plot(h_clust_2)
barplot(h_clust_2$height, names.arg = (doc_count-1):1)
dend_2 <- as.dendrogram(h_clust_2)
clusters_count_2 <- find_k(dend_2)$k
cols_dend_2 <- colour_branches(
  dend_2,
  k = clusters_count_2,
  col = cols
)
plot(cols_dend_2)
legend(
  "topright",
  legend,
  cex = 0.5
)
cols_dend_2 <- colour_branches(
  dend_2,
  col = cols_pattern[dend_2 %>% labels],
)
plot(cols_dend_2)
legend(
  "topright",
  legend,
  cex = 0.5
)
clusters_2 <- cutree(
  h_clust_2,
  k = clusters_count_2
)
clusters_matrix_2 <- matrix(
  0,
  doc_count,
  clusters_count_2
)
rownames(clusters_matrix_2) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_2[doc_no, clusters_2[doc_no]] <- 1
}
corrplot(clusters_matrix_2)
distance_matrix_2 <- as.matrix(dist_matrix_2)
corrplot(distance_matrix_2)

# eksperyment 3
dist_matrix_3 <- dist(dtm_tf_all_m, method = "cosine")
h_clust_3 <- hclust(dist_matrix_3, method = "ward.D2")
plot(h_clust_3)
barplot(h_clust_3$height, names.arg = (doc_count-1):1)
dend_3 <- as.dendrogram(h_clust_3)
clusters_count_3 <- find_k(dend_3)$k
cols_dend_3 <- colour_branches(
  dend_3,
  k = clusters_count_3,
  col = cols
)
plot(cols_dend_3)
legend(
  "topright",
  legend,
  cex = 0.5
)
cols_dend_3 <- colour_branches(
  dend_3,
  col = cols_pattern[dend_3 %>% labels],
)
plot(cols_dend_3)
legend(
  "topright",
  legend,
  cex = 0.5
)
clusters_3 <- cutree(
  h_clust_3,
  k = clusters_count_3
)
clusters_matrix_3 <- matrix(
  0,
  doc_count,
  clusters_count_3
)
rownames(clusters_matrix_3) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_3[doc_no, clusters_3[doc_no]] <- 1
}
corrplot(clusters_matrix_3)
distance_matrix_3 <- as.matrix(dist_matrix_3)
corrplot(distance_matrix_3)

# analiza skupień
# metoda niehierarchiczna 
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. liczba skupień

#eksperyment 4
clusters_count_4 <- 3
k_means_4 <- kmeans(dtm_tfidf_2_16, centers = clusters_count_4)
clusters_4 <- k_means_4$cluster
clusters_matrix_4 <- matrix(
  0,
  doc_count,
  clusters_count_4
)
rownames(clusters_matrix_4) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_4[doc_no, clusters_4[doc_no]] <- 1
}
corrplot(clusters_matrix_4)

# porównanie wyników eksperymentów
Bk_plot(
  dend_1,
  dend_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a"
)
Bk_plot(
  dend_1,
  dend_3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a"
)
Bk_plot(
  dend_3,
  dend_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a"
)

rand_exp1_pattern <- comPart(clusters_1, clusters_pattern)
rand_exp2_pattern <- comPart(clusters_2, clusters_pattern)
rand_exp3_pattern <- comPart(clusters_3, clusters_pattern)
rand_exp4_pattern <- comPart(clusters_4, clusters_pattern)
rand_exp1_exp2 <- comPart(clusters_1, clusters_2)
rand_exp1_exp4 <- comPart(clusters_1, clusters_4)
rand_exp2_exp4 <- comPart(clusters_2, clusters_4)







