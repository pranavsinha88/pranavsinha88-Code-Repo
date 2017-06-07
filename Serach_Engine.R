install.packages("tm")
install.packages("SnowballC")
install.packages("openxlsx")
library(tm)
library(SnowballC)
library(openxlsx)

doc1 <- "Future automation technology trends. The goal of automation is to build flexible production systems that can cope with the sequences of operations"
doc2 <- "ABB is a pioneering technology leader that works closely with utility, industry, transportation and infrastructure customers to write the future of industrial revolution"
doc3 <- "Let's write the future with robots that have what it takes to collaborate. ... ABB's groundbreaking transmission technology connects far-flung energy sources"
doc4 <- "Future; Smart home. ABB smart home technology: smarter, greener living ... See how full integration unlocks a smart building's full potential with ABB Ability"
doc5 <- "ABB: IoTSP is the future of manufacturing. ... Technology and innovation will be the key to reach this goal."
doc6 <- "ABB, the leading power and automation technology group, was cited for a ... to build the grid of the future and will be a big boost for renewable integration."
doc7 <- "ABB, the leading power and automation technology group, today announced a breakthrough in the ability to interrupt Direct Current, solving a 100-year-old"
doc8 <- "Last month ABB participated at the Slush startup and tech conference in Helsinki for the first time."
doc9 <- "Uncovering the Secrets of Future Competitiveness Roman Boutellier, Oliver ... This holds particularly true for ABB's key technologies"
doc10 <- "The ABB corporate document Mission, Values and Policies provides the following statement on ... Technological innovation is essential to secure ABB's future"
doc11 <- "Ulrich Spiesshofer, President & CEO of ABB, talks to IHS Energy SVP Atul Arya about technology and the impact its having on the energy"
doc12 <- "ABB i-bus® KNX | Intelligent building systems technology 03. Why is it safest to plan with ABB? No one can foresee the future."
doc13 <- "However, in the future ABB-PBS expected its wage rates to go up faster than west ... The Technology Role for ABB-PBS "
doc14 <- "ABB Ability Symphony® Plus technology connects ABB's legacy of power generation leadership with its digital future at ABB Customer World"
doc15 <- "Written evidence Memorandum submitted by ABB l. About ABB 1 . 1 ABB is a leader in power and automation technologies that enable utility and industry"
doc16 <- "ABB is paving the way to the Factory of the Future with latest robotics technologies at K 2016"

doc.list <- list(doc1, doc2, doc3, doc4, doc5, doc6, doc7, doc8, doc9, doc10, doc11, doc12, doc13, doc14, doc15, doc16)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))

query <- "ABB Industrial Robotics"

my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")
my.corpus <- Corpus(my.docs)

my.corpus

getTransformations()
my.corpus <- tm_map(my.corpus, removePunctuation)
content(my.corpus[[1]])

my.corpus <- tm_map(my.corpus, stemDocument)
content(my.corpus[[1]])

my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, content_transformer(tolower))
my.corpus <- tm_map(my.corpus, stripWhitespace)
content(my.corpus[[1]])

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
term.doc.matrix.stm$dimnames

colnames(term.doc.matrix.stm) <- c(names(doc.list), "query")
inspect(term.doc.matrix.stm[0:140, ])

tdm_op <- inspect(term.doc.matrix.stm[0:140, ])

write.csv (tdm_op, "tdm_op.csv")

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

write.csv(term.doc.matrix, "search_tdm.csv")

cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n",
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm),
    "bytes.")

get.tf.idf.weights <- function(tf.vec) {
  # Computes tfidf weights from term frequency vector
  n.docs <- length(tf.vec)
  doc.frequency <- length(tf.vec[tf.vec > 0])
  weights <- rep(0, length(tf.vec))
  weights[tf.vec > 0] <- (1 + log2(tf.vec[tf.vec > 0])) * log2(n.docs/doc.frequency)
  return(weights)
}

get.tf.idf.weights(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 15))
tfidf.matrix <- t(apply(term.doc.matrix, 1,
                        FUN = function(row) {get.tf.idf.weights(row)}))

colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix[0:3, ]
tdm_weight <- tfidf.matrix[0:20, ]
write.csv(tdm_weight, "tdm_weight.csv")

angle <- seq(-pi, pi, by = pi/16)

plot(cos(angle) ~ angle, type = "b", xlab = "angle in radians",
     main = "Cosine similarity by angle")
tfidf.matrix <- scale(tfidf.matrix, center = FALSE,
                      scale = sqrt(colSums(tfidf.matrix^2)))
tfidf.matrix[0:20, ]

write.csv(tfidf.matrix[0:20, ], "tdm_norm.csv")

query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]
doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores),
                         text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)
write.xlsx(results.df, row.names = FALSE, right = FALSE, digits = 2, "search_op.xlsx")