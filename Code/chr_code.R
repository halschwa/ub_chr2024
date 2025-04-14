### load libraries

#topic modelling
library(tm) 
# data viz
library(ggplot2)
library(gridExtra)
# assess n. topics parameter for LDA
library(ldatuning)
# LDA
library(topicmodels)
# pipeline and data manipulation
library(tidyverse)
library(tidytext)
library(reshape2)

# additional
# library(LDAvis)
# library(Rtsne)


# load helper functions
source("functions.R")

### parameters 

# minimum number of records where a term appears to be included in the analysis
minimumFrequency <- 2
# number of terms to visualize for each found topic
numTermsPerTopic <- 10
# list of stopwords
wordsToIgnore <- c("del", "dels", "al", "un", "una", "als", "gran", "part", "ac", "a.c", "fa", "crist", "anys", "partir", "també", "però", "sobre",'form')


### main code

## 1. load dataset
textos <- read.csv("textos.csv")

# remove apostrophes
apostrophes <- c("d'","d’","s’", "s'","l'","l’","n'", "n’","D'","D’","S’","S'","L'","L’","N'", "N’")
textos$text<- gsub(paste0(apostrophes,collapse = "|"),"", textos$text)

## 2. word frequency analysis
stopWords <- c(tm::stopwords("ca"), wordsToIgnore)
stopWords <- data.frame(word=stopWords)

tidyWords <- textos %>% unnest_tokens(word, text)
tidyWords <- tidyWords %>% anti_join(stopWords) 


wordsFreqAll<- tidyWords %>% count(word, sort = TRUE) %>% 
  mutate(freq=n/sum(n)) %>% 
  arrange(desc(n)) %>% slice(1:10)
wordsFreqMuseum <- tidyWords %>% group_by(museu) %>% count(word, sort = TRUE) %>% 
  mutate(freq=n/sum(n)) %>% 
  arrange(desc(n)) %>% slice(1:10)

g1 <- ggplot(wordsFreqAll, aes(x=n, y=reorder(word, n))) + geom_bar(stat="identity", fill="indianred2") + theme_bw()
g2 <- ggplot(wordsFreqMuseum, aes(x=n, fill=museu, y=reorder_within(word, n, museu))) + geom_bar(stat="identity") + facet_wrap(~museu, scales="free_y", nrow=2) + theme_bw() + scale_y_reordered() + theme(legend.position="none")

pdf("freqParaules.pdf", width=10, height=8)
grid.arrange(g1,g2, ncol=1)
dev.off()

## 3. topic modelling

## create corpus, dtm, and remove any empty text
textosTM <- textos
textosTM$doc_id <- textos$museu
textosTM$museu <- NULL

corpus <- createCorpus(textosTM, stopWords$word)
dtm <- createDTM(corpus)
# remove any empty row after applying stopwords and any terms under minimumFrequency
presentIdx <- getPresentIdx(dtm)
dtm <- removeEmptyRows(presentIdx, dtm)
textos <- removeEmptyRows(presentIdx, textosTM)

# assess number of topics per corpus
numTopicsPlot <- exploreNumberTopics(dtm)

# choose number of topics
numTopics <- 8

# run LDA
lda <- LDA(dtm, numTopics, method = "Gibbs", control = list(iter = 1000, verbose = 25, alpha = 50/numTopics))

# plot top terms per topic
topTerms <- getTopTerms(lda, numTermsPerTopic)
pdf("topTermsLem.pdf", width=12, height=6)
plotTopTerms(topTerms)
dev.off()

