
createCorpus <- function( documents, stopWords )
{
	corpus <- Corpus(DataframeSource(documents))
	processedCorpus <- tm_map(corpus, content_transformer(tolower))
	removeSpecialChars <- function(x) gsub("[^a-zçàèéíòóúA-Z0-9 ]","",x)
	processedCorpus <- tm_map(processedCorpus, removeSpecialChars) 
	processedCorpus <- tm_map(processedCorpus, removeNumbers)
	processedCorpus <- tm_map(processedCorpus, removeWords, stopWords)
	processedCorpus <- tm_map(processedCorpus, stemDocument, language = "ca")
	# remove dashes after deletion of words with dashes (some times dashes were stil present)
	processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = FALSE)
	processedCorpus <- tm_map(processedCorpus, stripWhitespace)
	processedCorpus <- tm_map(processedCorpus, removeWords, stopWords)
	return(processedCorpus)
}


# Document-Term Matrix based on corpus and minimum frequency
createDTM <- function( corpus )
{
  DTM <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
  return(DTM)
}

# functions to remove any document without text in the odd case that they are empty once all terms appearing less than minimumFrequency are removed
getPresentIdx <- function(DTM)
{
  presentIdx <- slam::row_sums(DTM) > 0
  return(presentIdx)
}
removeEmptyRows <- function( presentIdx, dataset )
{
  # because some documents do not have any term repeated in other documents we have empty rows (e.g. rows with no terms)
  # LDA will not like this so we remove them from the analysis
  return(dataset[presentIdx,])
}

# compute and plot indexes to choose topic modelling number of topics
exploreNumberTopics <- function( dtm )
{
	resultsTuning <- ldatuning::FindTopicsNumber(dtm, topics = seq(from = 4, to = 25, by = 1), metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), method = "Gibbs", verbose = TRUE)
  	g1 <- ggplot(resultsTuning, aes(x=topics, y=CaoJuan2009)) + geom_line() + geom_point() + ggtitle("minimize CaoJuan2009") + theme_bw() + scale_x_continuous(breaks=seq(min(resultsTuning$topics), max(resultsTuning$topics,1)))
	  g2 <- ggplot(resultsTuning, aes(x=topics, y=Arun2010)) + geom_line() + geom_point() + ggtitle("minimize Arun 2010") + theme_bw() + scale_x_continuous(breaks=seq(min(resultsTuning$topics), max(resultsTuning$topics,1)))
  
	  g3 <- ggplot(resultsTuning, aes(x=topics, y=Deveaud2014)) + geom_line() + geom_point() + ggtitle("maximize Deveaud2014") + theme_bw()+ scale_x_continuous(breaks=seq(min(resultsTuning$topics), max(resultsTuning$topics,1)))
	  g4 <- ggplot(resultsTuning, aes(x=topics, y=Griffiths2004)) + geom_line() + geom_point() + ggtitle("maximize Griffiths2004") + theme_bw()+ scale_x_continuous(breaks=seq(min(resultsTuning$topics), max(resultsTuning$topics,1)))
	  return(grid.arrange(g1, g2, g3, g4, ncol=2))
}

# get list of top terms per topic after LDA
getTopTerms <- function(lda, numTermsPerTopic)
{
	betas <- tidy(lda, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:numTermsPerTopic)
	betas$topic <- paste("Topic n.",as.character(betas$topic))
	return(betas)
}


# visualize as an ordered barplot the top terms of each topic
plotTopTerms <- function(topTerms)
{
ggplot(topTerms, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", strip.position = "top") + theme(legend.position="none") + theme_bw() + ggtitle("Top Terms per Topic") + scale_y_reordered()+ ylab("top terms") + xlab("term weight for topic")
}


