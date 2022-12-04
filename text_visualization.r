library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(stm)
library(seededlda)
library(tm)
rm(list=ls(all=TRUE))
metadata <- read_csv("data/AidDatasGlobalChineseDevelopmentFinanceDataset_v2.0.csv")
corpusT <- corpus(metadata, text_field = "Title")
toksT <- tokens(corpusT, remove_punct = TRUE, remove_numbers=TRUE)
toksT <- tokens_wordstem(toksT)
toksT <- tokens_select(toksT,  stopwords("en"), selection = "remove")
dfmT <- dfm(toksT)
dfm_trimmedT <- dfm_trim(dfmT, min_docfreq = 0.05, docfreq_type = "prop")
dfm_cloudT <- dfm_remove(dfm_trimmedT,c("project",'$','chines','china','see','sign','million','provid', 'link'))
textplot_wordcloud(dfm_cloudT, col="black")
corpus <- corpus(metadata, text_field = "Description")
toks <- tokens(corpus, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfmD <- dfm(toks)
dfm_trimmed <- dfm_trim(dfmD, min_docfreq = 0.05, docfreq_type = "prop")
dfm_cloud <- dfm_remove(dfm_trimmed,c("project",'$','chines','china','see','sign','million'))
textplot_wordcloud(dfm_cloud, col="black")
lda <- textmodel_lda(dfm_trimmed, k = 10)
lda.terms <- terms(lda, 10)
lda.terms
mu <- lda$phi
dim(mu)
mu[1,][order(mu[1,], decreasing=T)][1:10]
mu[2,][order(mu[2,], decreasing=T)][1:10]
mu[3,][order(mu[3,], decreasing=T)][1:10]
mu[4,][order(mu[4,], decreasing=T)][1:10]
mu[5,][order(mu[5,], decreasing=T)][1:10]
mu[6,][order(mu[6,], decreasing=T)][1:10]
mu[7,][order(mu[7,], decreasing=T)][1:10]
mu[8,][order(mu[8,], decreasing=T)][1:10]
mu[9,][order(mu[9,], decreasing=T)][1:10]
mu[10,][order(mu[10,], decreasing=T)][1:10]
pi <- lda$theta
dim(pi)
metadata[order(pi[1,],decreasing=T),]
metadata[order(pi[2,],decreasing=T),]
metadata[order(pi[3,],decreasing=T),]
metadata[order(pi[4,],decreasing=T),]
metadata[order(pi[5,],decreasing=T),]
metadata[order(pi[6,],decreasing=T),]
metadata[order(pi[7,],decreasing=T),]
metadata[order(pi[8,],decreasing=T),]
metadata[order(pi[9,],decreasing=T),]
metadata[order(pi[10,],decreasing=T),]
temp <- textProcessor(documents=metadata$Description,metadata=metadata)
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)
model.stm <- stm(out$documents, out$vocab, K = 10, data = out$meta) 
findThoughts(model.stm, texts=out$meta$Title, topics=1, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=2, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=3, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=4, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=5, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=6, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=7, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=8, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=9, n=3)
findThoughts(model.stm, texts=out$meta$Title, topics=10, n=3)
labelTopics(model.stm)
plot(model.stm, n=10)
