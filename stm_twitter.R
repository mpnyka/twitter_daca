setwd("~/Desktop/Sociology/Research/Twitter")
library("stm")
library("ggplot2")
library("dplyr")
library(quanteda)
library(tidyverse)
library("tidytext")
library(drlib)

## reading text in
tweets <- read.csv("tweets_daca_pol.csv")
tweets$position <- ifelse(tweets$polarity > 0.6, 1, ifelse(tweets$polarity < 0.4, -1, 0)) 
ok <- !is.na(tweets$polarity)
stopwords <- readLines("stopwordsR.txt")

tweets <- tweets[ok,]

## tweets 2

tweets2 <- read.csv("tweets_daca_pol2.csv")
tweets2$position <- ifelse(tweets2$polarity > 0.6, 1, ifelse(tweets2$polarity < 0.4, -1, 0)) 
ok <- !is.na(tweets2$polarity)
tweets2 <- tweets2[ok,]

## plot distribution of polarity score
a <- ggplot(tweets, aes(tweets$polarity))
a + geom_histogram(fill="#FEC500") + labs(x = "polarity score", y = "") +
theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "#FAF9F6"), text = element_text(colour = "black"),
         axis.text = element_text(colour = "black") )

a2 <- ggplot(tweets2, aes(tweets2$polarity))
a2 + geom_histogram(fill="#FEC500") + labs(title ="Distribution of polarity scores", x = "polarity score", y = "", color= "white")

## plot distribution of for/undefined/against
b <- ggplot(tweets, aes(tweets$position))
b + geom_bar(fill=c("#27627F", "#003D50", "#001824")) + labs(title ="Positions", x = "", y = "")

## random sample of 20 tweets:

positive <- sample(tweets$tweet[tweets$position==1], 20, replace=FALSE)
undefined <- sample(tweets$tweet[tweets$position==0], 20, replace=FALSE)
negative <- sample(tweets$tweet[tweets$position==-1], 20, replace=FALSE)

processed <- textProcessor(tweets$tweet, metadata = tweets, removestopwords= TRUE, customstopwords = c("one", "hes", "doesnt", "say", "behind","also", "big","dont", "like","dream", "act", "now", "last", "one", "will", "got", "away", "can", "let", "daca", "dreamers", "amp", "like"))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

## estimating topic model, covartiates as prevalence:
twitterSTM <- stm(documents = out$documents, vocab = out$vocab,
                       K = 18, prevalence =~ polarity,
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral")

td_twitter <- tidy(twitterSTM)

td_twitter %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "#FAF9F6"))+
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta))

##see topics
labelTopics(twitterSTM)

prep <- estimateEffect(1:18 ~ polarity, twitterSTM,
                       meta = out$meta, uncertainty = "Global")

summary(prep, topics = 3)

plot(prep, covariate = "polarity", topics = 1:18,
     model = twitterSTM, method = "difference",
     cov.value1 = 1, cov.value2 = 0,
     xlab = "More Against ... More in Favor",
     xlim = c(-.5, .5), labeltype = "custom",
     custom.labels = 1:18)


plot(prep, covariate = "polarity", topics = 1:18,
     model = twitterSTM, method = "continuous",
     xlab = "Effect of For vs. Against",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(0, 1), labeltype = "custom",
     custom.labels = 1:18)

plot(twitterSTM, type = "summary", xlim = c(0, 1))

thoughts3 <- findThoughts(twitterSTM, texts =  out$meta$tweet,
                             n = 5, topics = 3)$docs[[1]]
thoughts8 <- findThoughts(twitterSTM, texts = out$meta$tweet,
                              n = 5, topics = 8)$docs[[1]]

thoughts17 <- findThoughts(twitterSTM, texts = out$meta$tweet,
                          n = 20, topics = 17)$docs[[1]]

par(mfrow = c(1, 2),mar = c(2, .8, 1, .8))
plotQuote(thoughts3[c(1, 4, 5)], width = 30, main = "Topic 3")
plotQuote(thoughts20[c(1, 2, 3)], width = 30, main = "Topic 8")
par(mfrow = c(1, 1),mar = c(2, .8, 1, .8))
plotQuote(thoughts17[c(12, 13, 8)], width = 30, main = "Topic 17")

mod.out.corr <- topicCorr(twitterSTM)
plot(mod.out.corr)

storage <- searchK(out$documents, out$vocab, K = c(18, 20, 22),
                   prevalence =~ polarity, data = meta)
plot(storage)

## trying function removeWord

gsub(sprintf("(*UCP)\\b(%s)\\b", paste(sort(c('daca', 'dreamers'), decreasing = TRUE), collapse = "|")), "", 'hi hello daca', perl = TRUE)
