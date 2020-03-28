# directory <- "ACDBooks"
directory <- "CBBCollection"



# Code cell 2
library(mallet)
filenames <- dir(paste("DCS1020/data", directory, sep = "/"))
titles <- character()
rawtitles <- filenames
for (i in 1:length(rawtitles)){
  workingfilename <- unlist(strsplit(rawtitles[i], split = ""))
  finalfilename <- paste(c(workingfilename[5:(length(workingfilename)-4)]), collapse = "")
  titles[i] <- finalfilename
}
docs <- as.character()
for (i in 1:length(filenames)){
  text <- scan(paste("DCS1020/data", directory, filenames[i], sep = "/"), what = "\n")
  docs[i] <- paste(text, collapse = " ")
}
docs <- gsub("[[:cntrl:]]", " ", docs)  # replace control characters with space
docs <- gsub("[[:digit:]]", "", docs) # remove numbers - replace with blank
docs <- gsub("\\,", " \\, ", docs) # puts spaces around commas
docs <- gsub("\\.", " \\. ", docs) # puts spaces around periods
docs <- gsub("\\?", " \\? ", docs) # puts spaces around question marks
docs <- gsub("\\;", " \\; ", docs) # you see the pattern, right?
# add and edit lines as you see fit for other types of punctuation
docs <- tolower(docs)



# Code cell 4
K <- 25
threshold <- 2
G <- 5000
alpha <- 0.2
eta <- 0.2



# Code cell 5

library(tm)
docs_list <- strsplit(docs, "[[:space:]]+") # like the "\\W" step we have seen before
print(length(docs_list))
stopWordFile = "DCS1020/data/stopwords.txt"
stop_words = scan(stopWordFile, what = "character", sep = "\n")
term.table <- table(unlist(docs_list))
term.table <- sort(term.table, decreasing = TRUE)
print("The term table has dimensions:")
print(dim(term.table))

# remove terms that are stop words or occur under the threshold:
# also from https://ldavis.cpsievert.me/reviews/reviews.html

del <- names(term.table) %in% stop_words | term.table < threshold 
term.table <- term.table[!del]
term.table <- term.table[which(names(term.table) != "")]

# putting everything into the right format for building the model
corpus <- Corpus(VectorSource(docs))
DTM <- DocumentTermMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = FALSE, dictionary = names(term.table), wordLengths = c(1, Inf)))


print("The term table without stop words and low frequency words has dimensions:")
print(dim(term.table))

vocab <- names(term.table)



# Code cell 7
# functions below provided by https://ldavis.cpsievert.me/reviews/reviews.html
get.terms <- function(x) { # determines which vocab words in corpus are in each document 
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(docs_list, get.terms) # determines which vocab words in corpus are in each document



# Code cell 8
# establish variables for LDA function
# outlined by https://ldavis.cpsievert.me/reviews/reviews.html
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 



# Code cell 10
library(lda)
set.seed(357) # random number generator, but allows results to be reproduced
# if correctly understood, randomly generated results, like the topics, can be replicated on a corpus
t1 <- Sys.time() # get start time
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time() #get end time
print(t2 - t1) # visualize time elapsed



# Code cell 11
top.words <- top.topic.words(fit$topics, 25, by.score=TRUE)
top_words_filename <- paste("DCS1020/CBBResults/", directory, K, "topics", alpha, "alpha",
                            eta, "etaTopWords.csv", sep = '')
write.csv(top.words,file = top_words_filename, row.names = FALSE)



# Code cell 12
library(data.table)
all_top <- as.character()
for(i in 1:ncol(top.words)){
  all_top <- c(all_top, top.words[,i])
}
unique_top <- unique(all_top)
top_table <- sort(table(all_top), decreasing = TRUE)
top_assign <- fit[["topics"]][, unique_top]
top_assign_df <- cbind(data.frame(top_assign, topic = factor(1:K)))
colnames(top_assign_df) <- c(colnames(top_assign), "topic")
word_weight <- melt(top_assign_df)



# Code cell 13
library(wordcloud)
for(i in 1:K){
  subset <- word_weight[which(word_weight$topic == i),]
  clean_subset <- subset[which(subset$value > 0),]
  ordered_sub <- clean_subset[order(-clean_subset$value),]
  top25 <- ordered_sub[which(ordered_sub$variable %in% top.words[,i]) ,]
  wc_title <- paste(directory, "\nTopic ", i, "of", K, sep = "")
  wc_filename <- paste("DCS1020/CBBResults/", directory, K, "topics", alpha, "alpha",
                       eta, "Topic", i, ".jpg", sep = '')
  jpeg(wc_filename)
  wordcloud(top25$variable, top25$value, c(5, 1), min.freq = 1, rot.per = 0, random.order = F)
  par(adj = 0)
  title(wc_title)
  dev.off()
}


# Code cell 14
# Topic proportions
topic.proportions <- t(fit$document_sums) / colSums(fit$document_sums)
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions <- cbind(topic.proportions, Document = filenames)
# each column is a topic, each row is a chapter, cells show the proportion of the document dedicated to the topic
# can be used to show topic distribution over documents or time
# use this file for graphing with Excel or R
print("Saving topic distributions csv.")
distributions_filename <- paste("DCS1020/CBBResults/", directory, K, "topics", alpha, "alpha",
                                eta, "etaTopicDistributions.csv", sep = '')
write.csv(topic.proportions, file = distributions_filename)


# Topic Proportions
# there will be a warning with the next step -
# as long as number of wraps * number of topics = length of your new df
# no data was lost
topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:length(documents), labels = "wrap")),
                             variable.name="topic",
                             id.vars = c("Document"))
# shows each document\'s representation of each topic
# each row is a document-topic observation; columns give doc number, year, filename,
# keyword occurrence number,topic, and value of topic in that document
print("Saving topic proportions csv")
proportions_filename <- paste("DCS1020/CBBResults/", directory, K, "topics", alpha, "alpha",
                              eta, "etaTopicProportions.csv", sep = '')
write.csv(topic.proportions.df, file = proportions_filename)



# Code cell 15
#Identify representative documents/wraps
topics <- unique(topic.proportions.df$topic)
#topics <- topics[c(3,7,21)]
for (i in 1:length(topics)){
  topic <- as.character(topics[i])
  tws <- unlist(strsplit(topic, '\\W'))
  top5 <- paste(tws[1:5], collapse = "_")
  file <- paste("DCS1020/CBBResults/", directory, K, "topics", alpha, "alpha",
                eta, "eta", top5, "Examples.txt", sep = '')
  topic_df <- topic.proportions.df[which(topic.proportions.df$topic == topic),]
  obvs <- topic_df[which(topic_df$value >= 0.50),]
  for (x in 1:nrow(obvs)){
    filename <- obvs$Document[x]
    write(filename, file, append = TRUE)
  }
}