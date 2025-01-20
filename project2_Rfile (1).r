install.packages("rvest")
install.packages("SnowballC")
install.packages("tokenizers")
install.packages("stringr")
install.packages("hunspell")
install.packages("tm")
install.packages("textstem")
install.packages("topicmodels")
library(topicmodels)


library(rvest)
library(tokenizers)
library(SnowballC)
library(stringr)
library(hunspell)
library(textstem)
library(tm)


url <- "https://www.dhakatribune.com/bangladesh/politics/366589/july-revolution-student-leaders-to-launch-new?fbclid=IwZXh0bgNhZW0CMTEAAR18J1Kfoc9H96TSLsyZKjVhqnp6NYVmgpA8Mfbyt8bO6tPAAWyN-QMBvio_aem_WrOQTSVHYAF1E5pRhmwxLQ"
webpage <- read_html(url)
text_data <- html_text(html_nodes(webpage, "p"))

# Text Cleaning
text_data <- tolower(text_data)
text_data <- gsub("[[:punct:]]", "", text_data)
text_data <- gsub("\\d+", "", text_data)
text_data <- stripWhitespace(text_data)


contractions <- c("can't" = "cannot", "won't" = "will not", "I'm" = "I am", "it's" = "it is")
expand_contractions <- function(text, contractions) {
  for (contraction in names(contractions)) {
    text <- gsub(contraction, contractions[contraction], text, ignore.case = TRUE)
  }
  return(text)
}
expanded_text <- sapply(text_data, expand_contractions, contractions = contractions)


emoji_pattern <- "[\U0001F600-\U0001F64F]"
emoticon_pattern <- "[:;]-?[)D(|P]"
text_no_emojis <- sapply(expanded_text, function(text) {
  text <- str_remove_all(text, emoji_pattern)
  text <- str_remove_all(text, emoticon_pattern)
  return(text)
})


tokens <- tokenize_words(text_no_emojis)


text_no_stopwords <- removeWords(unlist(tokens), stopwords("en"))


remove_misspelled_words <- function(text) {
  words <- unlist(tokenize_words(text))
  is_correct <- hunspell_check(words)
  corrected_words <- words[is_correct]
  return(paste(corrected_words, collapse = " "))
}
spell_checked_text <- sapply(text_no_stopwords, remove_misspelled_words)


clean_tokens <- unlist(tokenize_words(spell_checked_text))
clean_tokens <- clean_tokens[nchar(clean_tokens) > 1 & !grepl("^[0-9[:punct:]]+$", clean_tokens)]

clean_tokens <- lemmatize_words(clean_tokens)



projectTwo <- data.frame(tokens = clean_tokens, stringsAsFactors = FALSE)


#write.csv(projectTwo, "D://projectTwo.csv", row.names = FALSE)

# Create a Document-Term Matrix
dtm <- DocumentTermMatrix(projectTwo)
inspect(dtm)

# Calculate TF-IDF
dtm_tfidf <- weightTfIdf(dtm)
inspect(dtm_tfidf)



# Fit the LDA model
k <- 2  # Number of topics
lda_model <- LDA(dtm, k = k, control = list(seed = 500))

# Get terms associated with each topic
terms(lda_model, 10)  # Top 10 terms for each topic

# Get topic proportions for each document
topic_proportions <- posterior(lda_model)$topics
print(topic_proportions)
write.csv(topic_proportions, "D://projectTwo.csv", row.names = FALSE)



