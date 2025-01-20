install.packages("rvest")
library(rvest)
install.packages("tm")
library(tm)
install.packages("tokenizers")
library(tokenizers)
install.packages("Snowballc")
library(SnowballC)
install.packages("stringr")
library(stringr)
install.packages("hunspell")
library(hunspell)
install.packages("textstem")
library(textstem)

url <- "https://www.thedailystar.net/opinion/views/news/july-revolution-and-gen-zs-march-mass-political-awareness-3675326?fbclid=IwY2xjawHH_5lleHRuA2FlbQIxMAABHXYnNd7VLZo5JW5F-J1xOkDiCGqPnjh0S1GgL0Wmyb_alS067Aj40ajcig_aem_XBocNH6QhTj8Uxzm3d-SPw"
webpage <- read_html(url)
text_data <- html_text(html_nodes(webpage, "p"))



# clean
install.packages("httr")
library(httr)

# Function to fetch and clean text from a URL
fetch_and_clean_text <- function(url) {
  # Fetch the HTML content from the URL
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })
  
  if (is.null(webpage)) return(NULL)
  
  # Extract the text content (modify selector based on website structure)
  text <- webpage %>%
    html_nodes("p") %>%  # Adjust this selector as needed
    html_text() %>%
    paste(collapse = " ")  # Combine all paragraphs into a single string
  
  # Clean the text
  text <- tolower(text)  # Convert to lowercase
  text <- str_remove_all(text, "http\\S+|www\\.\\S+")  # Remove URLs
  text <- str_remove_all(text, "<.*?>")  # Remove HTML tags
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")  # Remove non-ASCII
  text <- str_remove_all(text, "[[:punct:]]")  # Remove punctuation
  text <- str_remove_all(text, "\\d+")  # Remove numbers
  words <- unlist(strsplit(text, "\\s+"))  # Tokenize
  stop_words <- stopwords("en")
  words <- words[!(words %in% stop_words)]  # Remove stopwords
  words <- wordStem(words, language = "en")  # Stem words
  cleaned_text <- paste(words, collapse = " ")  # Recombine
  
  return(cleaned_text)
}

# Example usage
url <- "https://www.thedailystar.net/opinion/views/news/july-revolution-and-gen-zs-march-mass-political-awareness-3675326?fbclid=IwY2xjawHH_5lleHRuA2FlbQIxMAABHXYnNd7VLZo5JW5F-J1xOkDiCGqPnjh0S1GgL0Wmyb_alS067Aj40ajcig_aem_XBocNH6QhTj8Uxzm3d-SPw"
cleaned_text <- fetch_and_clean_text(url)

if (!is.null(cleaned_text)) {
  cat("Cleaned Text:\n", cleaned_text)
}






#token


install.packages("tidytext")

library(tidytext)

fetch_clean_and_tokenize <- function(url) {
  # Fetch the HTML content from the URL
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })
  
  if (is.null(webpage)) return(NULL)
  
  # Extract text content (adjust selector if needed)
  text <- webpage %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = " ")  # Combine all paragraphs
  
  # Clean the text
  text <- tolower(text)  # Convert to lowercase
  text <- str_remove_all(text, "http\\S+|www\\.\\S+")  # Remove URLs
  text <- str_remove_all(text, "<.*?>")  # Remove HTML tags
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")  # Remove non-ASCII
  text <- str_remove_all(text, "[[:punct:]]")  # Remove punctuation
  text <- str_remove_all(text, "\\d+")  # Remove numbers
  
  # Tokenize the cleaned text
  tokens <- unlist(strsplit(text, "\\s+"))  # Split by whitespace
  tokens <- tokens[tokens != ""]  # Remove empty tokens
  
  # Remove stopwords
  stop_words <- stopwords("en")
  tokens <- tokens[!(tokens %in% stop_words)]
  
  # Stem words
  tokens <- wordStem(tokens, language = "en")
  
  return(tokens)
}

# Example usage
url <- "https://www.thedailystar.net/opinion/views/news/july-revolution-and-gen-zs-march-mass-political-awareness-3675326"
tokens <- fetch_clean_and_tokenize(url)

if (!is.null(tokens)) {
  print("Tokenized Text:")
  print(tokens)
}




















