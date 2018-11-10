library(dplyr)
install.packages("tidytext")
library(tidytext)
library(tidyr)
library(ggplot2)
library(lubridate)
ArticlesApril18 <- read.csv("C:/Users/kishore vavilapalli/Downloads/nyt-comments/ArticlesApril2018.csv", stringsAsFactors = F)
CommentsApril18 <- read.csv("C:/Users/kishore vavilapalli/Downloads/nyt-comments/CommentsApril2018.csv", stringsAsFactors = F)
dim(ArticlesApril2018)
dim(CommentsApril2018)
ArticlesApril2018 %>%
  count(byline) %>%
  mutate(byline = reorder(byline, n)) %>%
  top_n(20) %>%
  ggplot(aes(x=byline, y=n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x="Byline", y="Number of articles") +
  coord_flip()+
  theme_bw()
ArticlesApril2018 %>%
  count(newDesk) %>%
  mutate(newDesk = reorder(newDesk, n)) %>%
  ggplot(aes(x=newDesk, y=n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x="News Desk", y="Number of articles") +
  coord_flip()+
  theme_bw()
comment_count <- CommentsApril2018 %>%
  count(articleID) %>%
  rename(num_comments = n)
ArticlesApril2018 <- ArticlesApril2018 %>%
  left_join(comment_count, by = "articleID")
ggplot(data = ArticlesApril2018, aes(x= num_comments)) +
  geom_histogram(col = "darkblue", fill = "white", bins = 30) +
  labs(x= "Number of Comments", y="") +
  theme_minimal()
summary(ArticlesApril2018$num_comments)
ggplot(data = ArticlesApril2018, aes(x= articleWordCount, y= num_comments)) +
  geom_point(col = "darkblue", alpha = 0.5) +
  geom_smooth(method = "lm", linetype = 2, se = F, col = "red") +
  labs(x= "Word Count", y= "Number of comments") +
  theme_bw()
print(paste("The correlation between the article's wordcount and the number of comments it receives is ", round(cor(ArticlesApril2018$articleWordCount, ArticlesApril2018$num_comments),4), sep = ""))

tidy_articles <- ArticlesApril2018 %>%
  select(articleID, byline, headline, pubDate, typeOfMaterial, articleWordCount, num_comments) %>%
  unnest_tokens(word, headline) %>%
  anti_join(stop_words, by="word")
headline_sentiments <-  tidy_articles %>%
  inner_join(get_sentiments("bing"), by = "word")

tidy_articles %>%
  count(word) %>%
  top_n(25) %>%
  ggplot(aes(x= reorder(word, n), y=n)) +
  geom_col(fill = "darkblue") + 
  coord_flip() +
  labs(x= "Word", y= "Count") +
  theme_bw()

tidy_articles$word[tidy_articles$word == "trump's"] <- "trump"
tidy_articles %>%
  count(word) %>%
  top_n(25) %>%
  ggplot(aes(x= reorder(word, n), y=n)) +
  geom_col(fill = "darkblue") + 
  coord_flip() +
  labs(x= "Word", y= "Count") +
  theme_bw()

# Are headlines more positive or negative?

headline_sentiments <-  tidy_articles %>%
  inner_join(get_sentiments("bing"), by = "word")

headline_sentiments %>%
  count(sentiment) %>%
  ggplot(aes(x= sentiment, y= n)) +
  geom_col(fill = c("lightgrey", "darkblue")) +
  labs(x="Headline Sentiment", y= "Count") +
  theme_minimal()

headline_sentiments$pubDate <- ymd_hms(headline_sentiments$pubDate)
headline_sentiments$day_of_week <- wday(headline_sentiments$pubDate, label = TRUE)

ggplot(data = headline_sentiments, aes(x=day_of_week, fill = sentiment)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("lightgrey", "darkblue")) +
  labs(x= "Weekday", "Frequency") +
  theme_minimal()

# What are some of the words being used?

headline_word_counts <- headline_sentiments %>%
  count(word, sentiment)

headline_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x= word, y= n, fill = sentiment)) +
  geom_col(show.legend = FALSE, fill = "darkblue") +
  facet_wrap(~sentiment, scales = "free") +  
  labs(x= "Count", y= "Word") +
  coord_flip() +
  theme_bw()

headline_word_counts %>%
  filter(word != "trump", word != "unknown") %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x= word, y= n, fill = sentiment)) +
  geom_col(show.legend = FALSE, fill = "darkblue") +
  facet_wrap(~sentiment, scales = "free") + 
  labs(x= "Count", y= "Word") +
  coord_flip() +
  theme_bw()

# create a df of the top 25 used words
top_25_headline_words <- tidy_articles %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  select(word)

# join articles metadata to the top 25 words df
top_25_headline_words <- tidy_articles %>%
  inner_join(top_25_headline_words, by = "word")
# boxplot
ggplot(data = top_25_headline_words, aes(x=word, y= num_comments)) +
  geom_boxplot(col = "darkblue", fill = "lightgrey", alpha = 0.2) +
  labs(x= "Word", y= "Number of Comments") +
  coord_flip() +
  theme_bw()

trump_articles <- tidy_articles %>%
  mutate(trump_yes_no = factor(ifelse(word == "trump", "yes", "no"))) %>%
  filter(trump_yes_no == "yes") %>%
  select(articleID, trump_yes_no) %>%
  right_join(comment_count, by = "articleID")

trump_articles$trump_yes_no[is.na(trump_articles$trump_yes_no)] <- "no"

fit_trump <- lm(num_comments ~ trump_yes_no, data = trump_articles)

summary(fit_trump)
