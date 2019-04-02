library(dplyr)
library(ggplot2)
library(readr)
library(tidytext) # this one
library(stringr)
library(tidyr)
library(rvest)
library(udpipe) # and this one
library(scales)
## here we are going to find the root_file 
root <- rprojroot::find_rstudio_root_file()
## we are going to set the data file path here
dataDir <- file.path(root, 'Data')

jacobsQuotes <- c("Cities have the capability of providing something for everybody, only because, and only when, they are created by everybody.",
                  "A city street equipped to handle strangers, and to make a safety asset, in itself, our of the presence of strangers, as the streets of successful city neighborhoods always do, must have three main qualities",
                  "First, there must be a clear demarcation between what is public space and what is private space. Public and private spaces cannot ooze into each other as they do typically in suburban settings or in projects.",
                  "Second, there must be eyes upon the street, eyes belonging to those we might call the natural proprietors of the street. The buildings on a street equipped to handle strangers and to insure the safety of both residents and strangers, must be oriented to the street. They cannot turn their backs or blank sides on it and leave it blind.",
                  "And third, the sidewalk must have users on it fairly continuously, both to add to the number of effective eyes on the street and to induce the people in buildings along the street to watch the sidewalks in sufficient numbers. Nobody enjoys sitting on a stoop or looking out a window at an empty street. Almost nobody does such a thing. Large numbers of people entertain themselves, off and on, by watching street activity."
)

jacobsQuotes

text_df <- tibble(quote = 1:length(jacobsQuotes), text = jacobsQuotes)
text_df 

text_df %>% unnest_tokens(word, text)

data("stop_words")
stop_words
text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words)

text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    count(word, sort =TRUE)

homePage <- "https://www.citylab.com/"
topStories <- read_html(homePage) %>% 
    html_nodes(css = "a.c-most-popular__link") %>% 
    html_attr(name = "href")
topStories

scrapeArticle <- function(url){
    html <- read_html(url)
    title <- html %>% 
        html_nodes("article h1") %>% 
        html_text()
    text <- html %>% 
        html_nodes("article") %>% 
        html_nodes(".s-article__section p") %>% 
        html_text()
        
    tibble(title, text, section = 1:length(text))
}

textDF <- topStories %>% 
    as.list() %>% purrr::map_df(~scrapeArticle(.x))
textDF

textDF %>% distinct(title)

textDF %>% View()

tidy_articles <- textDF %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)

tidy_articles %>% count(word, sort = TRUE)

tidy_articles %>% 
    count(word, sort = TRUE) %>% 
    filter(n>15) %>% 
    mutate(word = reorder(word,n)) %>% 
    ggplot(aes(word,n)) + 
    geom_col() + 
    xlab(NULL) +
    coord_flip() +
    theme_minimal()

frequency <- tidy_articles %>% 
    mutate(word = str_extract(word, "[a-z']+")) %>% 
    count(title, word) %>% 
    group_by(title) %>% 
    mutate(proportion = n/sum(n)) %>% 
    select(-n) %>% 
    spread(title, proportion) %>% 
    gather(title, proportion, 3:6)
tidy_articles %>% distinct(title)

frequency %>% 
    ggplot(aes(x = proportion,
               y =`A Guide to Successful Place-Based Economic Policies`))+
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = .3, height = .3)+
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) + 
    facet_wrap(~title, ncol = 2) + 
    theme(legend.position = "none") +
    theme_minimal()

    
cor.test(data = frequency[frequency$title == "How Poor Americans Get Exploited by Their Landlords",],
         ~proportion + `A Guide to Successful Place-Based Economic Policies`)
    
cor.test(data = frequency[frequency$title == "Solar Batteries Are Winning Over German Homeowners",],
         ~proportion + `A Guide to Successful Place-Based Economic Policies`)
cor.test(data = frequency[frequency$title == "WeWork Wants to Build the ‘Future of Cities.’ What Does That Mean?",],
         ~proportion + `A Guide to Successful Place-Based Economic Policies`)
Let’s Buy a Train  
cor.test(data = frequency[frequency$title == "Let’s Buy a Train",],
         ~proportion + `A Guide to Successful Place-Based Economic Policies`)

#model <- udpipe_download_model(language = "english-ewt")
udmodel_english <- udpipe_load_model(file = file.path(dataDir, "english-ewt-ud-2.3-181115.udpipe"))

psdDF <- udpipe_annotate(udmodel_english, textDF$text) %>% data.frame()

glimpse(psDF)

txt_freq(psDF$upos) %>% 
    ggplot(aes(x=reorder(key, freq), y = freq)) +
    geom_col(fill ="green") + 
    coord_flip() +
    theme_minimal()

nouns <- psDF %>% filter(upos %in% c("NOUN")) %>% 
    pull(token) %>% txt_freq()
nouns %>% 
    slice(1:20) %>% 
    ggplot(aes(x=reorder(key, freq), y = freq)) +
    geom_col(fill ="purple") + 
    coord_flip() +
    theme_minimal()
verbs <- psDF %>% filter(upos == "VERB") %>% 
    pull(token) %>% txt_freq()

stats <- keywords_rake(x = psDF, term = "lemma", group = "doc_id",
                       relevant = psDF$upos %in% c("NOUN", "ADJ"))


psDF$phrase_tag <- as_phrasemachine(psDF$upos, type = "upos")

phrases <- keywords_phrases(x = psDF$phrase_tag, term = psDF$lemma,
                            pattern = "(A|N)*N(P+D*(A|N)*N)*",
                            is_regex = TRUE,
                            detailed = FALSE) %>% 
    subset(ngram > 1 & freq > 3)
phrases %>% 
    ggplot(aes(x=reorder(keyword, freq), y = freq)) +
    geom_col(fill ="navy") + 
    coord_flip() +
    theme_minimal()

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

tidy_articles %>% distinct(title)
tidy_articles %>% filter(title == "WeWork Wants to Build the ‘Future of Cities.’ What Does That Mean?") %>% 
    inner_join(get_sentiments("nrc")) %>% 
    count(sentiment, sort = TRUE)

articleSentiments <- tidy_articles %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(title, index = section, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive-negative)

articleSentiments

articleSentiments %>% ggplot(aes(index, sentiment, fill = title)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~title, ncol=2, scales = "free_x") + theme_minimal()



