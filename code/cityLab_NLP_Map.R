## Text Scraping
library(dplyr)
library(ggplot2)
library(tidytext)
library(rvest)

homePage <- "https://www.citylab.com/"

topStories <- read_html(homePage) %>% 
    html_nodes(css = "a.c-most-popular__link") %>% 
    html_attr(name = "href")

scrapeArticle <- function(url){
    title <- read_html(url) %>% 
        html_nodes("article h1") %>% 
        html_text()
    
    text <- read_html(url) %>% 
        html_nodes("article") %>% 
        html_nodes(".s-article__section p") %>% 
        html_text()
   
    author <- read_html(url) %>% 
        html_nodes(xpath = '//*[@id="main-content"]/article/div[2]/div/ol/li/span/a') %>% 
        html_text()
    
    tibble(title, text, author, section = 1:length(text))
}


textDF <- topStories %>% 
    as.list() %>% purrr::map_df(~scrapeArticle(.x))

tidy_articles <- textDF %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

tidy_articles %>%
    count(title, word, sort = TRUE) %>%
    filter(n > 10) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = title)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "none") + 
    facet_wrap(~title, scales = "free")


## Sentiment Analysis

###nrc 
nrcDF <- tidy_articles %>% select(-section) %>% tidyr::nest(-c(title, author)) %>% 
    dplyr::mutate(combine = purrr::map(data, function(x)x %>% inner_join(get_sentiments("nrc")) %>% 
                                           count(sentiment, sort = TRUE))) %>% 
    select(-data) %>% 
    tidyr::unnest(combine)

nrcDF %>%
    ggplot(aes(sentiment, n, fill = title)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "none") + 
    facet_wrap(~title, scales = "free")

#### afinn
afinnDF <- tidy_articles  %>% tidyr::nest(-c(title, author, section)) %>% 
    dplyr::mutate(combine = purrr::map(data, function(x)x %>% inner_join(get_sentiments("afinn")) %>% 
                                           summarise(score = sum(score)))) %>% 
    select(-data) %>% 
    tidyr::unnest(combine)

afinnDF %>% ggplot(aes(section, score, fill = title)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~title, ncol = 2, scales = "free_x") +
    theme_minimal()

### bing

bingDF <- tidy_articles %>%
    group_by(title, author, section) %>% 
    inner_join(get_sentiments("bing")) %>% 
    count( sentiment) %>% 
    tidyr::spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive-negative) %>% ungroup()

bingDF %>% ggplot(aes(section, sentiment, fill = title)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~title, ncol = 2, scales = "free_x") +
    theme_minimal()

textDF %>% group_by(title, author) %>% 
    summarise(text = paste(text, collapse = " ")) %>% 
    ungroup() %>% unnest_tokens(sentence, text, token = "sentences")

textDF %>% group_by(title, author) %>% 
    summarise(text = paste(text, collapse = " ")) %>% 
    ungroup() %>% 
    sentimentr::get_sentences(text) %>% sentimentr::sentiment() %>% 
    as_tibble() %>% 
    group_by(title) %>% 
    summarise(sumSentiment = sum(sentiment),
              avgsent = mean(sentiment),
              medsent = median(sentiment),
              minsent = min(sentiment),
              maxsent = max(sentiment),
              sdsent = sd(sentiment),
              sentences = n(),
              wordcount = sum(word_count))

## Topic Modelling
library(MITIE)

ner_model_path <- "/usr/local/Cellar/MITIE-models/english/ner_model.dat"
ner <- NamedEntityExtractor$new(ner_model_path)

# Print out what kind of tags this tagger can predict

tag_names <- ner$get_possible_ner_tags()

textDF %>% group_by(title, section) %>% 
    summarise(text = paste(text, collapse = " ")) %>% 
    ungroup() %>%
    tidyr::nest(text) %>% 
    dplyr::mutate(tokens = purrr::map(data, function(x)mitie_tokenize(x$text))) %>% 
    dplyr::mutate(entities = purrr::map(tokens, function(x) ner$extract_entities(x))) %>% 
    dplyr::mutate(locations = purrr::map2(entities, tokens, function(x, y)tagsMap(x, y))) %>% 
    select(-tokens, -entities) %>% 
    tidyr::unnest(locations)-> test

test$locations[[1]]
    
tokens <- mitie_tokenize("On October 4, 2017, 
                         a motorist struck and killed an 18-year-old competitive cyclist named 
                         Clément Ouimet in Montreal, Quebec. The crash occurred on Camillien Houde Parkway, 
                         a limited-access road that traces the northern edge of the Mount Royal Park, 
                         a 500-acre artificial wilderness in the middle of the 
                         city that’s home to Montreal’s namesake dwarf mountain.")
tokens <- mitie_tokenize("We’ve written our fair share of travel guides around here. 
                         There are stateside adventures, like Atlanta, Berkeley, and 
                         Portland. Or, abroad ones, from Tulum, Mexico to Lisbon, Portugal.")


entities <- ner$extract_entities(tokens)

tagsFunc <- function(entity, tokens){
    position = paste("(", entity$start, ",", entity$end, ")", sep="")
    text = paste(tokens[entity$start:entity$end], collapse=" ")
    #data.frame(text = text)
    data.frame(text = text, tag = entity$tag)
}

tagsMap <- function(entities, tokens)purrr::map_df(entities %>% as.list(), ~tagsFunc(.x, tokens))
locationDF <- DF %>% filter(tag == "LOCATION")


##  Geocoding
library(RCurl)
query <- "Montreal"
key <- "AIzaSyD36e_IOZi6KQjuuy_RMOtId0svtjo6GJk"
locationSearch <- function(query, key){
    query <- stringr::str_replace_all(query, pattern = " ", replacement = "%20")
    # this creates the api call
    url <- sprintf("https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input=%s&inputtype=textquery&fields=geometry&key=%s",
                   query, key)
    ## This reads in the url
    output <- getURL(url)
    ## Read in json and return the results object from the list
    outDF <- jsonlite::fromJSON(output)$candidates 
    query <- stringr::str_replace_all(query, pattern = "%20", replacement = " ")
    if(is.null(outDF$geometry$location)){
        outDF <- data.frame(lat = NA, lng = NA, search = query)
    }else{
        outDF <- outDF$geometry$location %>% mutate(search = query)
        
        }
    
    ## return outDF
    outDF
}


locGeo <- locationDF$text %>% as.list() %>%  purrr::map_df(~locationSearch(.x, key))


## Mapping

library(leaflet)
m <- leaflet(locGeo) %>% 
    setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircleMarkers(
        stroke = FALSE, fillOpacity = 0.5
    )
m
