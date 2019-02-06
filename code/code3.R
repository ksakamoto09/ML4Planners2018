## Reading in csvs

library(readr)

## reading in csv from local dir
subway <- read_csv("Data/subway.csv")

## reading in csv from online
subwayonline <- read_csv("http://ml-course.kazsakamoto.com/Data/subway.csv")

head(subway)

## Reading in Excel

library(readxl)
## Getting the name of the sheets
subwaySheet <- excel_sheets("Data/subway.xlsx")
subwaySheet
# you can pull sheets by name
subwayXL <- read_excel("Data/subway.xlsx", sheet = "part1")
head(subwayXL)
# you can also pull sheets by index number
subwayXL <- read_excel("Data/subway.xlsx", sheet = 1)
# you can also pull sheets from a vector 
subwayXL <- read_excel("Data/subway.xlsx", sheet = subwaySheet[1])

## Pipes
library(magrittr)

head(subway)

subway %>% head()

head(subway, n=4)

subway %>% head(n=4)

tail(head(subway, n=5), n=1)

subway %>% head(n=5) %>% tail(n=1)

## Data manipulation with dplyr

library(dplyr)
## Select
subway[,c("Lat", "Long")]

subway %>% select(Lat, Long)

subway %>% select(5, 4)

subway %>% select(-c(5, 4))

## Filter
subway[subway$LINE  == 'SIR',]

subway %>% filter(LINE == 'SIR')

subway %>% filter(LINE == 'SIR', Long < -74.18)
subway %>% filter(LINE == 'SIR' & Long < -74.18)
## Or statment

subway %>% filter(LINE == 'SIR' | Long > -74.18)

subway %>% filter(LINE %in% c("SIR", "6"))
# dealing with NAs
subway %>% filter(!is.na(NAME))

 ## Slice
subway[1:5,]
subway %>% slice(1:5)

subway %>% slice(c(1:5, 10:15))

## mutate
subway %>% mutate(pass / stairs)
subway %>% mutate(ratio = pass/stairs)
## so far we haven't been saving any of our results
subway %<>% mutate(ratio = pass/stairs)
subway <- subway %>% mutate(ratio = pass/stairs)

head(subway)

subway %>% mutate(passLog = log(pass), twiceLog = passLog * 2)

## summarize
subway %>% summarize(mean(pass))
subway %>% summarize(avgPass = mean(pass))
mean(subway$pass) %>% typeof

subway %>% summarize(avgPass = mean(pass), 
                     medPass = median(pass),
                     sumPass = sum(pass),
                     minPass = min(pass),
                     maxPass = max(pass))

summaryFunc <- function(df, colName){
    # use summarize_at to select colName which is a string
    # the funcstions to run on it
    out <- df %>% summarize_at(.vars = colName,
                        .funs = c(mean, median, sum, min, max))
    # rename colmns
    names <- c("avg", "med", "sum", "min", "max")
    colnames(out) <- paste0(names, colName)
    # return output
    out
}

summaryFunc(subway, "pass")

subway %>% summarize_at(.vars = "pass",  .funs = c(min, max ), .cols = )
## group by
subway %>% group_by(borough) %>% 
    summarize(avgPass = mean(pass))

subway %>% group_by(borough) %>% 
    summarize(avgPass = mean(pass),
              medPass = median(pass),
              sumPass = sum(pass),
              minPass = min(pass),
              maxPass = max(pass))

subway %>% group_by(borough, stairs) %>% 
    summarize(avgPass = mean(pass),
              medPass = median(pass),
              sumPass = sum(pass),
              minPass = min(pass),
              maxPass = max(pass),
              count = n()) 

subwayWide <- subway %>% group_by(borough, stairs) %>% 
    summarize(AvgPass = mean(pass),
              count = n(),
              totPass = sum(pass))

head(subwayWide)

library(tidyr)
subwayLong <- subwayWide %>% gather(key = variable,
                                    value = value,
                                    -borough, -stairs)
subwayLong %>% View()

subwayLong %>% spread(key = variable, value  = value)

## Unite
subwayCoord <- subway %>% unite(Coord, Lat, Long, sep = ", ")

subwayCoord %>% separate(Coord, c("Lat", "Long"), sep = ", ") %>% 
    mutate(Lat = as.numeric(Lat),
           Long = as.numeric(Long))

subwayCoord %>% separate(Coord, c("Lat", "Long"), sep = ", ") %>% 
    mutate_if(~starts_with("La"), ~as.numeric(.x))
subway %>% select(Lat,Long) %>% 
    mutate_if(is.numeric, funs(as.character))

subway %>% mutate_at(vars(starts_with("La")), funs(as.character))
## arrange
subway %>% arrange(pass)
subway %>% arrange(desc(pass))
subway %>% arrange(desc(pass), stairs)

## sprintf

sprintf("Hello, %s, it's going to be %s today", "David", "sunny")
name <- "Sam"
forecast <- "rainy"
sprintf("Hello, %s, it's going to be %s today", name, forecast)

## string manipulation

library(stringr)
str_split(subway$NAME, pattern = " & ")

str_sub(subway$NAME, start=1, end=5)

str_detect(subway$NAME, pattern = "Ave")
subway %>% filter(str_detect(NAME, pattern = "Birchall"))

subway %>% filter(str_detect(NAME, pattern = "[r,R]"))

subway %>% filter(str_detect(NAME, pattern = "^[r,R]"))

subway %>% filter(str_detect(borough, pattern = "[d]$"))

subway %>% filter(str_detect(NAME, pattern = "[[::punct::]]"))

subway %>% filter(str_detect(NAME, pattern = "\\."))

subway %>% filter(str_detect(NAME, pattern = "\\s[E][\\s|\\.]"))

subway %>% filter(str_detect(NAME, pattern = "\\s[0-9][\\s|\\.]"))
subway %>% mutate(replacement = str_replace(LINE, pattern = "\\d",
                                            replacement = "x"))
subway %>% mutate(replacement = str_replace_all(LINE, pattern = "\\d",
                                            replacement = "x"))

## rvest
library(rvest)

wbSite <- read_html("http://wdi.worldbank.org/table/WV.1")
econSize <- wbSite %>% 
    html_nodes(xpath = '//*[@id="scrollTable"]') %>% 
    html_table() %>% "[["(1)
head(econSize)

wbColNames1 <- wbSite %>% html_nodes(".level0") %>% 
    html_nodes("th") %>% 
    html_text() %>% str_trim()

wbColNames2 <- wbSite %>% html_nodes(".level1") %>% 
    html_nodes("th") %>% 
    html_text() %>% str_trim()
wbColNames3 <- wbSite %>% html_nodes(".level2") %>% 
    html_nodes("th") %>% 
    html_text() %>% str_trim()
wbColNames <- paste(wbColNames1, wbColNames2, wbColNames3)
wbColNames[1] <- "Country"
colnames(econSize) <- wbColNames

View(econSize)
