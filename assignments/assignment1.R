if (!require(htmltab)) install.packages('htmltab')
library(htmltab
        )
## urls to read in
url1 <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
url2 <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita"

# take the second table in population page
pop <- htmltab(url1,2)

# tale the second table in GDP page
gdp <- htmltab(url2,2)

# rename pop columns to PopRank, Country, Population, Date, PopPercent, Source

# rename gdp columns to gdpRank, Country, GDP

# take all the "-" in the Rank columns in both tables and fill down with the value 
# from the row above

# clean the country names and join gdp and pop together

#
