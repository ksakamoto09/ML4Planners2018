## Missing Data

z <- c(1, 2, NA, 8, 3, NA, 5)
z

is.na(z)

mean(z)
mean(z, na.rm = TRUE)

zchar <- c("Jane Jacobs", NA, "Robert Moses")
zchar
is.na(zchar)

## NULL Values
y <- c(1, NULL, 3)
y

d <- NULL
is.null(d)
is.null(7)

# Lists
list(1,2,3)
list(c(1,2,3))
# naming lists
list1 <- list(num1 = c(1,2,3),
              num2 = 1:10)
list1

list1[[1]]
list1[[2]]
# or this way with named vectors
list1$num1
list1$num2

list1$num1[2]
list1$num2[3:5]

## Matrices
matrix(1:10, nrow = 5)
matrix(1:10, ncol = 5)

## Dataframes
ids <- 1:5
set.seed(123)
age <- round(runif(n = 5, min = 22, max = 30))
firstName <- c("Alex", "Peter", "Mary", "Beth", "Tom")

df <- data.frame(ids, age, firstName, stringsAsFactors = FALSE)
tbl1 <- tibble::tibble(ids, age, firstName)
df
nrow(df)
ncol(df)
dim(df)

## Packages
library(dplyr)
dplyr::glimpse(df)
str(df)

dplyr::glimpse(tbl1)

## Subsetting with Dataframes
df[1,]
df[,1]

df[1,2]
df[1:3,]
df[,c("age", "firstName")]

## Writing Functions
sayHello <- function(){
    print("Hello, World")
}

sayHello()

timesTwo <- function(x){
    return(x * 2)
}

timesTwo(3)

timesTwo <- function(x){
    x * 2
}
timesTwo(3)

timesX <- function(n, x){
    n * x
}

timesX(3, 2)

timesX <- function(n, x = 2){
    n * x
}

timesX(3)
timesX(3,7)

## Control Statments
if(2 > 1){
    print("This is true")
}

if(2 < 1){
    print("This is true")
}else{
    print("This is false")
}

checkName <- function(name){
    if(is.character(name)){
        sprintf("Hello %s", name)
    }else{
        print("I'm sorry that's not a string")
    }
}

checkName("Peter")
checkName(1)
checkName(firstName)

checkName2 <- function(name){
    if(is.character(name)){
        sprintf("Hello %s", name)
    }else if(is.numeric(name)){
        print("You entered a number")
    }else print("What are you doing?") # one line code doesn't need to be in {}
}

checkName2("Peter")
checkName2(1)
checkName2(NA)

ifelse(1==1, "Yes", "No")
x <- 2
ifelse(x == 1, "It's a 1", ifelse(x == 2, "it's a 2", "What is it?"))

## Switch
gsappCodeSwitch <- function(y){
    y <- stringr::str_to_lower(y)
    switch(y, 
           "up" = "Urban Planning",
           "arch" = "Architecture",
           "ud" = "Urban Design",
           "hp" = "Historic Preservation",
           "red" = "Real Estate Development",
           "Not A Program")
}

gsappCodeSwitch("UP")
gsappCodeSwitch("arch")
gsappCodeSwitch("mba")

## Loops
for(i in 1:10){
    print(i)
}
# while loops
x <- 1
while(x <= 10){
    print(x)
    x <- x + 1
}

## Controlling Loops
for(i in 1:10){
    if(i > 3 & i < 7){
        next
    }
    print(i)
}

x <- 1:10
x
x[!c(x > 3 & x < 7)]





