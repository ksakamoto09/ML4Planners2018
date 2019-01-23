### Basic Math

1 + 1
1 + 2 + 3
3 * 7 * 2
4 / 2
4 / 3

### PEMDAS
4 * 6 + 5
(4 * 6) + 5
4 * (6 + 5)

### Variables
x <- 2
x
y = 5

3 -> z
z

a <- b <- 7
assign("j", 4)
j <- 4


### Removing Variables
rm(j)
j

## Case Sensitive
theVariable <- 17
thevariable
theVariable

## Data Types
### Numeric
class(x)
is.numeric(x)

### Integers
i <- 5L
i
is.integer(i)
is.numeric(i)
is.integer(x)

### Interger Division
4L * 2.8
class(4L * 2.8)

class(5L / 2L)

### Characater Data
s <- "data"
s
is.character(s)
is.character(x)

nchar(s)
nchar("hello")
nchar(3)
nchar(452)

### Dates
date1 <- as.Date("2019-01-22")
date1
class(date1)
as.numeric(date1)

date2 <- as.POSIXct("2019-01-22 08:50")
date2
class(date2)
as.numeric(date2)

## Logicals

TRUE
TRUE * 5
FALSE * 5

2 == 3
2 != 3
2 < 3
2 <= 3 


## Vectors
x <- c(1,2,3,4,5)
x
x * 3
x + 2
x^2
sqrt(x)

y <- 6:10
1:2000000

-2:4
x + y
x^y
length(x)
x < y

## character Vectors
q <- c("Arch", "UP", "UD")
q
nchar(q)


## Vector Subsetting
q[1]
q[1:2]
q[c(1,3)]


## sum
sum(x)
?mean
apropos("mea")

