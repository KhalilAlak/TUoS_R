# IJC437/IJC445 Introduction to R (Part 1)

# 1. FIRST EXAMPLES IN R

2 + 3

x <- 2 + 3
x


# 2. FUNCTIONS IN R

myseq <- 1:10
myseq

sum(myseq)

myseq2 <- seq(1:10)
myseq2

sum(myseq)

help(sum)
?sum

sum(seq(1, 10))
sum(1:10)


# 3. CLASSES AND OPERATORS

class(3.14)

2 == 3
2 == 2
"Sam" == "Hilary"
"Sam" != "Hilary"

3 > 1 & 3 <= 4
3 > 5 | 3 <= 2


# 4. VECTORS: CREATE & ACCESS

x <- c(1.2, 2.3, 0.2, 1.1)
x

class(x)
length(x)
str(x)

x[1]
x[4]

# Exercise 1
x[10]

x[1:2]
x[c(1, 2)]

# Exercise 2
x[c(2, 3)]

x[-1]
x[-2]

x[1] <- 1.0
x


# 4.1 VECTORISED COMPARISONS

c(3, 4, 6, 7, -2, -1) > 1
x > 2
x > 1 & x <= 4
x > 1 | x <= 4

x[x > 2]


# 4.2 VECTORISED OPERATIONS

x.doubled <- x * 2
x.doubled

# Exercise 3
x.squared <- x^2
x.squared


# 4.3 USEFUL FUNCTIONS FOR VECTORS

mean(x)
summary(x)

sort(x)
sort(x, decreasing = TRUE)

append(x, 5.2)
append(x, c(5.2, 7.3))

x <- append(x, c(5.2, 7.3))
x

# Exercise 4
mean(x)
length(x)
class(x)


# 4.4 CHARACTER VECTORS & NAMED VECTORS

name <- c("Tom", "Deep", "Harry")
name
str(name)

# Exercise 5
names(name) <- c("One", "Two", "Three")
name

name[1]
name["One"]

# Exercise 6
name[c(1, 3)]
name[c("One", "Three")]


# 4.5 DAYS PER MONTH – NAMED VECTOR

# Exercise 7
days_per_month <- c(
  Jan = 31,
  Feb = 28,
  Mar = 31,
  Apr = 30,
  May = 31,
  Jun = 30,
  Jul = 31,
  Aug = 31,
  Sep = 30,
  Oct = 31,
  Nov = 30,
  Dec = 31
)

days_per_month

days_in_year <- sum(days_per_month)
days_in_year

sum(days_per_month == 31)
sum(days_per_month == 30)

names(days_per_month[days_per_month == 30])


# 5. MATRICES: CREATE & ACCESS

A <- matrix(
  c(2, 4, 3, 1, 5, 7),
  nrow = 2,
  ncol = 3,
  byrow = TRUE
)
A

p <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
p

class(A)
dim(A)
