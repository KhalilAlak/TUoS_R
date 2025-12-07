# IJC437/IJC445 Introduction to R (Part 2)

# 1. LISTS

a <- list("a", 1, c(4, 5, 6))
a

str(a)

a[[1]]
a[[3]]

a[c(1, 3)]

names(a) <- c("One", "Two", "Three")
a

a[["One"]]
a$One

# Exercise 1: Select first and third by name
a[c("One", "Three")]

a <- list(One = "a", Two = 1, Three = c(4, 5, 6))

b <- list(
  text = "Data Science is cool",
  sequence = 1:10,
  data = iris
)
b

b$data[1, ]


# 2. DATA FRAMES

medals <- data.frame(
  Country = c("USA", "GBR", "CHN"),
  Gold    = c(46, 27, 26),
  Silver  = c(37, 23, 18),
  Bronze  = c(38, 17, 26)
)

medals

medals$Gold
medals[[2]]

medals["Gold"]
class(medals["Gold"])

medals[2]
class(medals[2])

medals[c("Country", "Gold")]
medals[c(1, 2)]

medals[1, ]

# Exercise 2: Select first and third rows
medals[c(1, 3), ]

# Exercise 3: Select rows 1 & 3 and columns 1 & 3
medals[c(1, 3), c(1, 3)]


# 2.1 SELECTING ROWS AND COLUMNS (OPTION 1)

medals[medals$Country == "GBR", ]

medals[medals$Country == "CHN" | medals$Country == "GBR", ]

medals[medals$Gold >= 27, ]


# 2.2 SELECTING WITH subset()

subset(medals, select = Gold)
subset(medals, select = c(Country, Gold))

subset(medals, subset = (Country == "USA"))

subset(
  medals,
  select = c(Country, Gold),
  subset = (Country == "USA")
)

# Exercise 4: Countries with ≥27 gold medals
subset(
  medals,
  select = c(Country, Gold),
  subset = (Gold >= 27)
)


# 2.3 ADDING ROWS

newCountry <- data.frame(Country = "RUS", Gold = 19, Silver = 18, Bronze = 19)
medals <- rbind(medals, newCountry)
medals

# Exercise 5: Add Germany and Japan

newCountry <- data.frame(Country = "GER", Gold = 17, Silver = 10, Bronze = 15)
medals <- rbind(medals, newCountry)

newCountry <- data.frame(Country = "JPN", Gold = 12, Silver = 8, Bronze = 21)
medals <- rbind(medals, newCountry)

medals


# 2.4 ADDING COLUMNS

medals$Total <- medals$Gold + medals$Silver + medals$Bronze
medals

colSums(medals[, c("Gold", "Silver", "Bronze")])
rowSums(medals[, c("Gold", "Silver", "Bronze")])
