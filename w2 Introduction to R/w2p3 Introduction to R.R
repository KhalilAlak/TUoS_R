# IJC437/IJC445 Introduction to R (Part 3)

# 1. BASIC PLOTTING

t <- seq(from = 0, to = 10, by = 0.1)
y <- sin(t)

plot(y)

plot(x = t, y = y, type = "l")

plot(
  x = t, y = y, type = "l",
  xlab = "Angle", ylab = "Sine",
  main = "Sine function"
)

plot(
  x = t, y = y, type = "l",
  lty = "dashed", col = "blue",
  xlab = "Angle", ylab = "Sine",
  main = "Sine function"
)

plot(
  x = t, y = y, pch = 2,
  col = "red",
  xlab = "Angle", ylab = "Sine",
  main = "Sine function"
)

plot(
  x = t, y = y, type = "l",
  lty = "dashed", lwd = 3,
  col = "blue",
  xlab = "Angle", ylab = "Sine",
  main = "Sine function"
)
points(x = t, y = y, pch = 2, col = "red")

# Exercise 1: Plot sine and cosine together
t <- seq(0, 10, 0.1)
y_sin <- sin(t)
y_cos <- cos(t)

plot(
  t, y_sin, type = "l",
  col = "blue", lwd = 2,
  xlab = "Angle", ylab = "Value",
  main = "Sine and Cosine"
)
lines(t, y_cos, col = "red", lty = "dashed", lwd = 2)


# 2. PACKAGES IN R

# install.packages("WDI")
library(WDI)


# 3. USING WDI PACKAGE

new_wdi_cache <- WDIcache()

WDIsearch("GDP per capita", cache = new_wdi_cache)

gdp_capita <- WDI(
  country   = "all",
  indicator = "NY.GDP.PCAP.KD",
  start     = 2012,
  end       = 2022,
  cache     = new_wdi_cache
)

# View(gdp_capita)


# 3.2 ACCESSING DATA FRAME

gdp_capita$year
gdp_capita[1, 1]
gdp_capita[30, 4]
gdp_capita[, 4]
gdp_capita[12, ]
gdp_capita[1:5, 1:3]
gdp_capita[c(1, 3, 7), c(2, 4)]

gdp_capita[gdp_capita$country == "China", ]
gdp_capita[gdp_capita$year > 2018, ]

spain <- gdp_capita[gdp_capita$country == "Spain", ]


# 4. GDP PER CAPITA VISUALISATION

uk  <- gdp_capita[gdp_capita$country == "United Kingdom", ]
uae <- gdp_capita[gdp_capita$country == "United Arab Emirates", ]

# Exercise 2: UK GDP per capita line plot
plot(
  uk$year, uk$NY.GDP.PCAP.KD,
  type = "l"
)

# Exercise 3: Add labels and title
plot(
  uk$year, uk$NY.GDP.PCAP.KD,
  type = "l",
  col = "blue", lwd = 2,
  xlab = "Year",
  ylab = "GDP per capita (constant 2015 USD)",
  main = "GDP per capita of the UK (2012–2022)"
)

# Exercise 4: Add UAE line
plot(
  uk$year, uk$NY.GDP.PCAP.KD,
  type = "l",
  col = "blue", lwd = 2,
  xlab = "Year",
  ylab = "GDP per capita (constant 2015 USD)",
  main = "GDP per capita: UK vs UAE"
)

lines(
  uae$year, uae$NY.GDP.PCAP.KD,
  col = "red", lty = "dashed", lwd = 2
)

legend(
  "topleft",
  legend = c("United Kingdom", "United Arab Emirates"),
  col    = c("blue", "red"),
  lty    = c("solid", "dashed"),
  lwd    = 2,
  bty    = "n"
)
