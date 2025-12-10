library(WDI)
library(tidyverse)
new_wdi_cache <-WDIcache()
country_data <-WDI(country=c("GB","FR","ES","IT","NL", "CN","AE","IN","JO","US"),
                   indicator =c("NY.GDP.PCAP.KD","NY.GDP.PCAP.KD.ZG",
                                "SP.POP.TOTL","SP.DYN.LE00.IN"),
                   start = 2000, end = 2019,
                   extra=TRUE,
                   cache = new_wdi_cache)
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(colour = "#FF0000") +
  labs(x="Year", y="GDP Per Capita Growth (annual %)")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(colour = rgb(0.8,0.2,0.5)) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(aes(colour =country)) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Country")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(aes(colour =income)) +
  scale_colour_brewer(palette='Dark2') +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Income\nlevel")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =income)) +
  scale_colour_brewer(palette='Blues') +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Income\nlevel")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_gradient(low='blue', high='#FF0000') +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_gradient2(midpoint=70, low='blue',
                         mid='yellow', high='red' )+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_gradientn(colours=c('red','green','blue','purple','yellow'))+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =income)) +
  scale_colour_viridis_d() +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Income\nlevel")
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy",
       title='Test title', subtitle = 'Test subtitle')
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy",
       title='Test title', subtitle = 'Test subtitle',
       caption='World Development Indicators, World Bank')
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy",
       title='Test title', subtitle = 'Test subtitle',
       caption='World Development Indicators, World Bank',
       tag='A')
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(. ~ income) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Country",
       title='Faceting test', subtitle = 'Facet on the X axis',
       caption='World Development Indicators, World Bank')
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ .) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       title='Faceting test', subtitle = 'Facet on the Y axis',
       colour="Country",
       caption='World Development Indicators, World Bank')
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ income) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       title='Faceting test', subtitle = 'Facet on both axes',
       colour="Country",
       caption='World Development Indicators, World Bank')
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ income) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       title='Faceting test', subtitle = 'Facet on both axes',
       colour="Country",
       caption='World Development Indicators, World Bank')+
  scale_x_continuous(breaks=seq(2000,2020,10))
filename<-"test_image.png"
ggsave(filename)

#Exercise 1.1: Representing a Single Colour
ggplot(country_data, aes(x = year, y = SP.DYN.LE00.IN)) +
  geom_point(colour = "steelblue") +
  labs(x = "Year", y = "Life Expectancy (years)")

#Exercise 2.1: Applying Different Colour Scales
ggplot(country_data, aes(x = NY.GDP.PCAP.KD, y = SP.DYN.LE00.IN)) +
  geom_point(aes(colour = SP.POP.TOTL)) +
  scale_colour_distiller(palette = "Oranges", direction = 1) + # direction=1 makes light-to-dark
  labs(
    x = "GDP Per Capita (constant 2015 US$)",
    y = "Life Expectancy (years)",
    colour = "Population"
  )
#Exercise 3.1: Adding Descriptive Titles
ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG)) +
  geom_point(aes(colour = SP.DYN.LE00.IN)) +
  scale_colour_viridis_c() +
  labs(
    x = "Year",
    y = "GDP Per Capita Growth (annual %)",
    colour = "Life\nExpectancy",
    title = "GDP Growth vs. Life Expectancy for Selected Countries",
    subtitle = "Annual GDP per capita growth between 2000 and 2019.\nColour indicates life expectancy at birth for that year.",
    caption = "Data: World Development Indicators, World Bank. Plot by Gemini."
  )
