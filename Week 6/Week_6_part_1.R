install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("ggplot")
install.packages("waffle", repos = "https://cinc.rud.is")  # For waffle charts
install.packages("treemapify")     # For tree maps
install.packages("ggthemes")       # Extra themes for ggplot2
install.packages("extrafont")      # Fonts for waffle charts
install.packages("DT")             # Dependency for waffle
install.packages("htmlwidgets")    # Dependency for waffle
library(ggplot2)

ggplot(mpg, aes(manufacturer)) + geom_bar()+
  labs(x="Manufacturer", y="Frequency",
       title="Number of cars released",
       caption="mpg dataset")

ggplot(mpg, aes(manufacturer)) + 
  geom_bar(width = 0.5) +
  labs(x = "Manufacturer", y = "Frequency", 
       title = "Number of cars released", 
       caption = "mpg dataset")

library(tidyverse)
library(dplyr)
df <- mpg %>% group_by(manufacturer) %>% summarise(count = n())
ggplot(df, aes(manufacturer, count)) + 
  geom_bar(stat = "identity") +
  labs(x = "Manufacturer", y = "Frequency", 
       title = "Number of cars released", 
       caption = "mpg dataset")

ggplot(mpg, aes(manufacturer)) + 
  geom_bar(aes(fill = drv)) +
  labs(x = "Manufacturer", y = "Frequency", fill = "Drive type",
       title = "Number of cars released", 
       caption = "mpg dataset")

ggplot(mpg, aes(manufacturer)) + 
  geom_bar(aes(fill = drv), position = position_dodge(preserve = "single")) +
  labs(x = "Manufacturer", y = "Frequency", fill = "Drive type",
       title = "Number of cars released", 
       caption = "mpg dataset")

ggplot(mpg, aes(manufacturer)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Manufacturer", y = "Frequency", 
       title = "Number of cars released", 
       caption = "mpg dataset")

ggplot(mpg, aes(manufacturer)) + 
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Manufacturer", y = "Frequency", 
       title = "Number of cars released", 
       caption = "mpg dataset")

ggplot(mpg, aes(factor(1), fill = factor(cyl))) + 
  geom_bar() +
  coord_polar(theta = "y") +
  theme(axis.line = element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "Number of cylinders",
       title = "Proportion of cars by cylinder number", 
       caption = "mpg dataset")

install.packages(c('DT', 'htmlwidgets', 'extrafont'))
install.packages("waffle", repos = "https://cinc.rud.is")
install.packages("devtools")
library("devtools")
install_github("hrbrmstr/waffle")

library(ggplot2)
library(dplyr)
library(waffle)

df<- mpg %>% group_by(cyl=factor(cyl)) %>% summarise(count=n())
ggplot(df, aes(fill=cyl, values=count)) + geom_waffle()

df<- mpg %>% group_by(cyl=factor(cyl)) %>% summarise(count=n())
ggplot(df, aes(fill=cyl, values=count)) + geom_waffle(n_rows=9, colour="white") +
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) + coord_equal() +
  labs(x=NULL, y=NULL, fill="Number of\ncylinders",
       title="Proportion of cars by cylinder number",
       caption="mpg dataset")

library(ggplot2)

ggplot(mpg, aes(x = factor(1), fill = fl)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Fuel Type Distribution (Pie Chart)", fill = "Fuel Type")

library(dplyr)
library(ggplot2)

fuel_counts <- mpg %>%
  count(fl) %>%
  mutate(tiles = round(n / sum(n) * 100)) 

waffle_data <- fuel_counts %>%
  uncount(tiles) %>%
  mutate(id = row_number(),
         row = (id - 1) %% 10 + 1,
         col = (id - 1) %/% 10 + 1)

ggplot(waffle_data, aes(x = col, y = row, fill = fl)) +
  geom_tile(color = "white") +
  coord_equal() +
  theme_void() +
  labs(title = "Fuel Type Distribution (Waffle Chart)", fill = "Fuel Type")

library(treemapify)
ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label=country)) +
  geom_treemap() + geom_treemap_text() +
  labs(title="Distribution of countries by GDP",
       fill="Human\ndevelopment\nindex",
       caption="G20 data")

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label=country)) +
  geom_treemap() + geom_treemap_text(fontface="italic", colour="white", place="centre") +
  labs(title="Distribution of countries by GDP",
       fill="Human\ndevelopment\nindex",
       caption="G20 data")

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label=country, subgroup = region)) +
  geom_treemap() + geom_treemap_text(fontface="italic", colour="white", place="centre") +
  geom_treemap_subgroup_border() + geom_treemap_subgroup_text() +
  labs(title="Distribution of countries by GDP",
       fill="Human\ndevelopment\nindex",
       caption="G20 data")
#Execise 4 

df <- mpg %>%
  group_by(manufacturer) %>%
  summarise(
    count = n(),
    avg_fuel = mean((cty + hwy) / 2)
  )
#End of Execise
install.packages("gridExtra")
library("gridExtra")
grid.arrange(size.plot, colour.plot, shape.plot, ncol = 1)
size.plot<-ggplot(mpg, aes(displ, cyl))+ geom_point(aes(size=cty)) +
  labs(x='Displacement', y='Cylinders', size='City mpg')
colour.plot<-ggplot(mpg, aes(displ, cyl))+ geom_point(aes(colour= hwy)) +
  labs(x='Displacement', y='Cylinders', colour='Highway\nmpg')
shape.plot<-ggplot(mpg, aes(displ, cyl))+ geom_point(aes(shape=fl)) +
  labs(x='Displacement', y='Cylinders', shape='Fuel type')
grid.arrange(size.plot, colour.plot, shape.plot, ncol=1)

library(ggplot2)
ggplot(mpg, aes(displ, cyl))+geom_point(size=6, colour=rgb(1,0,0,0.25))+
  labs(x="Displacement", y="Cylinders",
       title="Translucent colours can show data point density",
       caption="mpg dataset")
ggplot(mpg, aes(displ, cyl))+
  geom_point(size=2,
             position = position_jitter(w=0.05,h=0.05))+
  labs(x="Displacement", y="Cylinders",
       title="Random noise can also show data point density",
       caption="mpg dataset")

ggplot(mpg, aes(displ, cyl))+
  geom_point(size=1,
             position = position_jitter(w=0,h=0.15))+
  labs(x="Displacement", y="Cylinders",
       title="Noise can be added on just one axis",
       caption="mpg dataset")

ggplot(mpg, aes(cty, hwy))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="City", y="Highway", title="Comparing fuel economy",
       caption="mpg dataset")

#Execise 5
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  labs(
    x = "City MPG",
    y = "Highway MPG",
    title = "Fuel Economy: City vs Highway",
    caption = "mpg dataset"
  )

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(color = rgb(0, 1, 0, 0.25), size = 5) +
  labs(
    x = "City MPG",
    y = "Highway MPG",
    title = "Density via Translucent Green Points",
    caption = "mpg dataset"
  )
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.5), size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    x = "City MPG",
    y = "Highway MPG",
    title = "Jittered Points with Linear Fit",
    caption = "mpg dataset"
  )
#End of Execise

data("economics")
ggplot(economics, aes(x=date, y=unemploy))+geom_line()+
  labs(x="Date", y="Unemployed (in thousands)",
       title="Unemployment progression",
       caption="economics dataset")

data("economics")
ggplot(economics, aes(x=date, y=unemploy))+
  geom_line(colour="red", size=2)+
  labs(x="Date", y="Unemployed (in thousands)",
       title="Unemployment progression",
       caption="economics dataset")

data("economics")
ggplot(economics, aes(x=date, y=unemploy))+
  geom_line(aes(colour=pop), size=1)+
  labs(x="Date", y="Unemployed (in thousands)",
       title="Unemployment progression",
       caption="economics dataset",
       colour="Population\n(in thousands)")

ggplot(economics, aes(x=date))+
  geom_line(aes(y=unemploy))+
  geom_line(aes(y=pop))+
  labs(x="Date", y="Numbers (in thousands)",
       title="Unemployment progression",
       caption="economics dataset")+
  scale_y_continuous(trans = "log",
                     breaks=c(10**3, 10**4, 10**5, 10**6))

ggplot(economics, aes(x=date))+
  geom_line(aes(y=unemploy, col="Unemployed"))+
  geom_line(aes(y=pop, col="Total population"))+
  labs(x="Date", y="Numbers (in thousands)",
       title="Unemployment progression",
       caption="economics dataset")+
  scale_y_continuous(trans = "log",
                     breaks=c(10**3, 10**4, 10**5, 10**6))+
  scale_colour_manual(name="Legend",
                      values=c("Unemployed"="red",
                               "Total population"="blue"))

#Execise 6
library(ggplot2)

data("economics")  # Load the dataset

ggplot(economics, aes(x = date, y = psavert, colour = pce)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Personal Savings Rate",
    colour = "Personal Consumption\nExpenditures",
    title = "Savings Rate Over Time (Colored by Consumption)",
    caption = "economics dataset"
  )

economics <- economics %>%
  mutate(unemp_pct = (unemploy / pop) * 100)

library(dplyr)
library(tidyverse)
data("economics")
economics <- economics %>%
  mutate(unemp_pct = (unemploy / pop) * 100)

ggplot(economics, aes(x = date, y = unemp_pct)) +
  geom_line(color = "darkred", size = 1) +
  labs(
    x = "Date",
    y = "Unemployment (% of Population)",
    title = "Unemployment Percentage Over Time",
    caption = "economics dataset"
  )
head(economics)
#End of Execise

hist1<-ggplot(mpg, aes(cty))+
  geom_histogram(bins=10, fill="lightgreen")+ labs(x="Fuel economy in the city",
                                                   y="Frequency", caption="mpg dataset", title='Bins')
hist2<-ggplot(mpg, aes(cty))+
  geom_histogram(binwidth=5, fill="lightblue")+ labs(x="Fuel economy in the city",
                                                     y="Frequency", caption="mpg dataset", title='Binwidth')
hist3<-ggplot(mpg, aes(cty))+
  geom_histogram(breaks=c(5,10,15,20,25,30,35,40), fill="lightcoral")+
  labs(x="Fuel economy in the city", y="Frequency",
       caption="mpg dataset", title='Breaks')
grid.arrange(hist1, hist2, hist3, ncol=1)

#Execise 7
ggplot(mpg, aes(x = displ)) +
  geom_histogram(breaks = c(1, 3, 5, 7), fill = "skyblue", color = "black") +
  labs(
    title = "Histogram with Custom Bin Boundaries",
    x = "Engine Displacement",
    y = "Frequency",
    caption = "mpg dataset"
  )
ggplot(mpg, aes(x = displ)) +
  geom_histogram(bins = 15, fill = "lightgreen", color = "black") +
  labs(
    title = "Histogram with 15 Bins",
    x = "Engine Displacement",
    y = "Frequency",
    caption = "mpg dataset"
  )
ggplot(mpg, aes(x = displ)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  labs(
    title = "Histogram with Bin Width = 1",
    x = "Engine Displacement",
    y = "Frequency",
    caption = "mpg dataset"
  )
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_bin2d(binwidth = c(2, 2)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "2D Histogram of City vs Highway MPG",
    x = "City MPG",
    y = "Highway MPG",
    fill = "Count",
    caption = "mpg dataset"
  )
#End of Execise

ggplot(mpg, aes(class, cty)) +
  geom_boxplot(varwidth=TRUE, fill="plum") +
  labs(title="Fuel economy in city grouped by Class of vehicle",
       caption="mpg dataset",
       x="Class of Vehicle",
       y="City Mileage")

#Execise 8
ggplot(mpg, aes(x = factor(cyl), y = displ)) +
  geom_boxplot(varwidth = TRUE, fill = "lightblue") +
  labs(
    title = "Engine Displacement by Cylinder Count",
    x = "Number of Cylinders",
    y = "Engine Displacement",
    caption = "mpg dataset"
  )

ggplot(mpg, aes(x = factor(cyl), y = displ)) +
  geom_violin(fill = "orchid", trim = FALSE) +
  labs(
    title = "Engine Displacement by Cylinder Count (Violin Plot)",
    x = "Number of Cylinders",
    y = "Engine Displacement",
    caption = "mpg dataset"
  )

ggplot(mpg, aes(x = factor(cyl), y = displ)) +
  geom_violin(fill = "lightgray", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "blue", outlier.shape = NA) +
  labs(
    title = "Combined Violin and Boxplot",
    x = "Number of Cylinders",
    y = "Engine Displacement",
    caption = "mpg dataset"
  )
#End of Execise

#Execise 9
ggplot(diamonds, aes(x = carat, y = price, color = cut, size = clarity)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Diamond Price vs Carat",
    x = "Carat",
    y = "Price (USD)",
    color = "Cut",
    size = "Clarity"
  ) +
  theme_minimal()

ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
  geom_boxplot() +
  labs(
    title = "Price Distribution by Cut",
    x = "Cut Quality",
    y = "Price (USD)",
    fill = "Cut"
  ) +
  theme_minimal()
