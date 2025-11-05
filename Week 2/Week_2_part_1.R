t <- seq(from=0, to=10, by=0.1) 
y <- sin(t)
plot(y)
plot(x=t, y=y, type="l")
plot(x=t, y=y, type="l", xlab="Angle", ylab="Sine", main="Sine function")
plot(x=t, y=y, type="l", lty="dashed", xlab="Angle", ylab="Sine", col="blue", main="Sine function")
plot(x=t, y=y, pch=2, xlab="Angle", ylab="Sine", col="red", main="Sine function")
plot(x=t, y=y, type="l", lty="dashed", lwd = 3, xlab="Angle", ylab="Sine", col="blue", main="Sine function")
points(x=t, y=y, pch=2, xlab="Angle", ylab="Sine", col="red", main="Sine function")
plot(x = t, 
       +      y = y_sin, 
       +      type = "p",                         
       +      pch = 17,                           
       +      col = "red",
       +      ylim = c(-1, 1),
       +      xlab = "Time (t)",
       +      ylab = "Value",
       +      main = "Sine and Cosine Functions (Points)")
points(x = t, 
         +        y = y_cos, 
         +        pch = 16,                           
         +        col = "blue")
library(WDI)
new_wdi_cache <- WDIcache()
WDIsearch("GDP per capita", cache = new_wdi_cache)
gdp_capita[1,1]
gdp_capita[30,4]
gdp_capita[,4]
gdp_capita[12,]
gdp_capita[1:5, 1:3]
gdp_capita[c(1,3,7), c(2,4)] 
gdp_capita[gdp_capita$country=="China",] 
gdp_capita[gdp_capita$year>2018,] 
spain <- gdp_capita[gdp_capita$country=="Spain",] 
plot(x = uk_gdp$year, 
      y = uk_gdp$NY.GDP.PCAP.KD, 
      type = "l",                                 
      col = "blue",                               
      lwd = 2,                                    
      main = "UK GDP Per Capita (Constant 2015 US$)", 
      xlab = "Year",                              
      ylab = "GDP Per Capita (US$)") 

View(gdp_capita)
uk = United Kingdom

uk_gdp <- subset(gdp_capita, country == "United Kingdom")
View(gdp_capita)
plot(x = uk_gdp$year,
      y = uk_gdp$NY.GDP.PCAP.KD,
      type = "l",                                 
      col = "blue",                               
      lwd = 2,                                    
      main = "UK GDP Per Capita (Constant 2015 US$)", 
      xlab = "Year",                              
      ylab = "GDP Per Capita (US$)")              

uk_data <- gdp_capita[gdp_capita$country == "United Kingdom", ]
uae_data <- gdp_capita[gdp_capita$country == "United Arab Emirates", ]
plot(x = uk_data$year, 
       y = uk_data$NY.GDP.PCAP.KD, 
       type = "l",
       +      col = "blue",
       +      main = "GDP Comparison: UK vs UAE",
       +      xlab = "Year",
       +      ylab = "GDP Per Capita (US$)",
       +      ylim = c(40000, 50000))
lines(x = uae_data$year, 
        +       y = uae_data$NY.GDP.PCAP.KD, 
        +       type = "l",
        +       col = "red")
legend("topright", 
         +        legend = c("UK", "UAE"), 
         +        col = c("blue", "red"), 
         +        lty = 1)
uk_data <- gdp_capita[gdp_capita$country == "United Kingdom", ]
gdp_current_uk <- WDI(country = "GB",
                        +                       indicator = "NY.GDP.PCAP.CD",
                        +                       start = 2012, end = 2022)
plot(x = uk_data$year, 
       +      y = uk_data$NY.GDP.PCAP.KD, 
       +      type = "l",
       +      col = "purple",
       +      main = "UK GDP: Constant vs Current US$",
       +      xlab = "Year",
       +      ylab = "GDP Per Capita",
       +      ylim = c(38000, 50000))
lines(x = gdp_current_uk$year, 
        +       y = gdp_current_uk$NY.GDP.PCAP.CD,
        +       type = "l",
        +       col = "orange")
legend("bottomright", 
         +        legend = c("Constant $", "Current $"), 
         +        col = c("purple", "orange"), 
         +        lty = 1)
uk<-gdp_capita[gdp_capita$country=="United Kingdom",]
write.table(uk, "UK GDP per capita 2012 2022.csv", sep=",", row.names=FALSE)
uk_copy<-read.csv("UK GDP per capita 2012 2022.csv", header=TRUE)	
View(uk_copy)
              
uk <- WDI(country="GB",
                          + indicator = "NY.GDP.PCAP.KD", start = 2012, end = 2022, cache = new_wdi_cache)
uae <- WDI(country="AE",
                           +            indicator = "NY.GDP.PCAP.KD", start = 2012, end = 2022, cache = new_wdi_cache)
uk_uae <- WDI(country=c("AE","GB"),
                              +               indicator = "NY.GDP.PCAP.KD", start = 2012, end = 2022, cache = new_wdi_cache)
us <- WDI(country="US",
                          +           indicator = c("NY.GDP.PCAP.KD","NY.GDP.PCAP.CD"),
                          +           cache = new_wdi_cache)
View(us)
us_better_names <- WDI(country="US",
                                       +                        indicator = c("GDP_pc_constant_dollar"="NY.GDP.PCAP.KD", 
                                                                              +                                      "GDP_pc_current_dollar"="NY.GDP.PCAP.CD"),
                                       +                        cache = new_wdi_cache)
us_better_names <- WDI(country="US",
                                       +                        indicator = c("GDP_pc_constant_dollar"="NY.GDP.PCAP.KD", 
                                                                              +                                      "GDP_pc_current_dollar"="NY.GDP.PCAP.CD"),
                                       +                        extra=TRUE,
                                       +                        cache = new_wdi_cache)
us_gdp <- WDI(country = "US",
                              +               indicator = c("GDP_pc_constant" = "NY.GDP.PCAP.KD", 
                                                            +                             "GDP_pc_current" = "NY.GDP.PCAP.CD"),
                              +               cache = new_wdi_cache)
plot(x = us_gdp$GDP_pc_constant,
                     +      y = us_gdp$GDP_pc_current,
                     +      main = "US GDP: Current vs. Constant Dollars",
                     +      xlab = "GDP Per Capita (Constant 2015 US$)", 
                     +      ylab = "GDP Per Capita (Current US$)",
                     +      pch = 19,                                     
                     +      col = "darkblue") 
y_axis_range <- range(c(us_gdp$GDP_pc_constant, us_gdp$GDP_pc_current), na.rm = TRUE)
plot(x = us_gdp$year, 
                     +      y = us_gdp$GDP_pc_constant,
                     +      type = "l",                                    
                     +      col = "blue",                                  
                     +      lwd = 2,                                       
                     +      ylim = y_axis_range,                           
                     +      main = "US GDP Per Capita Over Time",
                     +      xlab = "Year",
                     +      ylab = "GDP Per Capita (US$)")
lines(x = us_gdp$year, 
                      +       y = us_gdp$GDP_pc_current,
                      +       type = "l",
                      +       col = "red",                                  
                      +       lwd = 2)
legend("topleft", 
                       +        legend = c("Constant 2015 Dollars", "Current Dollars"), 
                       +        col = c("blue", "red"),
                       +        lty = 1, 
                       +        lwd = 2)
t <- seq(from=0, to=10, by=0.1) 
y <- sin(t)
plot(y)