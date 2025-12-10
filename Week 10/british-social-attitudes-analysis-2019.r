library(tidyverse)




# 1. Load the tab-separated file
bsa <- read_tsv("bsa2019_poverty_open(1)-1.tab")





# 2. Explore the data
dim(bsa)
names(bsa)
head(bsa)
summary(bsa)
View(bsa)   

miss_codes <- c(-1, 8, 9, 97, 98, 99, 997, 998, 999)

bsa_na <- bsa |>
  mutate(across(
    where(is.numeric),
    ~ ifelse(.x %in% miss_codes, NA_real_, .x)
  ))

summary(bsa_na)

# 2. Remove all rows that have any NA in any column
bsa_complete <- bsa_na |> drop_na()

nrow(bsa)
nrow(bsa_complete)





#Exercise 3 – Age column (RAgeCat): clean & histogram
# 1. Work with the age column (RAgeCat)

bsa_age <- bsa_na |>
  filter(!is.na(RAgeCat))

n_removed_age <- nrow(bsa_na) - nrow(bsa_age)

n_removed_age

# 2. Plot histogram of age values
ggplot(bsa_age, aes(x = RAgeCat)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "RAgeCat (age categories)",
    y = "Count",
    title = "Distribution of Respondents' Age (RAgeCat)"
  ) +
  theme_minimal()





# Exercise 4: Re-scale age into 4 groups

bsa_age4 <- bsa_age |>          
  mutate(age4 = case_when(
    RAgeCat %in% c(1, 2)      ~ 1,   # 18–34
    RAgeCat %in% c(3, 4)      ~ 2,   # 35–54
    RAgeCat == 5              ~ 3,   # 55–64
    RAgeCat %in% c(6, 7)      ~ 4,   # 65+
    TRUE ~ NA_real_
  ))

# Plot histogram of new age groups
ggplot(bsa_age4, aes(x = factor(age4))) +
  geom_bar(fill = "steelblue") +
  labs(
    x = "Age group (1=18–34, 2=35–54, 3=55–64, 4=65+)",
    y = "Count",
    title = "Distribution of Rescaled Age Groups"
  ) +
  theme_minimal()





# EXERCISE 5 – Education vs Age (using age4)

# 1) Drop rows with missing age4 or missing education (HEdQual3)
bsa_age_edu <- bsa_age4 |>
  filter(!is.na(age4),
         !is.na(HEdQual3))

# 2) Create a contingency table (matrix) of age4 by HEdQual3
edu_age_table <- table(
  AgeGroup   = bsa_age_edu$age4,
  Education  = bsa_age_edu$HEdQual3
)

edu_age_table


prop.table(edu_age_table, margin = 1)

edu_age_df <- as.data.frame(edu_age_table) |>
  arrange(AgeGroup, Education)

head(edu_age_df)





#EXERCISE 6 – Chi-squared test + boxplot (Age vs Education)
# age4 and HEdQual3 present
bsa_edu_age <- bsa_age4 |>
  filter(!is.na(age4),
         !is.na(HEdQual3))

# 1) Contingency table age x education
age_edu_tab <- table(bsa_edu_age$age4, bsa_edu_age$HEdQual3)
age_edu_tab

# 2) Chi-squared test of association
chisq.test(age_edu_tab)

# 3) Boxplot: education (y) by age group (x)
ggplot(bsa_edu_age,
       aes(x = factor(age4), y = HEdQual3)) +
  geom_boxplot() +
  labs(
    x = "Age group (1=18–34, 2=35–54, 3=55–64, 4=65+)",
    y = "Education level (HEdQual3)"
  ) +
  theme_minimal()

#EXERCISE 7 – Linear regression: NatFrEst - politics + income

# To keep only rows with all variables needed
bsa_lin <- bsa_na |>
  filter(!is.na(NatFrEst),
         !is.na(eq_inc_quintiles),
         !is.na(libauth),
         !is.na(leftrigh),
         !is.na(welfare2),
         !is.na(Politics))

# 1) Full model: politics + income
model_full <- lm(
  NatFrEst ~ libauth + leftrigh + welfare2 + Politics + eq_inc_quintiles,
  data = bsa_lin
)

summary(model_full)

# 2) Politics-only model (without income)
model_politics <- lm(
  NatFrEst ~ libauth + leftrigh + welfare2,
  data = bsa_lin
)

summary(model_politics)





#EXERCISE 8 – Logistic Regression: Predict “Increase taxes & spend” (TaxSpend)

bsa_tax <- bsa_age4 |> 
  filter(!is.na(TaxSpend),
         TaxSpend %in% c(1, 2, 3))

bsa_tax <- bsa_tax |>
  mutate(TaxSpend_bin = if_else(TaxSpend == 1, 1, 0))

#Drop rows missing any predictor variables
bsa_tax <- bsa_tax |>
  filter(
    !is.na(libauth),
    !is.na(leftrigh),
    !is.na(welfare2),
    !is.na(Politics),
    !is.na(age4)
  )

#Shuffle & Split (70% train, 30% test)
set.seed(123)

bsa_tax <- bsa_tax |> sample_frac(1)

n <- nrow(bsa_tax)
train_n <- floor(0.7 * n)

train <- bsa_tax[1:train_n, ]
test  <- bsa_tax[(train_n + 1):n, ]

#Fit the logistic regression model
model_log <- glm(
  TaxSpend_bin ~ libauth + leftrigh + welfare2 + Politics + age4,
  data = train,
  family = binomial(link = "logit")
)

summary(model_log)

#Predict on test set & compute accuracy
pred_prob <- predict(model_log, newdata = test, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

accuracy <- mean(pred_class == test$TaxSpend_bin)
accuracy

#The logistic regression model shows that attitudes toward welfare (welfare2) and political engagement (Politics) significantly predict whether respondents believe taxes and spending should be increased. Age is also a strong negative predictor, suggesting older people are less supportive of increasing taxes and public spending. Other political orientation variables (libauth, leftrigh) are not significant once welfare attitudes are included. The model performs extremely well overall, achieving 95.8% accuracy, which strongly supports the client’s hypothesis that political and demographic factors influence TaxSpend opinions.






#EXERCISE 9 – Heatmap: SMNews × PartyId2 x Spend1
#Heatmap: SMNews × PartyId2
# 1. Filter valid rows
bsa_h1 <- bsa_na |>
  filter(!is.na(SMNews),
         !is.na(PartyId2))

# 2. Make contingency table in tidy form
heat1 <- bsa_h1 |>
  count(SMNews, PartyId2)

# 3. Plot heatmap
ggplot(heat1, aes(x = factor(PartyId2),
                  y = factor(SMNews),
                  fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    x = "PartyId2",
    y = "SMNews (Social Media News Consumption)",
    fill = "Count"
  ) +
  theme_minimal()



#Heatmap: SMNews × Spend1
# 1. Filter valid rows
bsa_h2 <- bsa_na |>
  filter(!is.na(SMNews),
         !is.na(Spend1))

# 2. Make tidy frequency table
heat2 <- bsa_h2 |>
  count(SMNews, Spend1)

# 3. Plot heatmap
ggplot(heat2, aes(x = factor(Spend1),
                  y = factor(SMNews),
                  fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    x = "Spend1 (Government Spending Priorities)",
    y = "SMNews",
    fill = "Count"
  ) +
  theme_minimal()





#EXERCISE 10 — Correlation Matrix
vars <- c("TaxSpend", "SocTrust", "MuchPov", "Poverty1", "Poverty2", "PartyId2")

bsa_corr <- bsa_na |>
  select(all_of(vars)) |>
  drop_na()

cor_matrix <- cor(bsa_corr)
cor_matrix

install.packages("corrplot")
library(corrplot)

corrplot(
  cor_matrix,
  method = "color",
  type   = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)



#Execise 11
# During this practical, I learned how complex and realistic a large social survey dataset, such as the British Social Attitudes (BSA 2019) survey, can be. The Codebook and User Guide were essential because many variables used non-standard missing-value codes, and understanding these was critical before any analysis could begin. I also realised how unevenly different variables contain missing data: for example, age and sex were almost complete, while variables like social media news consumption and spending priorities had thousands of missing values. Working through the exercises showed me that cleaning, recoding, and grouping variables, especially transforming the age categories into four meaningful bandsare crucial steps that directly shape any conclusions that can be drawn from the data.
# I also learned how each statistical technique answers different questions for the client. The chi-square test helped identify an association between age and education, while linear regression showed how political orientation and income relate to perceptions of benefit fraud. Logistic regression allowed me to build a predictive model for attitudes towards tax and spending, and evaluating accuracy demonstrated how well the model performed. Creating heatmaps and correlation matrices helped reveal broader patterns in political attitudes and social media behaviour. Overall, this practical made me appreciate how real-world data science requires a combination of technical cleaning, statistical reasoning, and clear communication of insights to support policy decisions.

