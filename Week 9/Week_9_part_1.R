install.packages("GGally")
install.packages("ggradar")
install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE, force = TRUE)


library(ggplot2)
library(WDI)
library(MASS)
library(GGally)
library(ggradar)
library(scales)
library(tidyverse)

new_wdi_cache <- WDIcache()

education <- WDI(
  indicator = c(
    "SE.PRM.ENRR",
    "SE.SEC.ENRR",
    "SE.TER.ENRR",
    "SE.SEC.PROG.ZS",
    "SE.PRM.CMPT.ZS"
  ),
  start = 2014,
  end = 2014,
  extra = TRUE,
  cache = new_wdi_cache
)

# Remove Aggregate rows + missing rows
education <- education[education$region != "Aggregates", ]
education <- na.omit(education)


# PCA
pca <- prcomp(education[, 7:11])

education.pca <- data.frame(
  country = education$country,
  region = education$region,
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2]
)

ggplot(education.pca, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs(title = "PCA of education data", caption = "World Bank")

ggplot(education.pca, aes(PC1, PC2)) +
  geom_point(aes(colour = region), size = 3) +
  labs(title = "PCA of education data", caption = "World Bank", colour = "Region")

education.loading <- data.frame(
  dimensions = colnames(education)[7:11],
  PC1 = pca$rotation[, 1],
  PC2 = pca$rotation[, 2]
)

ggplot(education.loading, aes(PC1, PC2, label = dimensions)) +
  geom_text(size = 3) +
  labs(title = "Loading plot of education data", caption = "World Bank")

# MDS
scaled.data <- scale(education[, 7:11])
distance.matrix <- dist(scaled.data)
mds <- isoMDS(distance.matrix)

education.mds <- data.frame(
  country = education$country,
  region = education$region,
  MDS1 = mds$points[, 1],
  MDS2 = mds$points[, 2]
)

ggplot(education.mds, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(title = "MDS of education data", caption = "World Bank")

distance.matrix2 <- dist(scaled.data, "canberra")
mds2 <- isoMDS(distance.matrix2)

education.mds2 <- data.frame(
  country = education$country,
  region = education$region,
  MDS1 = mds2$points[, 1],
  MDS2 = mds2$points[, 2]
)

ggplot(education.mds2, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(title = "MDS with Canberra distance", caption = "World Bank")

# PARALLEL COORDINATES
ggparcoord(
  education,
  columns = c(7, 8, 9, 10, 11),
  groupColumn = 12
) +
  labs(colour = "Region", x = "Variables", y = "Scaled values",
       title = "Parallel coordinates of education data", caption = "World Bank")

# Rotated x-axis labels
ggparcoord(
  education,
  columns = c(7, 8, 9, 10, 11),
  groupColumn = 12
) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(colour = "Region", x = "Variables", y = "Scaled values",
       title = "Parallel coordinates of education data", caption = "World Bank")

# Final improved version with vertical lines
ggparcoord(
  education,
  columns = c(7, 8, 9, 10, 11),
  groupColumn = 12
) +
  theme(axis.text.x = element_text(angle = 45),
        panel.background = element_blank()) +
  geom_vline(xintercept = c(1, 2, 3, 4, 5)) +
  labs(colour = "Region", x = "Variables", y = "Scaled values",
       title = "Parallel coordinates of education data", caption = "World Bank")

# SPIDER CHART

education$year <- NULL

education %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_region = str_replace_all(region, " ", "_")) %>%
  group_by(new_region) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar()

# Improved spider chart
education %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_region = str_replace_all(region, " ", "_")) %>%
  group_by(new_region) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 3, legend.text.size = 8)



wdi_cache <- WDIcache()

# Exercise 1.1

labour_indicators <- c(
  "SL.TLF.CACT.ZS",
  "SL.AGR.EMPL.ZS",
  "SL.IND.EMPL.ZS",
  "SL.SRV.EMPL.ZS",
  "SL.EMP.TOTL.SP.ZS",
  "SL.UEM.TOTL.ZS"
)

labour <- WDI(
  indicator = labour_indicators,
  start = 2019,
  end = 2019,
  extra = TRUE,
  cache = wdi_cache
)

labour <- labour[labour$region != "Aggregates", ]
labour <- na.omit(labour)

labour_indicator_cols_idx <- which(colnames(labour) %in% labour_indicators)


labour_pca <- prcomp(labour[, labour_indicator_cols_idx])

labour_pca_df <- data.frame(
  country = labour$country,
  region  = labour$region,
  PC1     = labour_pca$x[, 1],
  PC2     = labour_pca$x[, 2]
)

# Exercise 1.1 
ggplot(labour_pca_df, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs(
    title   = "Exercise 1.1 - PCA of labour data (text labels)",
    caption = "World Bank"
  )

# Exercise 1.1
ggplot(labour_pca_df, aes(PC1, PC2)) +
  geom_point(aes(colour = region), size = 3) +
  labs(
    title   = "Exercise 1.1 - PCA of labour data (coloured by region)",
    caption = "World Bank",
    colour  = "Region"
  )

# Exercise 1.1 
labour_pca_scaled <- prcomp(labour[, labour_indicator_cols_idx], scale. = TRUE)

labour_pca_scaled_df <- data.frame(
  country = labour$country,
  region  = labour$region,
  PC1     = labour_pca_scaled$x[, 1],
  PC2     = labour_pca_scaled$x[, 2]
)

ggplot(labour_pca_scaled_df, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs(
    title   = "Exercise 1.1 (Extra) - Scaled PCA of labour data",
    caption = "World Bank"
  )

# Exercise 2.1

labour_scaled <- scale(labour[, labour_indicator_cols_idx])

labour_dist_euclidean <- dist(labour_scaled)
labour_mds_euclidean  <- isoMDS(labour_dist_euclidean)

labour_mds_euclidean_df <- data.frame(
  country = labour$country,
  region  = labour$region,
  MDS1    = labour_mds_euclidean$points[, 1],
  MDS2    = labour_mds_euclidean$points[, 2]
)

ggplot(labour_mds_euclidean_df, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(
    title   = "Exercise 2.1 - MDS of labour data (Euclidean)",
    caption = "World Bank"
  )

# Exercise 2.1 
labour_dist_canberra <- dist(labour_scaled, method = "canberra")
labour_mds_canberra  <- isoMDS(labour_dist_canberra)

labour_mds_canberra_df <- data.frame(
  country = labour$country,
  region  = labour$region,
  MDS1    = labour_mds_canberra$points[, 1],
  MDS2    = labour_mds_canberra$points[, 2]
)

ggplot(labour_mds_canberra_df, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(
    title   = "Exercise 2.1 (Extra) - MDS of labour data (Canberra)",
    caption = "World Bank"
  )

# Exercise 2.1 
labour_dist_manhattan <- dist(labour_scaled, method = "manhattan")
labour_mds_manhattan  <- isoMDS(labour_dist_manhattan)

labour_mds_manhattan_df <- data.frame(
  country = labour$country,
  region  = labour$region,
  MDS1    = labour_mds_manhattan$points[, 1],
  MDS2    = labour_mds_manhattan$points[, 2]
)

ggplot(labour_mds_manhattan_df, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(
    title   = "Exercise 2.1 (Extra) - MDS of labour data (Manhattan)",
    caption = "World Bank"
  )

# Exercise 2.1
labour_dist_minkowski <- dist(labour_scaled, method = "minkowski", p = 3)
labour_mds_minkowski  <- isoMDS(labour_dist_minkowski)

labour_mds_minkowski_df <- data.frame(
  country = labour$country,
  region  = labour$region,
  MDS1    = labour_mds_minkowski$points[, 1],
  MDS2    = labour_mds_minkowski$points[, 2]
)

ggplot(labour_mds_minkowski_df, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(
    title   = "Exercise 2.1 (Extra) - MDS of labour data (Minkowski p=3)",
    caption = "World Bank"
  )

# Exercise 3.1

labour_indicator_cols_idx <- which(colnames(labour) %in% labour_indicators)
labour_income_col_idx     <- which(colnames(labour) == "income")

# Exercise 3.1
ggparcoord(
  labour,
  columns     = labour_indicator_cols_idx,
  groupColumn = labour_income_col_idx
) +
  labs(
    colour  = "Income status",
    x       = "Variables",
    y       = "Scaled values",
    title   = "Exercise 3.1 - Parallel coordinates of labour data",
    caption = "World Bank"
  )

ggparcoord(
  labour,
  columns     = labour_indicator_cols_idx,
  groupColumn = labour_income_col_idx
) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(
    colour  = "Income status",
    x       = "Variables",
    y       = "Scaled values",
    title   = "Exercise 3.1 - Parallel coordinates of labour data (rotated labels)",
    caption = "World Bank"
  )

ggparcoord(
  labour,
  columns     = labour_indicator_cols_idx,
  groupColumn = labour_income_col_idx
) +
  theme(
    axis.text.x      = element_text(angle = 45),
    panel.background = element_blank()
  ) +
  geom_vline(xintercept = seq_along(labour_indicator_cols_idx)) +
  labs(
    colour  = "Income status",
    x       = "Variables",
    y       = "Scaled values",
    title   = "Exercise 3.1 - Parallel coordinates of labour data (styled)",
    caption = "World Bank"
  )

# Exercise 4.1

labour_spider <- labour
labour_spider$year <- NULL

# Exercise 4.1
labour_spider %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_income = str_replace_all(income, " ", "_")) %>%
  group_by(new_income) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar()

# Exercise 4.1
labour_spider %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_income = str_replace_all(income, " ", "_")) %>%
  group_by(new_income) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(
    axis.label.size  = 3,
    legend.text.size = 8
  )

# Exercise 5

population_indicators <- c(
  "SP.POP.TOTL",
  "SP.POP.GROW",
  "SP.DYN.LE00.IN",
  "SP.DYN.TFRT.IN"
)

population <- WDI(
  indicator = population_indicators,
  start     = 2019,
  end       = 2019,
  extra     = TRUE,
  cache     = wdi_cache
)

population <- population[population$region != "Aggregates", ]
population <- na.omit(population)

population_indicator_cols_idx <- which(colnames(population) %in% population_indicators)

pop_pca <- prcomp(population[, population_indicator_cols_idx], scale. = TRUE)

pop_pca_df <- data.frame(
  country = population$country,
  region  = population$region,
  PC1     = pop_pca$x[, 1],
  PC2     = pop_pca$x[, 2]
)

ggplot(pop_pca_df, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs(
    title   = "Exercise 5 - PCA of population dynamics (countries)",
    caption = "World Bank"
  )

ggplot(pop_pca_df, aes(PC1, PC2)) +
  geom_point(aes(colour = region), size = 3) +
  labs(
    title   = "Exercise 5 - PCA of population dynamics (by region)",
    caption = "World Bank",
    colour  = "Region"
  )

population_spider <- population
population_spider$year <- NULL

population_spider %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_region = str_replace_all(region, " ", "_")) %>%
  group_by(new_region) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(
    axis.label.size  = 3,
    legend.text.size = 8
  )
