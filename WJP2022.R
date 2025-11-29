library(readxl)
library(httr)
library(ggplot2)
library(car)

# 1. Read database from GitHub
url <- "https://raw.githubusercontent.com/andreea-rmz/Impact-of-Corruption-on-the-Rule-of-Law-using-RStudio/main/data/WJP_2022.xlsx"

temp <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp, overwrite = TRUE))


WJP_2022 <- read_excel(temp)

# MLR
m1 <- lm(score ~ factor1 + factor2 + factor3 + factor4 +
           factor5 + factor6 + factor7 + factor8,
         data = WJP_2022)
summary(m1)

# Corr matrix
cor(WJP_2022)

# factor2 vs score
ggplot(WJP_2022, aes(x = factor2, y = score)) +
  geom_point() +
  geom_smooth(method = "lm")

# Hypothesis test
pt(6.936e+14, df = 131, lower.tail = FALSE)

# 6. VIF para multicolinealidad
vif(m1)
