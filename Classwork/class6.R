library('magrittr')
library('tidyverse')

movies <- read.csv("movie_metadata.csv")



movies %<>% mutate(grossM = gross / 10e6,
                   budgetM = budget / 10e6) %>% 
filter(budgetM < 400)


movies %<>% filter(!director_name == "Woody Allen")
table(movies$director_name)
library(forcats)
director_simple <- fct_lump(movies$director_name, n = 2)
levels(director_simple)

fct_lump(movies$director_name, n = 30) %>% table()
movies %<>% mutate(director_factor = 
                     fct_lump(director_name, n = 20))

table(movies$director_factor)

mod1 <- lm(grossM ~ budgetM + imdb_score + 
             relevel(director_factor, ref = "Other"), 
           data = movies)
summary(mod1)

install.packages('coefplot')
library(coefplot)
coefplot(mod1, sort = "magnitude")


mod2 <- lm(grossM ~ log(budgetM) + imdb_score, 
           data = movies)
summary(mod2)

mod3 <- lm(log(grossM) ~ log(budgetM) + imdb_score, 
           data = movies)
summary(mod3)

mod4 <- lm(log(grossM) ~ budgetM + imdb_score, 
           data = movies)
summary(mod4)


ggplot(movies, aes(x = imdb_score, y = grossM))+ geom_point()

mod5 <- lm(grossM ~ budgetM + imdb_score + I(imdb_score^2), 
           data = movies)

summary(mod5)

install.packages('margins')
library('margins')
m <- margins(mod5, at = list(imdb_score = 1:10))
cplot(m, x = "imdb_score", what = "prediction")