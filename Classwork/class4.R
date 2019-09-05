install.packages('tidyverse')
library('tidyverse')
# class 4
movies <- read.csv("movie_metadata.csv", stringsAsFactors = FALSE)
dim(movies)


#filter
movies_eng <- movies %>% filter(language == "English")

# select
movies_sub <- movies %>% select(movie_title, gross, imdb_score, budget, director_name)
head(movies_sub)

movies_sub %>% slice(1:10)

# mutate
movies_sub <- movies_sub %>% mutate(budgetM = budget / 1000000,
                                    grossM = gross / 1000000,
                                    profitM = grossM - budgetM,
                                    budget_log = log(budget))
head(movies_sub)


hist(movies_sub$budgetM)

# %<>% saving back to the same object
library('magrittr')
movies_sub %<>% filter(budgetM < 400)
hist(movies_sub$budgetM)

# rename
movies_sub %<>% rename(director = director_name,
                       title = movie_title)

# group by
Director_Avg <- movies_sub %>% group_by(director) %>% 
  summarize(gross_Avg_Dir = mean(grossM, na.rm = TRUE),
            profit_Avg_Dir = mean(profitM, na.rm = TRUE)) %>% 
  arrange(desc(profit_Avg_Dir))
Director_Avg

movies_sub %>% filter(director == "Tim Miller")
