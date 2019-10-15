# class15_Lasso

#------------------------------------------------
### Lasso Regression
#------------------------------------------------
library(glmnet)
library(glmnetUtils)

# load the movies dataset
library('tidyverse')
options(scipen = 50)
set.seed(1861)
movies <- read.csv("Datasets/movie_metadata.csv")
movies <- movies %>% filter(budget < 400000000) %>% 
  filter(content_rating != "",
         content_rating != "Not Rated",
         !is.na(gross)) 
movies <- movies %>% 
  mutate(genre_main = unlist(map(strsplit(as.character(movies$genres),"\\|"),1)),
         grossM = gross / 1000000,
         budgetM = budget / 1000000,
         profitM = grossM - budgetM)
movies <- movies %>% mutate(genre_main = fct_lump(genre_main,5),
                            content_rating = fct_lump(content_rating,3),
                            country = fct_lump(country,2),
                            cast_total_facebook_likes000s = 
                              cast_total_facebook_likes / 1000) %>%   drop_na()

train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies %>% slice(train_idx)
movies_test <- movies %>% slice(-train_idx)



# Lasso model
lasso_mod <- 
  cv.glmnet(profitM ~ ., 
            data = movies_train %>% 
              select(-c(director_name,actor_1_name,
                        actor_2_name,actor_3_name,
                        plot_keywords,movie_imdb_link,
                        country,budgetM,grossM, genres,
                        language, movie_title, budget, gross)),
            alpha = 1)

lasso_mod$lambda.1se
lasso_mod$lambda.min


coef(lasso_mod, lasso_mod$lambda.1se)
coef(lasso_mod, lasso_mod$lambda.min)


coef_mat <- data.frame(
  rownames = rownames(coef(lasso_mod)) %>% data.frame(),
  coef_1se = as.matrix(coef(lasso_mod, lasso_mod$lambda.1se)) %>% round(3)
) %>% remove_rownames() %>% 
  rename(rownames = 1,
         coef_1se = 2)

coef_mat

rownames(coef_mat)

library(coefplot)
coefpath(lasso_mod)

# estimate Lasso mod 
Lasso_mod <- 
  
  cv.glmnet(profitM ~ ., 
  data = movies_train %>% 
  
    select(-c(director_name,actor_1_name,
  actor_2_name,actor_3_name,
  plot_keywords,movie_imdb_link,
  country,budgetM,grossM, genres,
  language, movie_title, budget, gross)),
  
  alpha = 1)

coef(Lasso_mod, 
     s = Lasso_mod$lambda.min)

coef(Lasso_mod, 
     s = Lasso_mod$lambda.1se)

# put in a matrix
coef_mat <- data.frame(
  varname = rownames(coef(Lasso_mod)) %>% 
            data.frame(),
  Lasso_min = as.matrix(coef(Lasso_mod, 
                   s = Lasso_mod$lambda.min)) %>% 
              round(3),
  Lasso_1se = as.matrix(coef(Lasso_mod, 
                   s = Lasso_mod$lambda.1se)) %>% 
              round(3)
) %>% rename(varname = 1, 
             Lasso_min = 2, 
             Lasso_1se = 3)  %>% 
      remove_rownames()

coef_mat

plot(Lasso_mod)

# place in one coef


# explore how coefficients 
# change as we change lambda
library(coefplot)
coefpath(Lasso_mod)


