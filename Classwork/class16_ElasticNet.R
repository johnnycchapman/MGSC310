#------------------------------------------------
### Load and Clean Data
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
                              cast_total_facebook_likes / 1000) %>%   
  drop_na() %>% select(-c(director_name,actor_1_name,
                          actor_2_name,actor_3_name,
                          plot_keywords,movie_imdb_link,
                          country,budgetM,grossM, genres,
                          language, movie_title, budget, gross))

train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies %>% slice(train_idx)
movies_test <- movies %>% slice(-train_idx)



alpha_grid <- seq(0, 1, length = 5)
alpha_grid

enet_mod <- cva.glmnet(profitM ~ .,
                       data = movies_train,
                       alpha = alpha_grid)


summary(enet_mod)


enet_mod$modlist[[2]]


minlossplot(enet_mod)

# 3rd model where alpha = 0.5
plot(enet_mod$modlist[[3]])


coef(enet_mod$modlist[[3]])

# lasso coefficients
coef(enet_mod$modlist[[5]])

coef(enet_mod$modlist[[5]], s = 
     enet_mod$modlist[[5]]$lambda.min)


enet_coef <- data.frame(
  varname = rownames(coef(enet_mod$modlist[[1]])),
  ridge = as.matrix(coef(enet_mod$modlist[[1]])) %>% round(3), # round to three places
  enet_025 = as.matrix(coef(enet_mod$modlist[[2]])) %>% round(3),
  enet_05 = as.matrix(coef(enet_mod$modlist[[3]])) %>% round(3),
  enet_075 = as.matrix(coef(enet_mod$modlist[[4]])) %>% round(3),
  lasso = as.matrix(coef(enet_mod$modlist[[5]])) %>% round(3)
) %>% remove_rownames() %>% 
  rename(varname = 1, ridge = 2, enet_025 = 3, enet_05 = 4, enet_075 = 5, lasso = 6)


enet_coef


#------------------------------------------------
### ElasticNet
#------------------------------------------------
alpha_list <- seq(0,1,len = 5)
alpha_list

enet_fit <- cva.glmnet(profitM ~ . ,
                       data = movies_train,
                       alpha = alpha_list)

enet_fit

minlossplot(enet_fit)

plot(enet_fit)

# look at each individual model
plot(enet_fit$modlist[[4]])

# view coefficients
coef(enet_fit, alpha = 0.75) %>% 
  round(3)

# if we want the lambda.min version
coef(enet_fit, alpha = 0.75, 
     s = enet_fit$modlist[[4]]$lambda.min)

coef(enet_fit, alpha = 1) %>% 
  round(3)

enet_coefs <- data.frame(
  varname = rownames(coef(enet_fit,alpha = 0)),
  ridge = as.matrix(coef(enet_fit, alpha = 0)) %>% round(3),
  alpha025 = as.matrix(coef(enet_fit, alpha = 0.25)) %>% round(3),
  alpha05 = as.matrix(coef(enet_fit, alpha = 0.5)) %>% round(3),
  alpha075 = as.matrix(coef(enet_fit, alpha = 0.75)) %>% round(3),
  lasso = as.matrix(coef(enet_fit, alpha = 1)) %>% round(3)
) %>% rename(varname = 1, ridge = 2, alpha025 = 3, alpha05 = 4, alpha075 = 5, lasso = 6) %>% 
  remove_rownames()

enet_coefs

## post lasso
library('hdm')
postLasso <- rlasso(profitM ~ . , 
                    data = movies_train, 
                    post = TRUE)
summary(postLasso)

postLasso_coef <- data.frame(
  varname = rownames(postLasso$coefficients %>% data.frame()),  
    postLasso = postLasso$coefficients %>% data.frame() %>% round(3)
) %>% remove_rownames() %>% rename(varname = 1, postLasso = 2)


enet_coefs <- left_join(enet_coefs, postLasso_coef, by = "varname")
enet_coefs

