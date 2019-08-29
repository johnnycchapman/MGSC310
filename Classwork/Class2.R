# install packages
install.packages('tidyverse')
library('tidyverse')

data(mpg)
help(mpg) # ?mpg

names(mpg)

view(mpg)

dim(mpg)

summary(mpg)

#create factor
mpg$class_factor <- as.factor(mpg$class)

table(mpg$class_factor)


unique(mpg$class_factor)

# plotting

# Add themes
install.packages('ggthemes')
library('ggthemes')

p <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + 
  labs(x = "Engine Displacement (Liters)", 
       y = "Mile Per Gallon (Highway)",
       title = "Engine Size and Miles Per Gallon") +
  theme_minimal()

# saves last plot
ggsave("Engine_Size_MPG_Scatter.pdf", 
       width = 6, height = 5)

getwd()

p <- p + geom_smooth(method = "lm")
p # or plot(p)


# facet wrap
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = factor(cyl))) + 
  labs(x = "Engine Displacement (Liters)", 
       y = "Mile Per Gallon (Highway)",
       title = "Engine Size and Miles Per Gallon") +
  facet_wrap(~class, scales = "free") + 
  geom_smooth() + 
  theme_minimal()

ggsave("MPG_by_Displacement_Facet_Class.pdf", 
       width = 8, height = 6)
