# Analysis

library(tidyverse)
library(tidycensus)
source("C:/Projects/r/soc-course-project/scripts/data.r")

d <- get_data()

dd <- mutate(d, pct_nbnw = 1 - pct_white - pct_black)

df <- filter(d, pct_car0 > 0.4)

g <- ggplot(data = df) +
  geom_point(mapping = aes(x = income_est, y = walkability)) +
  geom_smooth(mapping = aes(x = income_est, y = walkability), method = lm)
# print(g)

g2 <- ggplot(data = df) + 
  geom_point(mapping = aes(x = pct_black, y = walkability)) +
  geom_smooth(mapping = aes(x = pct_black, y = walkability), method = lm)
# print(g2)

g3 <- ggplot(data = df) + 
  geom_point(mapping = aes(x = pct_col, y = walkability)) +
  geom_smooth(mapping = aes(x = pct_col, y = walkability), method = lm)
print(g3)

fit <- lm(walkability ~
            income_est +
            pct_white +
            pct_black +
            pct_car0 +
            pct_col,
          data = dd)