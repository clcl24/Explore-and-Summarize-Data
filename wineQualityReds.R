# Load packages
library(ggplot2)
library(gridExtra)
library(GGally)

# Load the Data
rwq <- read.csv('wineQualityReds.csv')
str(rwq)

rwq$X <- NULL

summary(rwq)

grid.arrange(qplot(rwq$fixed.acidity),
             qplot(rwq$volatile.acidity),
             qplot(rwq$citric.acid),
             qplot(rwq$residual.sugar),
             qplot(rwq$chlorides),
             qplot(rwq$free.sulfur.dioxide),
             qplot(rwq$total.sulfur.dioxide),
             qplot(rwq$density),
             qplot(rwq$pH),
             qplot(rwq$sulphates),
             qplot(rwq$alcohol),
             qplot(rwq$quality),
             ncol = 3)

p1 <- ggplot(data = rwq, aes(x = fixed.acidity)) + 
  geom_histogram() + 
  scale_x_log10(breaks = (0: 15))

p2 <- ggplot(data = rwq, aes(x = volatile.acidity)) + 
  geom_histogram() + 
  scale_x_log10(breaks = seq(0.1, 1.5, 0.2))

p3 <- ggplot(data = rwq, aes(x = total.sulfur.dioxide)) + 
  geom_histogram() + 
  scale_x_log10(breaks = seq(0, 150, 20))

p4 <- ggplot(data = rwq, aes(x = sulphates)) + 
  geom_histogram() + 
  scale_x_log10(breaks = seq(0.1, 2, 0.2))

grid.arrange(p1, p2, p3, p4, ncol = 1)

rwq$rating <- ifelse(rwq$quality < 5, 'bad', ifelse(
  rwq$quality < 8, 'average', 'good'))
rwq$rating <- ordered(rwq$rating,
                     levels = c('bad', 'average', 'good'))
summary(rwq$rating)

rwq$quality <- factor(rwq$quality, ordered = T)
qplot(data = rwq, rating, fill = quality, 
      xlab = "Rating", ylab = "count", main = 'Distribution of Red Wine by Rating' )

rwq$quality <- as.numeric(rwq$quality)
ggcorr(rwq, geom = "circle", nbreaks = 5)

rwq$quality <- factor(rwq$quality, ordered = T)
ggplot(data = rwq, aes(x = quality, y = fixed.acidity, fill = rating)) +
  geom_boxplot() +
  ggtitle('Fixed Acidity in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Fixed Acidity (g / dm^3)')

ggplot(data = rwq, aes(x = quality, y = volatile.acidity, fill = rating)) +
  geom_boxplot() +
  ggtitle('volatile Acidity in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Volatile Acidity (g / dm^3)')

ggplot(data = rwq, aes(x = quality, y = citric.acid, fill = rating)) +
  geom_boxplot() +
  ggtitle('Citric Acid in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Citric Acid (g / dm^3)')

ggplot(data = rwq, aes(x = quality, y = chlorides, fill = rating)) +
  geom_boxplot() +
  ggtitle('Chlorides in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Chlorides (g / dm^3)')

ggplot(data = rwq, aes(x = quality, y = total.sulfur.dioxide, fill = rating)) +
  geom_boxplot() +
  ggtitle('Total Sulfur Dioxide in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Total Sulfur Dioxide (mg / dm^3)')

ggplot(data = rwq, aes(x = quality, y = density, fill = rating)) +
  geom_boxplot() +
  ggtitle('Density in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Density (g / cm^3)')

ggplot(data = rwq, aes(x = quality, y = sulphates, fill = rating)) +
  geom_boxplot() +
  ggtitle('Sulphates Levels in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Sulphates (g / dm^3)')

ggplot(data = rwq, aes(x = quality, y = alcohol, fill = rating)) +
  geom_boxplot() +
  ggtitle('Alcohol Levels in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Alcohol (% volume)')

cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations <- c(
  cor_test(rwq$fixed.acidity, rwq$quality),
  cor_test(rwq$citric.acid, rwq$quality),
  cor_test(rwq$sulphates, rwq$quality),
  cor_test(rwq$alcohol, rwq$quality),
  cor_test(rwq$volatile.acidity, rwq$quality),
  cor_test(rwq$chlorides, rwq$quality),
  cor_test(rwq$density, rwq$quality))

names(correlations) <- c('fixed.acidity', 'citric.acid', 
                         'sulphates', 'alcohol', 
                         'volatile.acidity',
                         'chlorides', 'density')
correlations

ggplot(data = rwq, aes(x = as.numeric(quality), y = citric.acid, color = quality)) +
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm",color ='blue') + 
  scale_y_continuous(limits = c(0, 0.8)) +
  ggtitle('Citric Acid in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Citric Acid (g / dm^3)')

ggplot(data = rwq, aes(x = as.numeric(quality), y = sulphates, color = quality)) +
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm",color ='blue') + 
  scale_y_continuous(limits = c(0.3, 1.4)) +
  ggtitle('Sulphates in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Sulphates (g / dm^3)')

ggplot(data = rwq, aes(x = as.numeric(quality), y = alcohol, color = quality)) +
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm",color ='blue') + 
  scale_y_continuous(limits = c(8, 14.5)) +
  ggtitle('Alcohol in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Alcohol (% volume)')

ggplot(data = rwq, aes(x = as.numeric(quality), y = volatile.acidity, color = quality)) +
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm",color ='blue') + 
  scale_y_continuous(limits = c(0.1, 1.4)) +
  ggtitle('Volatile Acidity in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Volatile Acidity (g / dm^3)')

cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations_acid <- c(
  cor_test(rwq$fixed.acidity, rwq$citric.acid),
  cor_test(rwq$citric.acid, rwq$volatile.acidity),
  cor_test(rwq$volatile.acidity, rwq$fixed.acidity),
  cor_test(rwq$pH, (rwq$fixed.acidity + rwq$volatile.acidity + rwq$citric.acid)))

names(correlations_acid) <- c('fixed.acidity_citric.acid', 
                         'citric.acid_volatile.acidity', 
                         'volatile.acidity_fixed.acidity',
                         'PH_total.acidity')
correlations_acid

p5 <- ggplot(data = rwq, aes(x = fixed.acidity, y = citric.acid)) +
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm",color ='blue')
p6 <- ggplot(data = rwq, aes(x = citric.acid, y = volatile.acidity)) +
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm",color ='blue')
p7 <- ggplot(data = rwq, aes(x = volatile.acidity, y = fixed.acidity)) +
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm",color ='blue')
p8 <- ggplot(data = rwq, aes(x = pH, y = fixed.acidity + volatile.acidity + citric.acid)) +
  geom_jitter(alpha = 1/5) +
  ylab('total.acidity') + 
  geom_smooth(method = "lm",color ='blue')

grid.arrange(p5, p6, p7, p8, ncol = 2)

ggplot(data = subset(rwq, rating != 'average'),
       aes(x = volatile.acidity, y = alcohol,
                      color = rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('Alcohol vs. Volatile Acidity and Wine Rating') +
  xlab('Volatile Acidity (g / dm^3)') +
  ylab('Alcohol (% volume)')

ggplot(data = subset(rwq, rating != 'average'),
       aes(x = citric.acid, y = alcohol,
                      color = rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('Alcohol vs. Citric Acid and Wine Rating') +
  xlab('Citric Acid (g / dm^3)') +
  ylab('Alcohol (% volume)')

ggplot(data = subset(rwq, rating != 'average'),
       aes(x = sulphates, y = alcohol,
                      color = rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('Alcohol vs. sulphates and Wine Rating') +
  xlab('Sulphates (g / dm^3)') +
  ylab('Alcohol (% volume)')

qplot(data = rwq, rating, fill = quality, 
      xlab = "Rating", ylab = "count", main = 'Distribution of Red Wine by Rating' )

ggplot(data = rwq, aes(x = quality, y = alcohol,
                      fill = rating)) +
  geom_boxplot() +
  ggtitle('Alcohol Levels in Different Wine Qualities') +
  xlab('Quality') +
  ylab('Alcohol (% volume)')

ggplot(data = subset(rwq, rating != 'average'),
       aes(x = volatile.acidity, y = alcohol,
                      color = rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('Alcohol vs. Volatile Acidity and Wine Rating') +
  xlab('Volatile Acidity (g / dm^3)') +
  ylab('Alcohol (% volume)')

