library(reshape2)

# Linear model analysis
dim(bb.pollen.sd.sw.dw.params)
names(bb.pollen.sd.sw.dw.params)

# change data from wide to long view
fin.wide_tobe_long <- bb.pollen.sd.sw.dw.params
dim(fin.wide_tobe_long)
names(fin.wide_tobe_long)



# to remain 4 important insecticides: Acetamiprid, Clothianidin, Imidacloprid, Thiamethoxam, and delete columns for other insecticides
bb.spp.cols <- names(fin.wide_tobe_long)[9:68]
pd.remaining.cols <- c("pd.Type", "pd.Acetamiprid", "pd.Clothianidin...MOECC", "pd.Clothianidin...AFL", "pd.Imidacloprid...MOECC", "pd.Thiamethoxam...AFL")
sd.remaining.cols <- c("Season", "sd.0-15_Acetamiprid", "sd.20-30_Acetamiprid", "sd.0-15_Clothianidin", "sd.20-30_Clothianidin", "sd.0-15_Imidacloprid", "sd.20-30_Imidacloprid", "sd.0-15_Thiamethoxam", "sd.20-30_Thiamethoxam")
sw.remaining.cols <- c("sw.ACETAMIPRID", "sw.CLOTHIANIDIN", "sw.IMIDACLOPRID", "sw.THIAMETHOXAM")
dw.remaining.cols <- c("dw.RAW WATER_ACETAMIPRID", "dw.TREATED WATER_ACETAMIPRID", "dw.RAW WATER_CLOTHIANIDIN", "dw.TREATED WATER_CLOTHIANIDIN", "dw.RAW WATER_IMIDACLOPRID", "dw.TREATED WATER_IMIDACLOPRID", "dw.RAW WATER_THIAMETHOXAM", "dw.TREATED WATER_THIAMETHOXAM")
bb.spp_important_insecticides <- c(bb.spp.cols, pd.remaining.cols, sd.remaining.cols, sw.remaining.cols, dw.remaining.cols)
length(bb.spp_important_insecticides)

# data without extra columns, delete variables that doesn't need for 
final.wide.tobe.long <- fin.wide_tobe_long[ ,bb.spp_important_insecticides]
dim(final.wide.tobe.long)
names(final.wide.tobe.long)




summary(final.wide.tobe.long)
# deleting for variables without any variation
# novariables = list of insecticides with constant values for example all values are 5
novariables = c('sd.0-15_Acetamiprid' ,'sd.20-30_Acetamiprid', 'sw.ACETAMIPRID', 'dw.RAW WATER_ACETAMIPRID', 'dw.TREATED WATER_ACETAMIPRID', 'dw.TREATED WATER_CLOTHIANIDIN', 'dw.TREATED WATER_IMIDACLOPRID', 'dw.TREATED WATER_THIAMETHOXAM')

# exclude novariables from data
final.wide.tobe.long <- final.wide.tobe.long[ , -which(names(final.wide.tobe.long) %in% novariables)]
names(final.wide.tobe.long)


# manage NA
sum(is.na(final.wide.tobe.long))
sapply(final.wide.tobe.long, function(x) sum(is.na(x)))

final.wide.tobe.long <- na.omit(final.wide.tobe.long)
dim(final.wide.tobe.long)

# convert data to long format
library(stringr)

fin.long.cleaned <- reshape2::melt(data = final.wide.tobe.long, id.vars = c(names(final.wide.tobe.long)[1:6], c(names(final.wide.tobe.long)[61:79])) , measure.vars = c(names(final.wide.tobe.long)[7:60]), variable.name = "bee.spp.type", value.name = "count")
fin.long.cleaned[c('bee.spp', 'bee.caste')] <- str_split_fixed(fin.long.cleaned$bee.spp.type, "\\.", 2)
names(fin.long.cleaned)


# clean for bb caste and spp
table(fin.long.cleaned$bee.spp)
table(fin.long.cleaned$bee.caste)

fin.long.cleaned[fin.long.cleaned$bee.caste == 'males', 'bee.caste'] = 'male'
fin.long.cleaned[fin.long.cleaned$bee.caste == 'Queens', 'bee.caste'] = 'Queen'
fin.long.cleaned[fin.long.cleaned$bee.caste == 'workers', 'bee.caste'] = 'worker'
table(fin.long.cleaned$bee.caste)

# remove rows which are sum of some other rows
fin.long.cleaned1 = fin.long.cleaned[fin.long.cleaned$bee.caste != 'total',]
table(fin.long.cleaned1$bee.caste)

summary(fin.long.cleaned1$count)
fin.long.cleaned1 <- fin.long.cleaned1[!is.na(fin.long.cleaned1$count), ]
dim(fin.long.cleaned1)

# to show rare bee spp
library(dplyr)
bb.spp.desc <- group_by(fin.long.cleaned1, bee.spp) %>%
  summarise(NofNonZero = sum(count > 0, na.rm = TRUE))

low.number.bb <- c('pensylvanicus', 'ternarius', 'perplexus', 'auricomus', 'terricola', 'flavidus' )
fin.long.cleaned1 <- fin.long.cleaned1[!fin.long.cleaned1$bee.spp %in% low.number.bb, ]
dim(fin.long.cleaned1)
levels(as.factor(fin.long.cleaned1$bee.spp))

levels(as.factor(fin.long.cleaned1$bb.date))
fin.long.cleaned1$bb.year = as.factor(format(fin.long.cleaned1$bb.Date, format = '%Y'))
class(fin.long.cleaned1$bb.year)
fin.long.cleaned1$bb.month = as.factor(format(fin.long.cleaned1$bb.Date, format = '%m'))
table(fin.long.cleaned1$bb.year, fin.long.cleaned1$bb.month)
names(fin.long.cleaned1)
table(fin.long.cleaned1$bb.Site.Type)


dim(fin.long.cleaned1)



## Data Analysis in GLM
# Long data for GLM analysis
names(fin.long.cleaned1)
GLM.analysis.data.long = fin.long.cleaned1[, c(1, 3, 6, 8:25, 27:31)]



# Assessing the response variable distribution
dim(GLM.analysis.data.long)
count.frq <- table(GLM.analysis.data.long$count)
barplot(count.frq)


counts <- as.numeric(levels(as.factor(GLM.analysis.data.long$count)))
count.mean <- mean(GLM.analysis.data.long$count)
Ncount <- length(GLM.analysis.data.long$count)

# 1) similarity with poisson distribution
pois.freq <- dpois(counts, count.mean)*Ncount
bbcount.pois.freq <- numeric(2*length(counts))
bbcount.pois.freq[1:(2*length(counts)) %% 2 != 0] <- count.frq
bbcount.pois.freq[1:(2*length(counts)) %% 2 == 0] <- pois.freq
labels <- character(2*length(counts))
labels[1:(2*length(counts)) %% 2!=0] <- as.character(counts)

# plot frequency distribution of count data and poison standard dist
barplot(bbcount.pois.freq, col=rep(c("red", "yellow"), length(counts)), names = as.numeric(labels), xlab = 'counts', ylab = 'frequency')
legend("top",
       c("Bumblebee count frequency","Poisson distribution"), 
       fill=c("red", "yellow"), 
       cex=0.7,
       text.font=4,
       bty = "n")



# 2) similarity with Negative binomial distribution
# first estimate the clumping parameter  (mean^2/(var - mean))
k <- mean(GLM.analysis.data.long$count)^2/(var(GLM.analysis.data.long$count)-mean(GLM.analysis.data.long$count))
nb.freq <- dnbinom(counts, k, mu=count.mean)*Ncount
bbcount.nb.freq <- numeric(2*length(counts))
bbcount.nb.freq[1:(2*length(counts)) %% 2 != 0] <- count.frq
bbcount.nb.freq[1:(2*length(counts)) %% 2 == 0] <- nb.freq

# plot frequency distribution of count data and Negative binomial standard dist
barplot(bbcount.nb.freq, col=rep(c("red", "yellow"), length(counts)), names = as.numeric(labels), xlab = 'counts', ylab = 'frequency')
legend("top",
       c("Bumblebee count frequency","Negative binomial distribution"), 
       fill=c("red", "yellow"), 
       cex=0.7,
       text.font=4,
       bty = "n")



pois.model.l <- glm(count ~ . , family = poisson, data = GLM.analysis.data.long)
summary(pois.model.l)
#step(pois.model.l, direction="backward")

quasipois.model.l <- glm(count ~ ., family = quasipoisson, data = GLM.analysis.data.long)
summary(quasipois.model.l)



library(MASS)
nb.model.l <- glm.nb(count ~ ., data = GLM.analysis.data.long)
summary(nb.model.l)


# hurdle method
library(pscl)
set.seed(123)
hurdle.model.l <- hurdle(count ~ ., dist = "negbin", data = GLM.analysis.data.long)
summary(hurdle.model.l)

# zero inflated method
zp.model.l <- zeroinfl(count ~ ., dist = "poisson", data = GLM.analysis.data.long)
summary(zp.model.l)





# K-fold Cross-Validation
# terms in Cross-Validation results
# number of folds: In practice, we typically choose between 5 and 10 folds because this turns out to be the optimal number of folds that produce reliable test error rates.
# RMSE: The root mean squared error. This measures the average difference between the predictions made by the model and the actual observations. The lower the RMSE, the more closely a model can predict the actual observations
# Rsquared: This is a measure of the correlation between the predictions made by the model and the actual observations. The higher the R-squared, the more closely a model can predict the actual observations.
# MAE: The mean absolute error. This is the average absolute difference between the predictions made by the model and the actual observations. The lower the MAE, the more closely a model can predict the actual observations.

library(caret)
ctrl <- trainControl(method = "cv", number = 5)
cv.model.pois <- train(count ~ ., data = GLM.analysis.data.long, method = "glm", trControl = ctrl, family = poisson(link = "log"))
cv.model.Qpois <- train(count ~ ., data = GLM.analysis.data.long, method = "glm", trControl = ctrl, family = quasipoisson(link = "log"))
cv.model.glm.nb <- train(count ~ ., data = GLM.analysis.data.long, method = "glm.nb", trControl = ctrl)

resams <- resamples(list(Poisson = cv.model.pois, QuasiPoisson = cv.model.Qpois, Negative.Binomial = cv.model.glm.nb))
summary(resams)


# Results in graph
summary(pois.model.l)
data_GLM.pred <- GLM.analysis.data.long
# Model prediction
## calculate and store predicted values
data_GLM.pred$phat_pois.1 <- predict(pois.model.l, type="response")
## order by program and then by math
data_GLM.pred <- data_GLM.pred[with(data_GLM.pred, order(bb.year)), ]
names(data_GLM.pred)
dim(data_GLM.pred)

## create the plots
library(ggplot2)
# 1) plot for pd.Imidacloprid...MOECC
ggplot(data_GLM.pred, aes(x = pd.Imidacloprid...MOECC, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Imidacloprid insecticide in pollen", y = "Expected Count of All Bumblebee")


# 2) plot for `sd.20-30_Clothianidin`
ggplot(data_GLM.pred, aes(x = `sd.20-30_Clothianidin`, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Clothianidin insecticide in streaming water", y = "Expected Count of All Bumblebee")


# 3) plot for `sd.20-30_Thiamethoxam`
ggplot(data_GLM.pred, aes(x = `sd.20-30_Thiamethoxam`, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Thiamethoxam insecticide in depth of 20 to 30 cm of soil", y = "Expected Count of All Bumblebee")


# 4) plot for sw.CLOTHIANIDIN
ggplot(data_GLM.pred, aes(x = sw.CLOTHIANIDIN, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Clothianidin insecticide in streaming water", y = "Expected Count of All Bumblebee")


# 5) plot for sw.THIAMETHOXAM
ggplot(data_GLM.pred, aes(x = sw.THIAMETHOXAM, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Thiamethoxam insecticide in streaming water", y = "Expected Count of All Bumblebee")

# 6) plot for `dw.RAW WATER_CLOTHIANIDIN`
ggplot(data_GLM.pred, aes(x = `dw.RAW WATER_CLOTHIANIDIN`, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Clothianidin insecticide in raw drinking water samples", y = "Expected Count of All Bumblebee")

# 7) plot for `dw.RAW WATER_IMIDACLOPRID`
ggplot(data_GLM.pred, aes(x = `dw.RAW WATER_IMIDACLOPRID`, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Imidacloprid insecticide in raw drinking water samples", y = "Expected Count of All Bumblebee")

# 8) plot for `dw.RAW WATER_THIAMETHOXAM`
ggplot(data_GLM.pred, aes(x = `dw.RAW WATER_THIAMETHOXAM`, y = phat_pois.1, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Thiamethoxam insecticide in raw drinking water samples", y = "Expected Count of All Bumblebee")



