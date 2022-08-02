# Linear model analysis
dim(bb.pollen.sd.sw.dw.params)
names(bb.pollen.sd.sw.dw.params)

# change data from wide to long view
fin.wide <- bb.pollen.sd.sw.dw.params

dim(fin.wide)
names(fin.wide)
fin.wide$bb.year <- year(fin.wide$bb.Date)
fin.wide$bb.month <- month(fin.wide$bb.Date)

# to remain 4 important insecticides: Acetamiprid, Clothianidin, Imidacloprid, Thiamethoxam, and delete columns for other insecticides
bb.remaining.cols <- c('County', 'bb.site', 'bb.Date', 'bb.year', 'bb.month', 'bb.Site.Type', 'Bee.Total', 'Queen.Total', 'Worker.Total', 'Male.Total')
pd.remaining.cols <- c("pd.Type", "pd.Acetamiprid", "pd.Clothianidin...MOECC", "pd.Clothianidin...AFL", "pd.Imidacloprid...MOECC", "pd.Thiamethoxam...AFL")
sd.remaining.cols <- c("Season", "sd.0-15_Acetamiprid", "sd.20-30_Acetamiprid", "sd.0-15_Clothianidin", "sd.20-30_Clothianidin", "sd.0-15_Imidacloprid", "sd.20-30_Imidacloprid", "sd.0-15_Thiamethoxam", "sd.20-30_Thiamethoxam")
sw.remaining.cols <- c("sw.ACETAMIPRID", "sw.CLOTHIANIDIN", "sw.IMIDACLOPRID", "sw.THIAMETHOXAM")
dw.remaining.cols <- c("dw.RAW WATER_ACETAMIPRID", "dw.TREATED WATER_ACETAMIPRID", "dw.RAW WATER_CLOTHIANIDIN", "dw.TREATED WATER_CLOTHIANIDIN", "dw.RAW WATER_IMIDACLOPRID", "dw.TREATED WATER_IMIDACLOPRID", "dw.RAW WATER_THIAMETHOXAM", "dw.TREATED WATER_THIAMETHOXAM")
important_columns <- c(bb.remaining.cols, pd.remaining.cols, sd.remaining.cols, sw.remaining.cols, dw.remaining.cols)

# data without extra columns
final.dat <- fin.wide[ ,important_columns]
dim(final.dat)
names(final.dat)

# manage NA
sum(is.na(final.dat))
sapply(final.dat, function(x) sum(is.na(x)))

final.dat <- na.omit(final.dat)
dim(final.dat)
names(final.dat)
summary(final.dat)


# deleting for variables without any variation
# novariables = list of insecticides with constant values for example all values are 5
novariables = c('sd.0-15_Acetamiprid' ,'sd.20-30_Acetamiprid', 'sw.ACETAMIPRID', 'dw.RAW WATER_ACETAMIPRID', 'dw.TREATED WATER_ACETAMIPRID', 'dw.TREATED WATER_CLOTHIANIDIN', 'dw.TREATED WATER_IMIDACLOPRID', 'dw.TREATED WATER_THIAMETHOXAM')

# exclude novariables from data
final.dat1 <- final.dat[ , -which(names(final.dat) %in% novariables)]
names(final.dat1)


# Data for analysis
analysis.data <- final.dat1[, c(1, 4:7, 12:29)]
names(analysis.data)
dim(analysis.data)

# distribution frequency of Bee.Total count
table(analysis.data$Bee.Total)
barplot(table(analysis.data$Bee.Total), col="#69b3a2", xlab = "Bumblebee Counts across all species", ylab = "Frequency")

# convert characters to factors for categorical variables for best modeling performance.S
class(analysis.data$County)
analysis.data$County <- as.factor(analysis.data$County)
class(analysis.data$Season)
analysis.data$Season <- as.factor(analysis.data$Season)
table(analysis.data$Season)
class(analysis.data$bb.year)
analysis.data$bb.year <- as.factor(analysis.data$bb.year)
class(analysis.data$bb.month)
analysis.data$bb.month <- as.factor(analysis.data$bb.month)
levels(analysis.data$bb.month)
table(analysis.data$bb.month)

# Modeling Stepwise variable selection
# intercept model
intercept_Model <- lm(Bee.Total ~ 1, data = analysis.data )
summary(intercept_Model)

# full model
full_Model <- lm(Bee.Total ~ ., data = analysis.data)
summary(full_Model)


# 1. perform forward stepwise regression
forward <- step(intercept_Model, direction='forward', scope=formula(full_Model), trace=0)
#view results of forward stepwise regression
forward$anova
#view final model
forward$coefficients
summary(forward)


# 2. perform backward stepwise regression
backward <- step(full_Model, direction='backward', scope=formula(full_Model), trace=0)
#view results of backward stepwise regression
backward$anova
#view final model
backward$coefficients
summary(backward)

# 3. perform Both-Direction stepwise regression
both <- step(intercept_Model, direction='both', scope=formula(full_Model), trace=0)
#view results of backward stepwise regression
both$anova
#view final model
both$coefficients
summary(both)



# K-Fold Cross-Validation
library(caret)

#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

#fit first regression model and use k-fold CV to evaluate performance
all.vars.model <- train(Bee.Total ~ ., data = analysis.data, method = "lm", trControl = ctrl)
#view summary of k-fold CV               
print(all.vars.model)

#fit second regression model and use k-fold CV to evaluate performance
steped.model <- train(Bee.Total ~ `sd.20-30_Clothianidin` + pd.Imidacloprid...MOECC + bb.year + `sd.20-30_Imidacloprid`, data = analysis.data, method = "lm", trControl = ctrl)
print(steped.model)


resams.of.mdl <- resamples(list(full.model = all.vars.model, steped.model = steped.model))

summary(resams.of.mdl)




# Plotting the predictions of bb count 
data_pred <- analysis.data
names(analysis.data)
names(data_pred)
## calculate and store predicted values
data_pred$phat_count <- predict(both, type="response")

## order by year
data_pred <- data_pred[with(data_pred, order(bb.year)), ]

summary(both)
## create the plot
library(ggplot2)
# plot for sd.20-30_Clothianidin
ggplot(data_pred, aes(x = `sd.20-30_Clothianidin`, y = phat_count, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Clothianidin insecticide in depth of 20-30 cm of soil", y = "Expected Count of All Bumblebee")

# plot for `sd.0-15_Thiamethoxam`
ggplot(data_pred, aes(x = `sd.0-15_Thiamethoxam`, y = phat_count, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Thiamethoxam insecticide in depth of 0-15 cm of soil", y = "Expected Count of All Bumblebee")


# plot for sd.0-15_Imidacloprid
ggplot(data_pred, aes(x = `sd.0-15_Imidacloprid`, y = phat_count, color = bb.year)) +
  geom_point( alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) +
  labs(x = "Imidacloprid insecticide in depth of 0-15 cm of soil", y = "Expected Count of All Bumblebee")

# plot for `sd.20-30_Thiamethoxam`
ggplot(data_pred, aes(x = `sd.20-30_Thiamethoxam`, y = phat_count, color = bb.year)) +
  geom_point(alpha=.5, position=position_jitter(h=.2), size = 3) +
  theme_classic(base_size = 16) + 
  labs(x = "Thiamethoxam insecticide in depth of 20-30 cm of soil", y = "Expected Count of All Bumblebee")

 
 