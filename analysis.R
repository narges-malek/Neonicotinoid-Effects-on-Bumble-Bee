bumblebee = read.csv(file.choose())

bumblebee <- bumblebee[ , -which(names(bumblebee) %in% c("X","X.1"))]
bumblebee$Sample.date.formated = as.Date(bumblebee$Sample.date, format = "%m/%d/%Y")
months(bumblebee$Sample.date.formated)[1:10]
names(bumblebee)

table(bumblebee$Site.Type)
bumblebee$Site.Type[bumblebee$Site.Type == '']
bumblebee <- bumblebee[bumblebee$Site.Type != '',]

bumblebee$Site.Type[bumblebee$Site.Type=='Agri'] = 'Agricultural'
bumblebee$Site.Type[bumblebee$Site.Type=='Agriculture'] = 'Agricultural'
bumblebee$Site.Type[bumblebee$Site.Type=='Semi-natural remnant'] = 'Natural'

str(bumblebee)
levels(as.factor(bumblebee$County))
levels(as.factor(bumblebee$MOECC.Site.Number))
table(bumblebee$Site.Type, bumblebee$Year)
table(months(bumblebee$Sample.date.formated), bumblebee$Year)

freq = aggregate(bimaculatus.total ~ months(bumblebee$Sample.date.formated) + Year + Site.Type, data = bumblebee, FUN = sum)
names(freq) = c("Month", "Year", "Site", "bimaculatus.total")


require(ggplot2)
p <- ggplot(data = freq, aes(x=Year, y=bimaculatus.total)) +
          geom_col(aes(color=Month, fill=Month), position = position_dodge(0.8), width = 0.7)


p1 <- ggplot(data = freq, aes(x=Year, y=bimaculatus.total)) +
      geom_col(aes(color=Month, fill=Month), position = position_dodge(0.8), width = 0.7) +
      facet_wrap(~Site)


bumblebee$citrinus.worker <- 0
bumblebee$flavidus.worker <- 0

# change data from wide to long view
library(reshape2)
names(bumblebee)
bumblebee_long <- melt(data = bumblebee, id.vars = c(names(bumblebee)[1:7], names(bumblebee)[62:64]), measure.vars = c(names(bumblebee)[8:61], names(bumblebee)[65:66]), variable.name = "bee.spp.type", value.name = "count")
hist(bumblebee_long$count[bumblebee_long$count>0])
mean(bumblebee_long$count[bumblebee_long$count>0], na.rm = TRUE)
var(bumblebee_long$count[bumblebee_long$count>0], na.rm = TRUE)

# split bee spp and type
library(stringr)
bumblebee_long[c('bee.spp', 'bee.caste')] <- str_split_fixed(bumblebee_long$bee.spp.type, "\\.", 2)


table(bumblebee_long$bee.spp.type)
table(bumblebee_long$bee.caste)
bumblebee_long$bee.caste[bumblebee_long$bee.caste == 'males'] <- 'male'
bumblebee_long$bee.caste[bumblebee_long$bee.caste == 'Queens'] <- 'Queen'
bumblebee_long$bee.caste[bumblebee_long$bee.caste == 'workers'] <- 'worker'

table(bumblebee_long$count[bumblebee_long$bee.caste != 'total'])
bumblebee_long_no_total <- bumblebee_long[bumblebee_long$bee.caste != 'total',]
tapply(bumblebee_long_no_total$count,list(bumblebee_long_no_total$Year, months(bumblebee_long_no_total$Sample.date.formated), bumblebee_long_no_total$Site.Type),mean, na.rm = TRUE)

mean_count <- aggregate(count ~ months(Sample.date.formated) + Year + Site.Type, data = bumblebee_long_no_total, FUN = mean)
names(mean_count) <- c("Month", "Year", "Site", "count")
ggplot(data = mean_count, aes(x=as.factor(Year), y=count)) +
  geom_col(aes(color=Month, fill=Month), position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~Site) + xlab("Year") + ylab("bee count mean") + theme_classic()

sum_count <- aggregate(count ~ months(Sample.date.formated) + Year + Site.Type, data = bumblebee_long_no_total, FUN = sum)
names(sum_count) <- c("Month", "Year", "Site", "count")
ggplot(data = sum_count, aes(x=as.factor(Year), y=count)) +
  geom_col(aes(color=Month, fill=Month), position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~Site) + xlab("Year") + ylab("sum of bee count") + theme_classic()

mean_count_spp <- aggregate(count ~ months(Sample.date.formated) + Year + bee.spp, data = bumblebee_long_no_total, FUN = mean)
names(mean_count_spp) <- c("Month", "Year", "spp", "count")
ggplot(data = mean_count_spp, aes(x=spp, y=count)) +
  geom_col(aes(color=Month, fill=Month), position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~Year, nrow=3) +
  xlab("bee species") + ylab("bee count mean") + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

mean_count_caste <- aggregate(count ~ months(Sample.date.formated) + Year + bee.caste, data = bumblebee_long_no_total, FUN = mean)
names(mean_count_caste) <- c("Month", "Year", "caste", "count")
ggplot(data = mean_count_caste, aes(x=caste, y=count)) +
  geom_col(aes(color=Month, fill=Month), position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~Year, nrow=3) +
  xlab("bee caste") + ylab("bee count mean") + theme_classic()


hist(bumblebee_long_no_total$count, xlab = "bumble bee count", col = 'blue', main='')

Stream_Data <- read_excel("Datasets/Multimedia_Stream_Neonic_Data.xlsx")
names(Stream_Data)
str(Stream_Data)
Stream_Data$Sample.date.formated = as.Date(as.character(Stream_Data$`SAMPLE DATE`), format = "%Y%m%d")
table(months(Stream_Data$Sample.date.formated), format(Stream_Data$Sample.date.formated, "%Y"))

neo_mean <- aggregate(RESULT ~ months(Sample.date.formated) + format(Sample.date.formated,"%Y")
                      + PARAMETER,data = Stream_Data, FUN = mean)

names(neo_mean) <- c("Month","Year","Insecticide","Result")

ggplot(neo_mean,aes(Year,Result))+
  geom_col(aes(color=Insecticide,fill=Insecticide),position = position_dodge(.8),width = .7)+
  facet_wrap(~Month, nrow = 4) +
  xlab("Year") + ylab("Neonicotinoid concentration mean") + theme_classic() 

model <- glm(count ~ Year*months(Sample.date.formated) + bee.spp + bee.role, quasipoisson, data = bumblebee_long_no_total)
summary(model)



 

