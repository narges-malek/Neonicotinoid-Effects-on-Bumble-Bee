#Data Description and Visualization

# Using Package " ggplot2 " to creat
library(ggplot2)


table(fin.long1$County)
levels(as.factor(fin.long1$County))
fin.long1[fin.long1$count>40, c("County","bb.date")]
names(fin.long1)

#The average number of bumblebee counted in samples collected date individually.

count.date <- group_by(fin.long1, bb.date) %>%
  summarise(count = mean(count))

ggplot(count.date, aes(x=bb.date, y=count)) +
  geom_line(color="darkgreen") +
  facet_grid(~ year(bb.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("Average count of Bumblebee")
  

#The average counted number of bumblebee by Their characteristics(Male, Female, Worker and Queen)

count.date.bee.caste <- group_by(fin.long1, bee.caste, bb.date) %>%
  summarise(count = mean(count))

ggplot(count.date.bee.caste, aes(x=bb.date, y=count)) +
  geom_line(color="darkgreen") +
  facet_grid(bee.caste ~ year(bb.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("Average count of Bumblebee")


#The average counted number of bumblebee by their species.

count.date.bee.spp <- group_by(fin.long1, bee.spp, bb.date) %>%
  summarise(count = mean(count))

ggplot(count.date.bee.spp, aes(x=bb.date, y=count)) +
  geom_line(color="darkgreen") +
  facet_grid(bee.spp ~ year(bb.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("Average count of Bumblebee")

#The average value of detected insecticide in soil samples provided in final merged dataframe.

count.sd.insecticide <- group_by(fin.long1, bb.date) %>%
  summarise(count = mean(count), `sd.0-15_Thiamethoxam` = mean(`sd.0-15_Thiamethoxam`), `sd.20-30_Imidacloprid` = mean(`sd.20-30_Imidacloprid`), `sd.20-30_Thiamethoxam` = mean(`sd.20-30_Thiamethoxam`))

ggplot(count.sd.insecticide, aes(x=bb.date, y=`sd.0-15_Thiamethoxam`)) +
  geom_line(color="darkgreen") +
  facet_grid(~year(count.sd.insecticide$bb.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("Average value for insecticide in soil")



#The average detected value of insecticide in stream water samples provided in final dataset.

count.sw.insecticide <- group_by(fin.long1, bb.date) %>%
  summarise(count = mean(count), sw.IMIDACLOPRID = mean(sw.IMIDACLOPRID))

ggplot(count.sw.insecticide, aes(x=bb.date, y=sw.IMIDACLOPRID)) +
  geom_line(color="darkgreen") +
  facet_grid(~year(count.sw.insecticide$bb.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("Average value for insecticide in streaming water")

#_______________________________________________________________



#Data Visualization for soil Dataset


soil.date <- group_by(soil, sd.date, `Parameter Name`) %>%
  summarise(Value = mean(Value))

ggplot(soil.date, aes(x=sd.date, y=Value)) +
  geom_line(color="darkgreen") +
  facet_grid(~ year(soil.date$sd.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "7 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("Average value for insecticide in soil")


ggplot(soil.date, aes(x=sd.date, y=Value)) +
  geom_line(color="darkgreen") +
  facet_grid(`Parameter Name` ~ year(soil.date$sd.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "7 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("Average value for insecticide in soil")



#Data Visualization for Stream water

streamwater.date <- group_by(Stream_water, sw.date, PARAMETER) %>%
  summarise(Value = mean(RESULT))

ggplot(streamwater.date, aes(x=sw.date, y=Value)) +
  geom_line(color="darkgreen") +
  facet_grid(~year(streamwater.date$sw.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "7 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("The average insecticide residue in stream water")


ggplot(streamwater.date, aes(x=sw.date, y=Value)) +
  geom_line(color="darkgreen") +
  facet_grid(PARAMETER ~ year(streamwater.date$sw.date), space="free_x", scales="free_x", switch="x") +
  scale_x_date(date_breaks = "7 day",
               date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.placement = "outside") +
  xlab("") +
  ylab("The average insecticide residue in stream water")

