##BumbleBee:

bumblebee <- read.csv(file = "C:\\Users\\Narges\\Desktop\\DataAnalytics\\CIND 820\\Bumble_Bee_Public_Data.csv")
summary(bumblebee)
summary(as.factor(bumblebee$ï..County))
nlevels(as.factor(bumblebee$ï..County))
str(bumblebee)
sum(is.na(bumblebee))

sum(bumblebee$bimaculatus.total )
sum(bumblebee$griseocollis.total)
sum(bumblebee$impatiens.total)
sum(bumblebee$fervidus.total)
sum(bumblebee$borealis.total)
sum(bumblebee$rufocinctus.total)
sum(bumblebee$pensylvanicus.total)
sum(bumblebee$vagans.total)
sum(bumblebee$ternarius.total)
sum(bumblebee$perplexus.total)
sum(bumblebee$citrinus.total)
sum(bumblebee$auricomus.total)
sum(bumblebee$terricola.total)
sum(bumblebee$flavidus.total)

bumblebee$Site.Type[bumblebee$Site.Type=='Agri']='Agricultural'
bumblebee$Site.Type[bumblebee$Site.Type=='Agriculture']='Agricultural'

bumblebee$date <- as.Date(bumblebee$Sample.date,format = '%m/%d/%Y')

##  bimaculatus:
aggregate(bimaculatus.total~Site.Type,data = bumblebee,FUN = length)
bim_mean <- aggregate(bimaculatus.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
          ,FUN = mean)
names(bim_mean)<- c("Month","Year","Site","Total")
ggplot(bim_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

bima_freq <- aggregate(bimaculatus.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                       ,FUN = length) 
names(bima_freq)<-c("Month","Year","Site","Total")
ggplot(bima_freq,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

aggregate(bimaculatus.total~Site.Type,data = bumblebee,FUN = sum)
bim_sum <- aggregate(bimaculatus.total~format(date,'%m')+Year+Site.Type,data = bumblebee,FUN = sum)
names(bim_sum)<-c("Month","Year","Site","Total")
ggplot(bim_sum,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)



##  griseocollis:

aggregate(griseocollis.total~Site.Type,data = bumblebee,FUN = sum)
gris_freq <- aggregate(griseocollis.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                     ,FUN = sum)
names(gris_freq)<-c("Month","Year","Site","Total")


ggplot(gris_freq,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

gris_mean <- aggregate(griseocollis.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                     ,FUN = mean)           
names(gris_mean)<- c("Month","Year","Site","Total")
ggplot(gris_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)


##  impatiens:
aggregate(impatiens.total~Site.Type,data = bumblebee,FUN = sum)
imp_freq <- aggregate(impatiens.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                       ,FUN = sum)
names(imp_freq)<- c("Month","Year","Site","Total")
ggplot(imp_freq,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

imp_mean <-aggregate(impatienssum()imp_mean <-aggregate(impatiens.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                      ,FUN = mean) 
names(imp_mean) <- c("Month","Year","Site","Total")  
ggplot(imp_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)


##  vagans:
aggregate(vagans.total~Site.Type,data = bumblebee,FUN = sum)
vag_freq <- aggregate(vagans.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
          ,FUN = sum)
names(vag_freq)<- c("Month","Year","Site","Total")
ggplot(vag_freq,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

vag_mean <-aggregate(vagans.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                      ,FUN = mean) 
names(vag_mean) <- c("Month","Year","Site","Total")  
ggplot(vag_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

##  rufocinctus:
aggregate(rufocinctus.total~Site.Type,data = bumblebee,FUN = sum)
ruf_freq <- aggregate(rufocinctus.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                       ,FUN = sum)
names(ruf_freq)<- c("Month","Year","Site","Total")
ggplot(ruf_freq,aes(Year,Total)+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

ruf_mean <-aggregate(rufocinctus.total~format(date,'%m')+Year+Site.Type ,data = bumblebee
                      ,FUN = mean) 
names(ruf_mean) <- c("Month","Year","Site","Total")  
ggplot(ruf_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Site)

head(bumblebee[,c(1,2,3,4,5,6,62,63)])
list(bumblebee$Flowers.Bees.Collected.on)

allbee <-c(bumblebee$bimaculatus.total,bumblebee$griseocollis.total,
           bumblebee$impatiens.total,bumblebee$rufocinctus.total,
           bumblebee$vagans.total)
length(allbee)
nrow(allbee)
  rowSums(bumblebee,allbee,na.rm = TRUE)
bumblebee$bees <-rowsum()

###  Soil:
soil <- read.csv(file = "C:\\Users\\Narges\\Desktop\\DataAnalytics\\CIND 820\\Soil_NNI_Monitoring_Data.csv")
ds<-head(soil)
View(ds)
summary(soil)
str(soil)
levels(as.factor(soil$X.MDL.flag))
nlevels(as.factor(soil$X.MDL.flag))
levels(as.factor(soil$Season))
levels(as.factor(soil$Field.Growth.Stage))
levels(as.factor(soil$Parameter.Name))
levels(as.factor(soil$Watershed))

soil$Parameter.Name <- as.factor(soil$Parameter.Name)

soil$Parameter.Name[soil$Parameter.Name=="   Acetamiprid"]= "Acetamiprid"
soil$Parameter.Name[soil$Parameter.Name=="   Dinotefuran"]="Dinotefuran"
soil$Parameter.Name[soil$Parameter.Name=="   Imidacloprid"]="Imidacloprid"                      
soil$Parameter.Name[soil$Parameter.Name=="   Thiacloprid"]="Thiacloprid"
soil$Parameter.Name[soil$Parameter.Name=="   Clothianidin"]="Clothianidin"
soil$Parameter.Name[soil$Parameter.Name=="   Flonicamid"]="Flonicamid"
soil$Parameter.Name[soil$Parameter.Name=="   Nitenpyram" ]="Nitenpyram"
soil$Parameter.Name[soil$Parameter.Name=="   Thiamethoxam"]="Thiamethoxam"

sum(is.na(soil))
aggregate(Value~ Collection.Year+Collection.Month,data = soil,FUN = mean)

aggregate(Value~ Watershed,data = soil,FUN = mean)
aggregate(Value~ Watershed,data = soil,FUN = sum)
aggregate(Watershed~Parameter.Name,data = soil,FUN = lenght)

ins_mean <-aggregate(Value~ Collection.Year+Parameter.Name+Field.Growth.Stage,data = soil,FUN = mean)
names(ins_mean)<- c('Year','Name','Type','Total')
#aggregate(Value~ Parameter.Name + Watershed,data = soil, FUN = sum)
#ggplot(ins_mean,aes(Year,Total))+ 
#  geom_col(aes(color=Name,fill=Name,position = position_dodge(.8),width = .7)+
#  facet_wrap(~Type)

ggplot(ins_mean,aes(Year,Total))+
geom_col(aes(color=Name,fill=Name,position = position_dodge(.8),width = .7)+
  facet_wrap(~Type)

  neo_mean <-aggregate(Value~+Collection.Month+Collection.Year+Parameter.Name+Field.Growth.Stage,data = soil
                       ,FUN = mean) 
names(neo_mean) <- c("Month","Year","Type","Field","Total")  
ggplot(neo_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Type)

ggplot(neo_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Field) 
ggplot(neo_mean,aes(Year,Total))+
  geom_col(aes(color=Type,fill=Type),position = position_dodge(.8),width = .7)+
  facet_wrap(~Field)
  
aggregate(Value~ Field.Growth.Stage,data = soil,FUN = mean)
aggregate(Value~ Collection.Year+Collection.Month,data = soil,FUN = mean)
aggregate(Value~ Collection.Year+Collection.Month,data = soil,FUN = mean)


## Drinking Water:
str(Drinking_Water_NNI_Data)
summary(Drinking_Water_NNI_Data)
sum(is.na(Drinking_Water_NNI_Data))
levels(as.factor(Drinking_Water_NNI_Data$`Sample Condition`))
list(as.factor(Drinking_Water_NNI_Data$`Sample Condition`))
sum(is.na(Drinking_Water_NNI_Data))

levels(as.factor(Drinking_Water_NNI_Data$Parameter))
levels(as.factor(Drinking_Water_NNI_Data$`Sample Type`))
levels(as.factor(Drinking_Water_NNI_Data$`Sample Condition`))
levels(as.factor(Drinking_Water_NNI_Data$Result))
#levels(as.factor(Drinking_Water_NNI_Data$`Sample Location`))
#levels(as.factor(Drinking_Water_NNI_Data$`Station #`))
levels(as.factor(Drinking_Water_NNI_Data$`Analysis Method`))

Drinking_Water_NNI_Data$date <- 
  as.Date(Drinking_Water_NNI_Data$`Sample Date`,format = '%Y%m%d')


peat_sum <- aggregate(`Detection Limit (DL)`~Year+Parameter+`Sample Type`+`Sample Location`
          ,data = Drinking_Water_NNI_Data ,FUN = sum )

aggregate(`Detection Limit (DL)`~Year+Parameter+`Sample Type`+`Sample Location`
          ,data = Drinking_Water_NNI_Data ,FUN = mean )
pest_mean<-aggregate(`Detection Limit (DL)`~Year+format(date,'%m')
                     +Parameter+`Sample Type`+`Sample Location`
                     ,data = Drinking_Water_NNI_Data ,FUN = mean ) 
head(pest_mean)
names(pest_mean)
names(pest_mean)<- c("Year","Month","Type","Sample","Location","Total")

ggplot(pest_mean,aes(Year,Total))+
  geom_col(aes(color=Type,fill=Type),position = position_dodge(.8),width = .7)+
  facet_wrap(~Sample)

ggplot(pest_mean,aes(Year,Total))+
  geom_col(aes(color=Sample,fill=Sample),position = position_dodge(.8),width = .7)+
  facet_wrap(~Type)

ggplot(pest_mean,aes(Year,Total))+
  geom_col(aes(color=Month,fill=Month),position = position_dodge(.8),width = .7)+
  facet_wrap(~Type)

ggplot(pest_mean,aes(Month,Total))+
  geom_col(aes(color=Type,fill=Type)
           ,position = position_dodge(.8),width = .7)+
  facet_wrap(~Year)

ggplot(pest_mean,aes(Year,Total))+
  geom_col(aes(color=Type,fill=Type),position = position_dodge(.8),width = .7)+
  facet_wrap(~Month)



## Neonicotinoid:
levels(as.factor(Multimedia_Stream_Neonic_Data$PARAMETER))
summary(Multimedia_Stream_Neonic_Data)
stream <- Multimedia_Stream_Neonic_Data
str(Multimedia_Stream_Neonic_Data)
stream$date <-as.Date(as.character(stream$`SAMPLE DATE`),format='%Y%m%d')
aggregate(RESULT~format(stream$date,'%m')+format(stream$date,'%Y'),data=stream,FUN= mean)
aggregate(RESULT~format(stream$date,'%m')+`SITE NAME`,data=stream,FUN= mean)
aggregate(RESULT~format(stream$date,'%m')+PARAMETER,data = stream,FUN = mean)
aggregate(RESULT~format(stream$date,'%m')+PARAMETER,data = stream,FUN = length)


## Pollen:
pollen <- read.csv(file = "C:\\Users\\Narges\\Desktop\\DataAnalytics\\CIND 820\\Pollen_Monitoring_Network_Data.csv")
