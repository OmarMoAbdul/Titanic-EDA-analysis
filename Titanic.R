#importing libraries...

library(tidyverse)

#Load in Data

titanic <- read_csv("C:\\Users\\User\\Downloads\\Titanic-dataset.csv")

#Preparing data for exploration

glimpse(titanic)

titanic$Survived=as.factor(titanic$Survived)
titanic$Pclass=as.factor(titanic$Pclass)
titanic$Sex=as.factor(titanic$Sex)
titanic$Embarked=as.factor(titanic$Embarked)
titanic$Cabin=as.factor(titanic$Cabin)

#Dealing with missing values...
summary(titanic)
table(titanic$Embarked)
titanic$Embarked[is.na(titanic$Embarked)]<-"S"
stitanic$Age[is.na(titanic$Age)]<-median(titanic$Age,na.rm = T)
summary(titanic$Age)


#EDA Analysis...

# What is the survival rate?...
ggplot(titanic) + geom_bar(mapping = aes(x=Survived)) + labs(title = "Survival rate") + theme_bw()
table(titanic$Survived)
prop.table(table(titanic$Survived))


# What is the gender rate?...
ggplot(titanic) + geom_bar(mapping = aes(x=Sex)) + labs(title = "Sex rate") + theme_bw()
table(titanic$Sex)
prop.table(table(titanic$Sex))


# What is the survival rate by gender?...
ggplot(titanic , aes(x= Sex, fill = Survived)) + 
  geom_bar() +
  theme_classic() + labs(title = "Survival rates by gender")

# What is the Pclass rate?...
ggplot(titanic) + geom_bar(mapping = aes(x=Pclass)) + labs(title = "Passenger class rate") + theme_bw()
table(titanic$Pclass)       
prop.table(table(titanic$Pclass))       


# What is the survival rates by Pclass?...
ggplot(titanic, aes(x=Pclass, fill = Survived)) +
  geom_bar() +
  theme_classic() + labs(title = "Survival rates by Passenger class")


# What is the survival rate by gender and Pclass...
ggplot(titanic, aes(x=Sex,fill=Survived))+geom_bar()+theme_bw()+facet_wrap(~Pclass)+
  labs(title = "Survival rates by gender and passenger class")

# How is the age distributed?...
ggplot(titanic, aes(x=Age))+geom_histogram(bins = 10)+theme_bw()+
  labs(title = "Titanic Age ditribution")

# What is the survival rate by age?...
ggplot(titanic, aes(x=Age, fill=Survived))+geom_histogram(bins = 10)+theme_bw()+
  labs(title = "Survival rate by Age")

# What is the Survival rate by Age, sex,  and Pclass?...
ggplot(titanic,aes(x=Age, fill=Survived))+geom_density(alpha=0.5)+theme_bw()+facet_wrap(Sex~Pclass)+
  labs(title = "Survival rate by age,gender,and Pclass",y="Survived")

                                  ###############################################

# How does fare and sex influence survival?...

##Tota Fare central tendency...
titanic %>%
  summarize(medFare = median(Fare))

titanic %>%
  summarize(meanFare = mean(Fare))

## Fare by male and female median...
titanic %>%
  filter(Sex=="male") %>%
  summarize(M.medFare=median(Fare))

titanic %>% 
  filter(Sex=="female") %>% 
  summarize(F.medFare=median(Fare))

## Fare by male and female mean...
titanic %>% 
  filter(Sex=="male") %>% 
  summarize(M.meanFare=mean(Fare))

titanic %>% 
  filter(Sex=="female") %>% 
  summarize(F.meanFare=mean(Fare))

#Pivot1 : find the relation among class,sex and mean,median fares...

pivot1 <- titanic %>% 
    group_by(Pclass,Sex) %>% 
    summarize(meanFare=mean(Fare),Passenger=n()) %>% 
    arrange(Pclass)
view(pivot1)

pivot2<- titanic %>% 
  group_by(Pclass,Sex) %>% 
  summarize(medFare=median(Fare),passenger=n()) %>% 
  arrange(Pclass)
view(pivot2)

#How does Pclass,sex,and fare(mean and median) affect surviving?...

pivot3<- titanic %>% 
  group_by(Pclass,Survived,Sex) %>% 
  summarize(meanFare=mean(Fare),passenger=n()) %>% 
  arrange(Pclass)
view(pivot3)

pivot4<-titanic %>% 
  group_by(Pclass,Survived,Sex) %>% 
  summarize(medFare=median(Fare),passenger=n()) %>% 
  arrange(Pclass)
view(pivot4)

#How does Embarkation affect fare?...

pivot5<-titanic %>% 
  group_by(Pclass,Embarked) %>% 
  summarize(meanFare=mean(Fare),passenger=n()) %>% 
  arrange(Pclass)
view(pivot5)

pivot6<-titanic %>% 
  group_by(Pclass,Embarked) %>% 
  summarize(medFare=median(Fare),passenger=n()) %>% 
  arrange(Pclass)
view(pivot6)

# Is there a relation between (fare,embarkation) and survival rate?...

pivot7<- titanic %>% 
  group_by(Pclass,Survived,Embarked) %>% 
  summarize(meanFare=mean(Fare),passenger=n()) %>% 
  arrange(Pclass)
view(pivot7)

pivot8<- titanic %>% 
  group_by(Pclass,Survived,Embarked) %>% 
  summarize(medFare=median(Fare),passenger=n()) %>% 
  arrange(Pclass)
view(pivot8)

# What is the distribution of Age by fare?...
ggplot(titanic,aes(x=Age,y=Fare))+geom_point()

#What is the distribution of Age by fare and sex?...

ggplot(titanic,aes(x=Age,y=Fare,color=Sex))+geom_point()

# What is the distribution of Age by fare,sex and survival rate?...

ggplot(titanic,aes(x=Age,y=Fare,color=Sex))+geom_point()+
  facet_grid(~Survived)

# Creating a new column called Familysize...
titanic$Familysize<- 1+titanic$SibSp+titanic$Parch
view(titanic)

#Is there a relation between Familysize and survival rate?...

ggplot(titanic,aes(x=Familysize,fill=Survived))+geom_histogram(bins = 5)+
  theme_bw()+
  facet_grid(~Pclass)



