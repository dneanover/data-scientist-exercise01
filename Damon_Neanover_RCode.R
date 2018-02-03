#SQL COMMAND ----------------------------------------------------------
          # SELECT r.id, 
          # age, 
          # workclasses.name AS workclass, 
          # education_levels.name AS education_levels, 
          # r.education_num,
          # marital_statuses.name AS martial_status, 
          # occupations.name AS occupation,
          # relationships.name AS relationship,
          # races.name AS race,
          # sexes.name AS sex,
          # r.capital_gain,
          # r.capital_loss,
          # r.hours_week,
          # countries.name AS country,
          # r.over_50k
          # FROM records AS r 
          # LEFT JOIN workclasses 
          # ON r.workclass_id = workclasses.id
          # LEFT JOIN education_levels
          # ON r.education_level_id = education_levels.id
          # LEFT JOIN marital_statuses 
          # ON r.marital_status_id = marital_statuses.id
          # LEFT JOIN occupations
          # ON r.occupation_id = occupations.id
          # LEFT JOIN relationships
          # ON r.relationship_id = relationships.id
          # LEFT JOIN races
          # ON r.race_id = races.id
          # LEFT JOIN sexes
          # ON r.sex_id = sexes.id
          # LEFT JOIN countries
          # ON countries.id = r.country_id
          # ORDER BY r.ID

#Importing Tidyverse for general purpose and shaping, Deducer for ggcorplot ----
library(tidyverse)
library(Deducer)
library(corrplot)

#Importing the CSV file into R ----
census = read_csv('C:\\Users\\damon\\OneDrive - North Carolina State University\\Census.csv', na = "?")
census_df = as.data.frame(census)
census_df = census_df[,2:15]

###################################
#    Exploratory Data Analysis    #
###################################

#Looking at the data ----
summary(census_df)
glimpse(census_df)

#separation ----
census_df_num = census_df[,c(1,4,10:12)]
census_df_chr = census_df[, c(2:3,5:9,13)]
target = census_df$over_50k

#Looking at the Correlation between Variables ----
cor_matrix <- cor.matrix(census_df_num)
ggcorplot(cor_matrix, census_df) #The numeric variables are uncorrelated

#correlation with the target variable -----
cor_matrix2 <- cor.matrix(target, census_df_num)

#Univariate plots ----
hist(census_df$age) #Age is right skewed
hist(census_df$education_num) #Left skewed

hist(census_df$capital_gain)#Gain has a very skewed distribution and outlier
                            # 244 individuals have 99999 with the next being 41310

hist(census$capital_loss) #large number at 0

hist(census_df$hours_week) #Very narrow distribution

#Frequency Counts ----
table(census_df$hours_week)
table(census_df$workclass)
table(census_df$martial_status)
table(census_df$occupation)
table(census_df$relationship)
table(census_df$race)
table(census_df$sex)
table(census_df$country)
table(census_df$education_levels)

#Target Variable -----
hist(census$over_50k)


################################################
######### Dimension Reduction ##################
################################################
#There are too many dimensions right now for a quality model

#New binary for capital gain above 0 and capital loss above 0-----
census_df$cap_gain = if_else(census_df$capital_gain > 0, 1, 0)
table(census_df$cap_gain)

census_df$cap_loss = if_else(census_df$capital_loss>0, 1, 0)
table(census_df$cap_loss)

#plot breaking down relationship between Capital and Over 50k -----
ggplot(census_df, aes(x=over_50k)) +
  geom_bar()+
  facet_grid(~cap_gain)

ggplot(census_df, aes(x=over_50k)) +
  geom_bar()+
  facet_grid(~cap_loss)

#compelling arguement to reduce Into different education levels ----
ggplot(census_df, aes(x = over_50k)) +
  geom_bar()+
  facet_grid(~education_levels)

#Recoding values -----
library(car)
census_df$education = census_df$education_levels
census_df$education <- Recode(census_df$education,"c('10th', '11th', '12th', '1st-4th', '5th-6th', '7th-8th', '9th', 'Preschool')='NotHSGrad'", as.factor.result=TRUE)
census_df$education <- Recode(census_df$education,"c('Assoc-acdm','Assoc-voc')='Associates'")
census_df$education <- Recode(census_df$education,"c('Doctorate', 'Masters', 'Prof-school')='AdvancedDegree'")

ggplot(census_df, aes(x = over_50k)) +
  geom_bar()+
  facet_grid(~education)

#Workclass also an opportunity to condense ----
ggplot(census_df, aes(x = over_50k)) +
  geom_bar()+
  facet_grid(~workclass)

table(census_df$workclass)

#looking into weird levels of workclass ----
subset(census_df, workclass == "Never-worked")
subset(census_df, workclass == "Without-pay")

census_df$work <- census_df$workclass
census_df$work <- Recode(census_df$work,"c('Federal-gov','Local-gov','State-gov')='Government'")
census_df$work[is.na(census_df$work)]<- "Other"
census_df$work<- Recode(census_df$work,"c('Never-worked', 'Without-pay', 'Other')='Other'")

#New Levels ----
ggplot(census_df, aes(x = over_50k)) +
  geom_bar()+
  facet_grid(~work)

#Countries ----
#intial glance, unreadable
ggplot(census_df, aes(x=over_50k)) +
  geom_bar() +
  facet_grid(~country)

table(census_df$country, target) 
census_df$region <- census_df$country
census_df$region <- Recode(census_df$region,"c('Cambodia', 'China', 'Hong', 'India', 'Iran','Japan','Laos','Philippines', 'Taiwan','Thailand','Vietnam','South','Outlying-US(Guam-USVI-etc)')='Asia + Guam'")
census_df$region <- Recode(census_df$region,"c('Columbia','Dominican-Republic','Ecuador','Peru')='South America'")
census_df$region <- Recode(census_df$region,"c('Canada','United-States')='North America'")
census_df$region <- Recode(census_df$region,"c('Cuba','Dominican-Republic','El-Salvador','Guatemala','Haiti','Honduras','Jamaica','Mexico','Nicaragua','Puerto-Rico','Trinadad&Tobago')='Central America and Islands'")
census_df$region <- Recode(census_df$region,"c('England','France','Germany','Greece','Holand-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')='Europe'")
table(census_df$region, target)

#region facet easier to see ----
ggplot(census_df, aes(x=over_50k)) +
  geom_bar() +
  facet_grid(~region)


#selecting transformed variables ----
keep = c("age","work","education","marital_status","occupation","relationship","race","sex","cap_gain","cap_loss","hours_week","region")
census_df_transform = dplyr::select(census_df, -c('workclass', 'education_levels', 'education_num', 'capital_gain','capital_loss','country','continent'))

census_df_transform = subset(census_df,select = c(over_50k, age, work, education, martial_status, occupation, relationship, race, sex, cap_gain, cap_loss, hours_week, region, education_num))
                                    

################################################
################ Sampling ######################
################################################
spec = c(train = .6, valid =.25, test = .15)

g = sample(cut(
  seq(nrow(census_df_transform)),
  nrow(census_df_transform)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(census_df_transform, g)

train = res$train
valid = res$valid
test = res$test

################################################
############# Modeling #########################
################################################

#Logistic Model ----

#A couple of passes at a model pulling in everything
train.omit = na.omit(train)
valid.omit = na.omit(valid)
test.omit = na.omit(test)

full_model <- glm(over_50k ~ age + work + education + martial_status + occupation + 
                    relationship + race + sex + cap_gain + cap_loss + hours_week + region, data=train.omit, 
                  family = binomial(link="logit"))
summary(full_model)

#null model
null_model <- glm(over_50k ~ 1, data=train.omit, family = binomial(link="logit"))
summary(null_model)

#stepwise
stepwise <- step(null_model, scope = list(upper = full_model),
     direction = "both",
     test="Chisq",
     data=train.omit,family = binomial(link="logit"))
summary(stepwise)

#forward
forward <- step(null_model, scope = list(upper = full_model),
                direction = "forward",
                test="Chisq",
                data=train.omit, family = binomial(link="logit"))
summary(forward)

#backward
backward<- step(full_model, scope = list(lower= null_model),
                direction = "backward",
                test="Chisq",
                data=train.omit, family = binomial(link="logit"))
summary(backward)

#ROC Plot
rocplot(stepwise)

stepwise.valid <- step(null_model, scope = list(upper = full_model),
                 direction = "both",
                 test="Chisq",
                 data=valid.omit,family = binomial(link="logit"))
rocplot(stepwise.valid)
#Same AIC of each model due to all of the variables being significant

#Model Classification ----

stepwise.pred <- predict(stepwise, type ="link")
contrasts(as.factor(train$over_50k))

#training Set
stepwise.pred$adj <- stepwise.pred[stepwise.pred >.5]= 1
stepwise.pred$adj <- stepwise.pred[stepwise.pred <.5]= 0
table(stepwise.pred$adj, as.factor(train.omit$over_50k))
mean(stepwise.pred$adj == train.omit$over_50k)

#Validation set
stepwise.valid <- predict(stepwise, newdata=valid.omit, type="response")
stepwise.valid[stepwise.valid >.5] =1
stepwise.valid[stepwise.valid <.5] =0
table(stepwise.valid,as.factor(valid.omit$over_50k))
mean(stepwise.valid == valid.omit$over_50k)

#Test Set
stepwise.test <- predict(stepwise, newdata=test.omit, type="link")
contrasts(as.factor(test$over_50k))
stepwise.test[stepwise.test >.5] = 1
stepwise.test[stepwise.test <.5]= 0
table(stepwise.test, as.factor(test.omit$over_50k))
mean(stepwise.test == test.omit$over_50k)

#Classification Tree ----
library(rpart)
library(rpart.plot)
tree <-rpart(over_50k ~ ., data=train, method="class") 
printcp(tree)
plotcp(tree)

rpart.plot(tree,type = 1, fallen.leaves=TRUE ,leaf.round=0, cex = .75)

valid_tree <- predict(tree,valid, type="class")
table(valid_tree, valid$over_50k)
valid_misclass = sum(valid_tree != valid$over_50k)/nrow(valid)
print(valid_misclass)

test_tree <- predict(tree, test, type = "class")
table(test_tree, test$over_50k)
test_misclass = sum(test_tree != test$over_50k)/nrow(test)
print(test_misclass)

#################################
######GG Plots###################
#################################

#
capital_gain <- census_df$capital_gain
capital_gain[capital_gain == 99999] <- 50000

#Jitter Plot showing relationship betwee education, capital gain, and over_50k

jitter<- ggplot(census_df_transform, aes(x= education_num, y =capital_gain, color=as.factor(over_50k )))+
    geom_jitter()+
    ggtitle("Scatter Plot of Education Against Capital Gains")+
    xlab("Level of Education")+
    ylab("Capital Gains ($)")+
    scale_color_manual(name = "Over 50k", values = c("red", "green4"), breaks = c(0,1), labels = c("No", "Yes"))

#transforming variables
census_df_transform2 <- census_df_transform %>% group_by(region) %>% mutate(proportion = mean(over_50k))
testing<- census_df_transform %>% group_by(region) %>% summarise(proportion = mean(over_50k))

#Bar plot showing proportion of individuals making 50k or more
proportion <- ggplot(testing, aes(region,proportion, fill=region) )+
  geom_col()+
  ggtitle("Proportion of Individuals making 50k by Region")+
  xlab("Region")+
  ylab("Proportion of Individuals making 50k")+
  scale_fill_discrete(name = "Geographic Region")

#Bar plot showing break down of education, sex, and over_50k
bar <- ggplot(census_df_transform, aes(x=over_50k, fill=sex))+
    geom_bar()+
    facet_grid(~education)+
    ggtitle("Education by Region")+
    xlab("Income Over 50k (1 for Yes, O for No)")+
    ylab("Count of individuals")+
    scale_fill_discrete(name="Sex")

#Combines all of the graphs into one effective image -----
grid.arrange(jitter,proportion, bar)
multiplot(jitter, proportion, bar, cols=2)
