# Linear-regression

getwd()
setwd('D://DataScience//DS//ML in R//Code snippets')

train <- read.csv(file.choose()) 

#### data filtering

set.seed(2)
s=sample(1:nrow(train),0.88*nrow(train))
ld_train <- train[s,]
ld_test <- train[-s,]

ld_test_real <- ld_test

ld_test <- ld_test %>% select(-Interest.Rate)

ld_test$Interest.Rate=NA

ld_train$data = 'train'
ld_test$data = 'test'

ld_all=rbind(ld_train,ld_test)

# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL

library(dplyr)

glimpse(ld_all)

ld_all <- ld_all %>% 
  mutate(Interest.Rate=as.numeric(gsub('%','',Interest.Rate)),
         Debt.To.Income.Ratio=as.numeric(gsub('%','',Debt.To.Income.Ratio)),
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines),
         Amount.Requested=as.numeric(Amount.Requested),
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
         )

glimpse(ld_all)

table(ld_all$Loan.Length)

ld_all <- ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=='36 months')) %>% 
  select(-Loan.Length)

table(ld_all$Loan.Purpose)

round(tapply(ld_all$Interest.Rate, ld_all$Loan.Purpose, mean,na.rm=T))

ld_all <- ld_all %>% 
  mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
         lp_11=as.numeric(Loan.Purpose %in% c('major_purchase','medical','car')),
         lp_12=as.numeric(Loan.Purpose %in% c('vacation','wedding','home_improvement')),
         lp_13=as.numeric(Loan.Purpose %in% c('other','small_business','credit_card')),
         lp_14=as.numeric(Loan.Purpose %in% c('debt_consolidation','house','moving'))) %>% 
  select(-Loan.Purpose)

CreateDummies <- function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for (cat in categories){
    name=paste(var,cat,sep='_')
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(ld_all)

ld_all <- CreateDummies(ld_all,'State',100)
ld_all <- CreateDummies(ld_all,'Home.Ownership',100)

library(tidyr)

ld_all <- ld_all %>% 
  separate(FICO.Range,into = c('f1','f2'),sep = '-') %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)

ld_all <- CreateDummies(ld_all,'Employment.Length',100)

# checking for NA values in the Dataframe

lapply(ld_all, function(x) sum(is.na(x)))

ld_all <- ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c('data','Interest.Rate'))){
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[ld_all$data=='train',col],na.rm = T)
  }
}

glimpse(ld_all)

# separate train and test data

ld_train = ld_all %>% filter(data=='train') %>% select(-data)
ld_test = ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)

set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1 <- ld_train[s,]
ld_test1 <- ld_train[-s,]

fit <- lm(Interest.Rate~.-ID,data = ld_train1)

summary(fit)

library(car)

# we'll take vif cutoff as 5

vif(fit)

sort(vif(fit),decreasing = T)

fit=lm(Interest.Rate~.-ID-lp_14,data = ld_train1)

sort(vif(fit),decreasing = T)

# p-Value take the cutoff 0.05

summary(fit)

fit <-step(fit)

# AIC score, what this step funtion does

summary(fit)

formula(fit)

fit=lm(Interest.Rate ~ Monthly.Income + Inquiries.in.the.Last.6.Months + 
         ll_36 + lp_11 + lp_12 + State_TX + Home.Ownership_MORTGAGE + 
         fico + Employment.Length_3years + Employment.Length_2years= ld_train1)

summary(fit)

val.pred <- predict(fit,newdata = ld_test1)

errors = ld_test1$Interest.Rate-val.pred

errors**2 %>% mean() %>% sqrt()

# model for prediction on the entire data 

fit.final=lm(Interest.Rate~.-ID,
             data = ld_train)

sort(vif(fit.final),decreasing = T)

fit.final <- step(fit.final)

summary(fit.final)

formula(fit.final)


fit.final=lm(Interest.Rate ~ Monthly.Income + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + 
               ll_36 + lp_11 + lp_12 + State_TX + Home.Ownership_MORTGAGE + 
               fico + Employment.Length_10years,
             data = ld_train)

summary(fit.final)

test.pred = predict(fit.final,newdata = ld_test)

write.csv(test.pred,'submission_loan data result.csv',row.names = F)

########################### Testing the result ########################

ld_test_real$Interest.Rate

ld_test_real <- ld_test_real %>% 
  mutate(Interest.Rate=as.numeric(gsub('%','',Interest.Rate)),
         Debt.To.Income.Ratio=as.numeric(gsub('%','',Debt.To.Income.Ratio)),
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines),
         Amount.Requested=as.numeric(Amount.Requested),
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )

glimpse(ld_test_real)

table(ld_test_real$Loan.Length)

ld_test_real <- ld_test_real %>% 
  mutate(ll_36=as.numeric(Loan.Length=='36 months')) %>% 
  select(-Loan.Length)

table(ld_test_real$Loan.Purpose)

round(tapply(ld_test_real$Interest.Rate, ld_test_real$Loan.Purpose, mean,na.rm=T))

ld_test_real <- ld_test_real %>% 
  mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
         lp_11=as.numeric(Loan.Purpose %in% c('major_purchase','medical','car')),
         lp_12=as.numeric(Loan.Purpose %in% c('vacation','wedding','home_improvement')),
         lp_13=as.numeric(Loan.Purpose %in% c('other','small_business','credit_card')),
         lp_14=as.numeric(Loan.Purpose %in% c('debt_consolidation','house','moving'))) %>% 
  select(-Loan.Purpose)

glimpse(ld_test_real)

ld_test_real <- CreateDummies(ld_test_real,'State',100)
ld_test_real <- CreateDummies(ld_test_real,'Home.Ownership',100)

library(tidyr)

ld_test_real <- ld_test_real %>% 
  separate(FICO.Range,into = c('f1','f2'),sep = '-') %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)

ld_test_real <- CreateDummies(ld_test_real,'Employment.Length',100)

# checking for NA values in the Dataframe

lapply(ld_test_real, function(x) sum(is.na(x)))

ld_test_real <- ld_test_real[!(is.na(ld_test_real$ID)),]

for(col in names(ld_test_real)){
  if(sum(is.na(ld_test_real[,col]))>0){
    ld_test_real[is.na(ld_test_real[,col]),col]=mean(ld_test_real[,col],na.rm = T)
  }
}

glimpse(ld_test_real)

val.pred <- predict(fit,newdata = ld_test)

errors = ld_test_real$Interest.Rate-val.pred

errors**2 %>% mean() %>% sqrt()




plot(fit.final,1) # residual vs fittle values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

############

output<- summary(fit.final)
names(output)

output$coefficients[,4]

