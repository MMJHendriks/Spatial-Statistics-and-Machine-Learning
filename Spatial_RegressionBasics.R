#Required Packages 
library(rJava)
library(xlsx)
library(fmsb)
library(base)

#Reading the dataset
income_data <- read.xlsx("C:/Users/mirth/Documents/Spatial/income.xlsx",sheetName = "Dataset")

#we check the variables
variables<-names(income_data)
#[1] "Income"            "Persons"           "Dwelling_type"     "Cars"              "Bicycles"         
#[6] "Children.Bicycles" "Males"             "Females"           "ED_Primary"        "ED_Middle"        
#[11] "ED_Tech_Voc"       "ED_University"     "ACT_Working"       "ACT_Students"      "ACT_Pensioner"    
#[16] "ACT_House"         "ACT_Jobless"       "ACT_Other"         "SCHDL_Fulltime"    "SCHDL_Parttime"   
#[21] "SCHDL_Occasional"  "SCHDL_WE"   
as.data.frame(table(income_data$Dwelling_type))
as.data.frame(table(income_data$Cars))

#identify the nature of variables
for(variables in income_data){
  print(class(variables))
  }

#identify whether some variables are perfectly correlated 
cor_matrix<-cor(income_data)
cor_matrix
cor_matrix>=1

VIF1=VIF(lm(Persons~ACT_Working+SCHDL_Fulltime, data=income_data))
VIF1
VIF2=VIF(lm(ACT_Working~Persons+SCHDL_Fulltime, data=income_data))
VIF2
VIF3=VIF(lm(SCHDL_Fulltime~ACT_Working+Persons, data=income_data))
VIF3

VIF4=VIF(lm(Persons~Dwelling_type+Cars+ACT_Working+ACT_Students+SCHDL_Fulltime, data=income_data))
VIF4
VIF5=VIF(lm(Dwelling_type~Persons+Cars+ACT_Working+ACT_Students+SCHDL_Fulltime, data=income_data))
VIF5
VIF6=VIF(lm(Cars~Persons+Dwelling_type+ACT_Working+ACT_Students+SCHDL_Fulltime, data=income_data))
VIF6
VIF7=VIF(lm(ACT_Working~Persons+Dwelling_type+Cars+ACT_Students+SCHDL_Fulltime, data=income_data))
VIF7
VIF8=VIF(lm(SCHDL_Fulltime~Persons+Dwelling_type+Cars+ACT_Students+ACT_Working, data=income_data))
VIF7


#check variables correctly coded, inconsistencies
summary(income_data)
#no person has 99 bicycles or children.bicycles
sort(income_data$Bicycles, decreasing = TRUE)
sort(income_data$Children.Bicycles, decreasing = TRUE)

#listwise deletion (children.)bycycles 99, 98 i.e. NA
income_data$Bicycles[income_data$Bicycles==99] <- NA
income_data$Bicycles[income_data$Bicycles==98] <- NA
income_data$Children.Bicycles[income_data$Children.Bicycles==99] <- NA
income_data$Children.Bicycles[income_data$Children.Bicycles==98] <- NA
income_dat<-na.omit(income_data)
summary(income_dat)

#recode categorical variable Dwelling type
Dwel_1 <- ifelse(income_dat$Dwelling_type == 1, 1, 0)
Dwel_2 <- ifelse(income_dat$Dwelling_type == 2, 1, 0)
Dwel_3 <- ifelse(income_dat$Dwelling_type == 3, 1, 0)
Dwel_4 <- ifelse(income_dat$Dwelling_type == 4, 1, 0)
Dwel_5 <- ifelse(income_dat$Dwelling_type == 5, 1, 0)
Dwel_6 <- ifelse(income_dat$Dwelling_type == 6, 1, 0)
Dwel_7 <- ifelse(income_dat$Dwelling_type == 7, 1, 0)

# check is other variables need to be recoded
as.data.frame(table(income_data$Dwelling_type))
Dwel_35 <- ifelse(income_dat$Dwelling_type==3 |income_dat$Dwelling_type==5, 1, 0)
Dwel_467 <- ifelse(income_dat$Dwelling_type==4 |income_dat$Dwelling_type==6|income_dat$Dwelling_type==7, 1, 0)

as.data.frame(table(income_data$Cars))
nocar <- (income_dat$Cars==0)+0 
onecar <- (income_dat$Cars==1)+0
twocar <- (income_dat$Cars==2)+0 
more3cars <- (income_dat$Cars>=3)+0 

#make the model 
model1 <- lm(Income~Persons, data=income_dat)
summary(model1)
model2 <- lm(Income~Persons+Dwel_1, data=income_dat)
summary(model2)
model3 <- lm(Income~Persons+Dwel_1+Dwel_2, data=income_dat)
summary(model3)
model4 <- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3, data=income_dat)
summary(model4)
model4_2 <- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_35, data=income_dat)
summary(model4_2)  #both dwel_3 and dwel_35 are significant, but the R^2 is actually lower so continue using Dwel3
model5 <- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Dwel_4, data=income_dat)
summary(model5)  #with a one-sided t-test dwel_4 might still be significant 
model6 <- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Dwel_4+Dwel_5, data=income_dat)
summary(model6) #clear that dwel_4 and 5 are not significant, so delete them from the model 
model7 <- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Dwel_6, data=income_dat)
summary(model7) # not significant so take out dwel_6
model8 <- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Dwel_7, data=income_dat)
summary(model8) # not significant so take out dwel_7
model8_2 <- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Dwel_467, data=income_dat)
summary(model8_2) #adding these (smaller sample size) dwelling types together as one variable to model still not sign

model9<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars, data=income_dat)
summary(model9)
model9_2<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+nocar, data=income_dat)
summary(model9_2)
model9_3<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+onecar+twocar+more3cars, data=income_dat)
summary(model9_3)

model10<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles, data=income_dat)
summary(model10)
model11<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Children.Bicycles, data=income_dat)
summary(model11)
model12<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Children.Bicycles+Females, data=income_dat)
summary(model12)  #added females which is significant,can't also add males
model13<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Children.Bicycles+Females+ED_Primary, data=income_dat)
summary(model13)
model14<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Children.Bicycles+Females+ED_Primary+ED_Middle, data=income_dat)
summary(model14)
model15<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Children.Bicycles+Females+ED_Primary+ED_Middle+ED_Tech_Voc, data=income_dat)
summary(model15)  
model16<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Children.Bicycles+Females+ED_Primary+ED_Middle+ED_Tech_Voc+ED_University, data=income_dat)
summary(model16)  #children.bicyles not sign anymore 
model16_2<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Primary+ED_Middle+ED_Tech_Voc+ED_University, data = income_dat)
summary(model16_2)

model17<- lm(Income~Persons+Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Primary+ED_Middle+ED_Tech_Voc+ED_University
             +ACT_Working, data=income_dat)
summary(model17)  #persons not sign anymore, so remove this variable
model18<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Primary+ED_Middle+ED_Tech_Voc+ED_University
             +ACT_Working+ACT_Students, data=income_dat)
summary(model18)  #ED_Primary and ED_Middle no longer significant so take out
                  #females also not sign, but leave in for theoretical reasons
model18_2<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working +ACT_Students, data=income_dat)
summary(model18_2)

model19<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working +ACT_Students
             +ACT_Pensioner, data=income_dat)
summary(model19)

model20<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House, data=income_dat)
summary(model20)
model21<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless, data=income_dat)
summary(model21)
model22<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless+ACT_Other, data=income_dat)
summary(model22) #ACT_Other not significant 

model23<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless+SCHDL_Fulltime, data=income_dat)
summary(model23) 
model24<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime, data=income_dat)
summary(model24) 
model25<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime+SCHDL_Occasional, data=income_dat)
summary(model25) #SCHDL_Occasional not significant 
model26<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime+SCHDL_WE, data=income_dat)
summary(model26) #SCHDL_WE not significant 

model<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime, data=income_dat)
summary(model)  # all variables are clearly significant 

modela<- lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+I(ACT_Working^2)+ACT_Students+ACT_Pensioner
           +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime, data=income_dat)
summary(modela)  # all variables are clearly significant 


##################
# Check the assumptions of the model
#plot everything direclty
plot(model)

#First Plotting residuals vs predictions
res=resid(model)
pre=predict(model)
plot(res~pre)

#Plotting histogram
hist(res)
x <- -4:4
lines(x, 99*0.7*dnorm(x,0,sd(res)),col=2)

#Calculation VIF
VIF1=VIF(lm(Income~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF1=VIF(lm(Dwel_1~Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF2=VIF(lm(Dwel_2~Dwel_1+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF3=VIF(lm(Dwel_3~Dwel_1+Dwel_2+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF4=VIF(lm(Cars~Dwel_1+Dwel_2+Dwel_3+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF5=VIF(lm(Bicycles~Dwel_1+Dwel_2+Dwel_3+Cars+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF6=VIF(lm(Females~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF7=VIF(lm(ED_Tech_Voc~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF8=VIF(lm(ED_University~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF9=VIF(lm(ACT_Working~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF10=VIF(lm(ACT_Students~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF11=VIF(lm(ACT_Pensioner~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF12=VIF(lm(ACT_House~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF13=VIF(lm(ACT_Jobless~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+SCHDL_Fulltime+SCHDL_Parttime,data=income_dat))
VIF14=VIF(lm(SCHDL_Fulltime~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Parttime,data=income_dat))
VIF15=VIF(lm(SCHDL_Parttime~Dwel_1+Dwel_2+Dwel_3+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
             +ACT_House+ACT_Jobless+SCHDL_Fulltime,data=income_dat))
VIF1
VIF2
VIF3
VIF4
VIF5
VIF6
VIF7
VIF8
VIF9
VIF10
VIF11
VIF12
VIF13
VIF14
VIF15
VIF_Model<-VIF(model)
VIFS<-cbind(VIF1, VIF2, VIF3,VIF4,VIF5, VIF6, VIF7, VIF8,VIF9,VIF10,VIF11,VIF12,VIF13,VIF14,VIF15,VIF_Model)
VIFS

#Searching for outlier (cook distance)
cookd=cooks.distance(model)
which(cookd>0.025)
outliers<-which(cookd>0.025)
ooutliers<-c(5112,5945,6052,6057,6271,6291,6357,6369,9314,10812,5100,5907,6008,6011,6221,6241,6304,6315,9246,10737) 
ooutliers
plot(cookd)

income_dato<-income_dat[-c(ooutliers),]

Dwel_1o <- ifelse(income_dato$Dwelling_type == 1, 1, 0)
Dwel_2o <- ifelse(income_dato$Dwelling_type == 2, 1, 0)
Dwel_3o <- ifelse(income_dato$Dwelling_type == 3, 1, 0)

modelo<- lm(Income~Dwel_1o+Dwel_2o+Dwel_3o+Cars+Bicycles+Females+ED_Tech_Voc+ED_University+ACT_Working+ACT_Students+ACT_Pensioner
            +ACT_House+ACT_Jobless+SCHDL_Fulltime+SCHDL_Parttime, data=income_dato)
summary(modelo)  # all variables are clearly significant 
plot(modelo)

#female, single person, dwelling_mortgage, finished uni, car, no bicylce, work full-time


