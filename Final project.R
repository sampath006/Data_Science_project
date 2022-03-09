#checking working directory
getwd()

#reading csv file place in same working directory and
#set current working directory to folder

da<-read.csv(file.choose(),header=TRUE)


#checking data summary
summary(da)

#checking total number of NA
sapply(da, function(x) sum(is.na(x)))

# from summary we can observe there are some outliers exist so I am replacing NA with median values of country wise
for (i in unique(da$Country)){
  for (j in 1:ncol(da[da$Country == i,])){
    da[da$Year == i,][,j] = ifelse(is.na(da[da$Year == i,][,j]),ave(da[da$Year == i,][,j],FUN = function(x)median(x,na.rm = TRUE)),da[da$Year == i,][,j])
    
  }
}
sapply(da, function(x) sum(is.na(x)))
# I cannot see much improvement this may be because of countries data for all null values are null for every year and first value is null for each year

for (i in unique(da$Year)){
  for (j in 1:ncol(da[da$Year == i,])){
    da[da$Year == i,][,j] = ifelse(is.na(da[da$Year == i,][,j]),ave(da[da$Year == i,][,j],FUN = function(x)median(x,na.rm = TRUE)),da[da$Year == i,][,j])
    
  }
}
sapply(da, function(x) sum(is.na(x)))


for(i in 4:ncol(da)){
  boxplot(da[,i],main=colnames(da)[i])
}

da = da[da$infant.deaths < 1001,]
da = da[da$Measles < 1001,]
da = da[da$under.five.deaths < 1001,]

da$BMI = NULL


n=nrow(da)



# calculating degree of corelation for Life Expectancy and Adult Mortality



x=da["Life.expectancy"]
y=da["Adult.Mortality"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Adult.Mortality)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom

r

#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#plot

plot(da$Life.expectancy,da$Adult.Mortality,xlab='Life expectancy',ylab='Adult mortality')


# calculating degree of corelation for Life Expectancy and infant Deaths
x=da["Life.expectancy"]
y=da["infant.deaths"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$infant.deaths)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom

r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#plot

plot(da$Life.expectancy,da$infant.deaths,xlab='Life expectancy',ylab='Infants deaths')


# calculating degree of corelation for Life Expectancy and Alcohol



x=da["Life.expectancy"]
y=da["Alcohol"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Alcohol)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance


#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#plot

plot(da$Life.expectancy,da$Alcohol,xlab='Life expectancy',ylab='Alcohol')

# calculating degree of corelation for Life Expectancy and Percentage Expenditure



x=da["Life.expectancy"]
y=da["percentage.expenditure"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$percentage.expenditure)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#plot

plot(da$Life.expectancy,da$percentage.expenditure,xlab='Life expectancy',ylab='Percentage Expenditure')



# calculating degree of corelation for Life Expectancy and Hepatitis B



x=da["Life.expectancy"]
y=da["Hepatitis.B"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Hepatitis.B)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$Hepatitis.B,xlab='Life expectancy',ylab='Hepatitis.B')


# calculating degree of corelation for Life Expectancy and Measles

x=da["Life.expectancy"]
y=da["Measles"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Measles)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom 



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$Measles,xlab='Life expectancy',ylab='Measles')


# calculating degree of corelation for Life Expectancy and under-five deaths 



x=da["Life.expectancy"]
y=da["under.five.deaths"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$under.five.deaths)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$under.five.deaths,xlab='Life expectancy',ylab='under five deaths')


# calculating degree of corelation for Life Expectancy and polio



x=da["Life.expectancy"]
y=da["Polio"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Polio)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$Polio,xlab='Life expectancy',ylab='Polio')


# calculating degree of corelation for Life Expectancy and Total Expenditure



x=da["Life.expectancy"]
y=da["Total.expenditure"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Total.expenditure)
test
n=da.


sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT 

plot(da$Life.expectancy,da$Total.expenditure,xlab='Life expectancy',ylab='Total expenditure')


# calculating degree of corelation for Life Expectancy and Diphtheria



x=da["Life.expectancy"]
y=da["Diphtheria"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Diphtheria)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$Diphtheria,xlab='Life expectancy',ylab='Diphtheria')


# calculating degree of corelation for Life Expectancy and HIV/AIDS



x=da["Life.expectancy"]
y=da["HIV.AIDS"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$HIV.AIDS)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance


#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$HIV.AIDS,xlab='Life expectancy',ylab='HIV AIDS')

# calculating degree of corelation for Life Expectancy and gdp



x=da["Life.expectancy"]
y=da["GDP"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$GDP)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance


#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$GDP,xlab='Life expectancy',ylab='GDP')


# calculating degree of corelation for Life Expectancy and Population



x=da["Life.expectancy"]
y=da["Population"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Population)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t

df=n-2;

#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$Population,xlab='Life expectancy',ylab='Population')


# calculating degree of corelation for Life Expectancy and thinness..1.19.years



x=da["Life.expectancy"]
y=da["thinness..1.19.years"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$thinness..1.19.years)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance


#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$thinness..1.19.years,xlab='Life expectancy',ylab='thinness..1.19.years')

# calculating degree of corelation for Life Expectancy and thinness.5.9.years



x=da["Life.expectancy"]
y=da["thinness.5.9.years"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$thinness.5.9.years)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance


#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$thinness.5.9.years,xlab='Life expectancy',ylab='thinness.5.9.years')

# calculating degree of corelation for Life Expectancy and Income.composition.of.resources



x=da["Life.expectancy"]
y=da["Income.composition.of.resources"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Income.composition.of.resources)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t
df=n-2;

# at 95% significance


#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT

plot(da$Life.expectancy,da$Income.composition.of.resources,xlab='Life expectancy',ylab='Income.composition.of.resources')

# calculating degree of corelation for Life Expectancy and Schooling



x=da["Life.expectancy"]
y=da["Schooling"]


result<-cor(x,y,method = "pearson")
test <- cor.test(da$Life.expectancy, da$Schooling)
test



sigma_x=sum(x)/n
sigma_y=sum(y)/n

x1=(x-sigma_x)

y1=(y-sigma_y)

x2=sum(x1 *x1)
y2= sum(y1 *y1)
num=sum(x1*y1)
denom=sqrt(x2* y2)



r=num/denom



r


#-----------------------Do the significance test  here we are using t test------------

t=(r/(sqrt(1-(r*r))))*(sqrt(n-2))
t



#---calculate-----coefficient-of-determination ------------------------

r_square = r*r
r_square

#PLOT
plot(da$Life.expectancy,da$Schooling,xlab='Life expectancy',ylab='Schooling')

