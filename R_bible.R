#comma problem omit read csv
temp=read.csv(file.name [i], sep = ',', stringsAsFactors = FALSE)
temp = apply(temp, 2, gsub, pattern = ',', replacement = '')
temp = apply(temp, 1:2, as.numeric)

gsub('1,26546,64.0', pattern = ',', replacement = '')

df = apply(df, 1:2, gsub, pattern = ',', replacement = '')


#set wd to location of R script source code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls(all.names = TRUE))

#saving all global env. as data file:
save.image()
#saving specific variable as .rdata file and then loading it:
save(mydata, file="mydata.rdata")
load("mydata.rdata")
#IMPPPPP#########################################################################
#p-value>5% =>ACCEPT H0 in 95% confidence level (CL)

#CL=95% means that if we accept H0 (by p>5%), the prob. of error would be 5%
#CL=90% means that if we accept H0 (by p>10%), the prob. of error would be 10%

#Tails are REJECT zone of H0

#MIDDLE is ACCEPT ZONE of H0
#MIDDLE is ACCEPT ZONE of H0
#MIDDLE is ACCEPT ZONE of H0

#example:
#H0 is linear regression models (y=a+bx) is that a=0 and b=0 so high pvalue accept it so we should remove x 
#from modeling if pvalue of b is high
#########################################################################
# text to date: https://www.statmethods.net/input/dates.html#:~:text=You%20can%20use%20the%20as,format%20gives%20the%20appropriate%20format.
as.Date('1980 Jan, 01',format='%Y %b, %d')

##################################################
#Pivot table 
library(tidyr)
library(dplyr)
year=c(1980,1980,1980,1981,1981,1981)
month=c(1,2,2,1,1,2)
value=rnorm(6,10,1)
df=as.data.frame(cbind(year,month,value))
df
colnames(df)=c('c_Yera','c_Month','c_Value')
pivot_month=
  df %>%  #df should be data frame                   
  select(c_Yera,c_Month,c_Value) %>% #put desired colnames 
  group_by(c_Yera, c_Month) %>%    #defines columns you want to have    
  summarise(c_Value_new=mean(c_Value)) #c_Value_new would be the name of new col in pivot table
pivot_month
pivot_year=
  df %>%  #df should be data frame                   
  select(c_Yera,c_Value) %>% #put desired colnames 
  group_by(c_Yera) %>%    #defines columns you want to have    
  summarise(c_Value_new=mean(c_Value)) #c_Value_new would be the name of new col in pivot table
pivot_year
View(pivot_month)
View(pivot_year)
###################################################
#shortcuts:
#https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts

# CTRL + SHIFT + C to comment multiple lines
#Data frame is modifiabel but matrix is not.
#if working by dataframe and NA is generated, the problem is in reading inputs. one reason can be , as thousand seprtator:
#when you have , as thousand seperator:
as.numeric(gsub("," ,"", '1,256.32'))

#Insert code section	Ctrl+Shift+R
#fold all sections: alt+o
#unfold all sections: shift+alt+o
##################################################
#Introduction

#John Chambers 1976 developed S language which is basic of R in 1991.
# CRAN: Comprehensive R Archive Network, https://cran.r-project.org
#R does not need indentation of codes and sub codes

#R Studio is downloadable from 
#https://rstudio.com/products/rstudio/download/

#Click on TOOLS >> CHECK FOR PACKAGE UPDATES

setwd('C://1.Research Files//R_Bible')   #use // istead of \ from windows default
library() # list all available packages
data() # list all available datasets

require(ggplot2)
require(reshape2)
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 

windowsFonts(A = windowsFont("Times New Roman"))
op=par(family='A')
#between them all text is in A format
par(op)
#to get help
#?command name
#help(command name)
#example(command name) #executes the example written in help of command

#cleaning console Ctrl+L
#pree 'Esc' to remove + in console.
#ctrl+alt+r run all of script
#############################################
#Variables
x=Inf
x=3
x<-4
assign('x',5) # x came as a text

# %/% integer division
# %% mod of division
# ^ or ** is exponent
# %*% matrix multiplying

x=9
y=3
x%/%y
x%%y
x**y


#boolean operators
x==y
x!=y
x<=y
x>=y

class(x)  # numeric, character, boolean, date,...
is.numeric(x)
is.character(x)
is.integer(x)

x=5.5
as.integer(x)
is.integer(x)
print(x)
print(as.integer(x))

###############################################
#Vectors
x=c(4:6,5,6)
x=seq(1,100,10)
x=1:10
x=10:1
x=rep(1,10)
x=rep('x',10)
length(x)

##################################################
#Data formats
col1=101:103
col2=104:106
col3=107:109
q=data.frame(col1,col2,col3,row.names = c('r1','r2','r3')) #it has rows and cols name but matrix does not
q

names(q) #gives only col names

q[,1]
q $ col1 #=q[,1], you can call a whol col by $ sign.

#Exporting data: x is the variable you want to export
write.table(x = q,file='qq.txt',sep=',') 

nrow(q)
ncol(q)
dim(q)
names(q)  #gives the col (attributes) names
head(q)   #some first rows of the frame

#list can contain different data from different lenghth and types
x1x2='a'
list_me=list(q,x1x2)
list_me[1]
list_me[2]

#Martix
matrix_me=matrix(1:10,2,5)
matrix_me[1,2] #give a cell of matrix
matrix_me[3]   # =matrix_me[1,2], first it counts all of rows and them cols
matrix_me[,2]  #all col2
matrix_me[2,]  #all row2

t(matrix_me) #transpose of a matrix

attributes(matrix_me)
dim(matrix_me)
nrow(matrix_me)
ncol(matrix_me)
colSums(matrix_me)


#is it mat/frame/vec?
is.data.frame(q)
is.matrix(q)
is.vector(q)

#Subsetting
x=runif(100,0,1000)
x[x>25] #inside [] should be a boolean statement so that it can seperate them if it is true. 
x_sub1=x[x<25 | x>900]
x_sub2=x[x<100 & x>0]
plot(x)
abline(200,0) #plot line with b as slope and a as intercept

plot(x_sub2)


a=matrix(1:12,2)
a
a[,c(-1,-2)] # it means do not show col 1 and 2

col1=101:103
col2=104:106
col3=c(107,107,109)
q=data.frame(col1,col2,col3,row.names = c('r1','r2','r3')) 
q #q should be data frame

mean(q$col1[q$col3==107]) # it means that mean col1 of q when col3 is 107
                          #inside [] should be boolean
#################################################
#Importing data: reading data
library(readxl)
data()
data("iris")    #calling a data source
head(iris)     

#reading data from internet
data_url = "http://www.jaredlander.com/data/Tomato%20First.csv"
tomato=read.table(file = data_url,header = T, sep = ',') #file should be a text which will be read from setwd().
head(tomato)

write.table(tomato,file = 'tomato_file.csv',sep = ',')

Inflation=read_excel("Inflation.xlsx") 
head(Inflation)
summary(Inflation)
str(Inflation)

#summary of column
summary(1962)  #1962 is column in inflation

#Reading a big data
#crime=read.csv(file = 'Crimes_-_2001_to_present.csv')
#View(crime)
#nrow(crime)

View(iris)
d=iris
windows()   #plotting in seperate windows
plot(d)

d_sub=iris[,c(-5)]
cor(d_sub)

#box plotting
observ=c(1,2,3,4,5)
class=c(1,1,2,2,1)
plot(observ~class)
boxplot(observ~class)

#ggplot
require(ggplot2)
df=data.frame(c(1:10))
df$class=as.character(c(1,1,1,1,-1,-1,-1,2,2,2)) #it should be charachter!!!
df$value=runif(n = 10)
ggplot(data = df, aes(x=class, y=value)) +
  geom_boxplot(aes(fill=value))+
  xlab('X')+ylab('Y')+
  scale_x_discrete(labels=c("1" = "a", 
                            "-1" = "b",
                            "2" = "c"))
############################################################################################
#Plotting
x=1:10
y=1:10
z=21:30

#windows() #having a plot in different windows: some times this command solves the problem of plotting 
par(mfrow=c(1,1)) #having different plots next to eachohter
plot(x,y)
plot(x,z)

legend("bottomright",
       c("a","b",'c','d'),
       fill=c("red","blue",'orange','black'),bty = "n")

#fit line in plot
x <- 1:10
y <- c(2,4,6,8,7,12,14,16,18,20)
lo <- loess(y~x)
plot(x,y)
lines(predict(lo), col='red', lwd=2)

#adding line to a plot: first arg is interc. and second is slope
abline(0,1)  
abline(4,1.1)

############################################################################################
#Statistics
#Uniform dist.
x=runif(100,-1,1) #random generation
plot(x)

#Normal dist.
x=rnorm(100,0,1)    #random values which have normal dist.
p=dnorm(x,0,1)      #pdf of x by normal dist.
q=pnorm(x,0,1)      #cdf of x by normal dist.
x_ver=qnorm(q,0,1)  #inverse cdf for normal dist.

plot(x,q)
plot(x,p)
summary(x_ver)

#chi-square and t dist.
#rt/ rchisq     random variable 
#pt/ pchisq     pdf
#pt/ dchisq     cdf
#qt/ qchisq     cdf-1

#sampling from distributions:
#Method1:
x=rchisq(n=1000,df=10)
steps=seq(0.01,0.99,0.01)
q1=quantile(x = x,probs = steps)  #you have to defien x
plot(q1)

#Method2:
q2=qchisq(steps,df=10)  # no need to define x just steps is enough
plot(q2)


mean(x)
sd(x)
var(x)
min(x)
max(x)
range(x)
median(x)
sort(x)
summary(x)
abs(x)
str(x)

q_short=quantile(x)  #gives inverse CDF in 4 famous probs
q_long=quantile(x,probs = seq(0,1,0.1)) #gives inverse CDF in desired probs as a vector

#sampling
p=rep(0.1,10)
y=sample(x = c(1:10),size = 3,replace =T,prob=rep(0.1,10)) #the sum of prob vector should be one and its size same as x 

y=sample(x = unique(c(1,1,2,3,4,5,6,5,7,8,9,11)),size = 3)
x = c(1,1,2,3,4,5,6,5,7,8,9,11)


id<-c(1,1,2,3,3)
date<-c("23-01-08","01-11-07","30-11-07","17-12-07","12-12-08")
df<-data.frame(id,date)
df$date2<-as.Date(as.character(df$date), format = "%d-%m-%y")

df[match(sample(unique(df$id),2),df$id),]

#t-test
cls=c(1,1,1,2,2,2)
obs=c(101,102,103,10,11,12)
data=data.frame(cls,obs)

class1=data$obs[data$cls==1]
class2=data$obs[data$cls==2]

#check the mean of sample is as the mean of population
#end of tails are reject zone of NULL hypo. (H0) and the middle is not rejecting 
#zone of H0 so higher pvalue than conf.level mean not rejecting H0

#pvalue>conf. interval=>ACCEPT H0
#Tails are REJECT zone of H0

#H0 is linear regression models (y=a+bx) is that a and b=0 so high pvalue accept so we should remove x 
#if pvalue of b is high


t.test(x=class1,y=NULL,mu=12,conf.level = 0.025) #it checks whether the population mean in 12 or not
                                                 #here p values is very low so the H0 is rejected
t.test(x=class1,y=class2,conf.level = 0.025) #checks whether the mean of two groups are same

#Normality Test: check whether our data is normal. H0; data is normal
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))


#Wilcox-test: checks how similar two data sets are. H0: means are equal
x1=rnorm(100)
x2=rnorm(100)
wilcox.test(x1,x2)

#How to normal data:
#box-cox 
#other methods: in session 7 of videos

################################################
#Function definnig
fun_sum=function(x,y){
  if(is.numeric(x)==FALSE|is.numeric(y)==FALSE){
    stop('wrong inputs eshgham')
  } else {z=x+y}
  return(z)
}

fun_sum(1,3)
fun_sum('a',4)
sum(2,3)

#----------------------------apply:
mat=matrix(c(-1,4,3,7,8,NA),2,3)
mat

o1=apply(X = mat, MARGIN = 2, FUN = mean, na.rm=TRUE) #nr.rm means ignore NA
o1
o2=apply(X = mat, MARGIN = 1, FUN = plot, main='plot', xlab='x', ylab='y') 

#defining function to be used in apply
sum_mahdi=function(x){
  if(is.numeric(x)==FALSE){
    stop('wrong inputs eshgham')
  } else {z=x+2}
  return(z) #if you use returen, it will show resutls in each call of function
}
y=sum_mahdi(3)
y
o3=apply(X = mat, MARGIN = c(1,2), FUN = sum_mahdi) 
o3

#below is same as above
o4=apply(mat,2, function(x) x+2) #1:2=c(1,2)
o4
o5=apply(mat,2, function(x) c(x+2,abs(x))) #1:2=c(1,2)
o5

#lapply: works on a list
A<-c(1:9)
B<-c(1:12)
C<-c(1:15)
my.lst<-list(A,B,C)
lapply(my.lst, sum)

#sapply works just like lapply, but will simplify the output if possible. This means that instead of returning a list like lapply, it will return a vector instead if the data is simplifiable.
sapply(my.lst, sum)
sapply(my.lst, function(x) x*2)

#vapply is similar to sapply, but it requires you to specify what type of data you are expecting the arguments for vapply
vapply(my.lst, sum, numeric(1))

#Sometimes you may want to perform the apply function on some data, but have it separated by factor. In that case, you should use tapply.
tdata <- as.data.frame(cbind(c(1,1,1,1,1,2,2,2,2,2), 
                             c(1,1,1,1,1,2,2,2,2,2),
                             c(1,1,1,1,1,2,2,2,2,2)))
colnames(tdata)
tapply(tdata$V2, tdata$V1, mean)
tapply(tdata$V2, tdata$V1, function(x) c(mean(x), sd(x)))

#Some other funcions

#get
a='a'
b=paste(a,'1',sep = '')
b
a1=1:10

print(b)
print(get(b))
print(get(b)[1])

#paster
x1='x1'
x2='x2'
x1x2=paste(x1,x2,sep = ",")

x1x2_sub=substring(x1x2,3,4)

x=1:10
y=11:20
z=cbind(x,y)
z
z=rbind(x,y)
z

#which function works on boolean statements
a=c('a','b',4)
which(a==4)   #gives the INDEX (location) of searched value/character

a=matrix(1:10,10,15) #finding a value ij in a matrix
which(a=10,arr.ind = T)  #gives error because a=10 is not a boolean so:
which(a==10,arr.ind = T)

#is.na(): finding missing values: it gives boolean outputs so can be used in which or subsetting
a=c(1,2,5,7,NA,NaN,100)
is.nan(a) #only NaN (not a number)
is.na(a)  #NaNs and NA (not available/missing value)
is.nan(a)|is.na(a) #=is.na(a), they are same

#how to delet na from data set;
a=matrix(c(1,2,NA,4,5,6,7,NA,9,10,11,12,13,14,15,16),nrow = 4,ncol = 4)
a
i=which(is.na(a)==TRUE,arr.ind = TRUE)
i
#deleting all row if one NA is in the row
a_new1=a[-i[,1],]
a_new1
#deleting all col if one NA is in the col
a_new2=a[,-i[,2]]
a_new2

print(z)   #showing in consile
View(z)    #showing seperately
remove(z)
rm(z)

q(save = 'yes')        #quit from R

x=9
sqrt(x)

#building an empty vector
y=numeric(11)
y

#power is arg to arg
a=c(2,3)
b=c(4,5)
a^b

##############################################################################
#for loop in R:
for(i in 1:10) {
  print(i)
}

########################################################################################
#IF condition
#| : or condition
#& : and condition
#! : disclaimer condition
x=10
if (x==3) {
  print(x)
} else {  #else should come right after secon acolade '}' of if
  print('x is not 3')
}
  
#########################################################################################
#while
i=1 #first define it
while(i<6){
  print(i)         #looping desired function 1
  i=i+1
  }

#for
for (i in 1:5) {
  if (i == 3){next}
  print(i)
}

for (i in 1:5) {
  if (i == 3){break}
  print(i)
}

y=c()
for (i in 1:500) {
  y[i]=mean(runif(100,0,1))
  plot(hist(y))
  Sys.sleep(0.05)   #making animation of results by system sleep
}

# this command: A=c(A,i) can help in for loops to add new values to old variable

##########################################################################################
#General packages and notes

#Correlation plotting
#install.packages("corrplot")
require(corrplot)
corrplot(cor_d,method = 'pie')
corrplot(cor_d,method = 'number')

#Associations rules
install.packages("arules")
library(arules)
titanicACE <- read.csv(file="TitanicAE.csv",sep = ';')
titanic_sub=titanicACE[,1:4]
rules=apriori(titanic_sub,parameter = list(supp=0.1,conf=0.2,minlen=2))
rules=sort(rules, by="lift")
inspect(rules)

##Curve fitting
x <- c(32,64,96,118,126,144,152.5,158)
y <- c(99.5,104.8,108.5,100,86,64,35.3,15)
#fit first degree polynomial equation:
fit  <- lm(y~x)  #like loes function
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(min(x),max(x), length=(max(x)-min(x))/5)

plot(x,y,pch=19,ylim=c(0,150))

lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

summary(fit4)  #you can see the equation which is:
yyy=7.474e+01+1.426e+00*xx^1-2.854e-02*xx^2+2.878e-04*xx^3-1.134e-06*xx^4
plot(yyy,predict(fit4, data.frame(x=xx))) #predict(fit4, data.frame(x=xx)) are predicted values from fitted line 
abline(0,1)

#In summary you can also see Multiple R-squared as a index of fitted line goodness
#################################################################
#plotting a dataframe:
require(ggplot2)
require(reshape2)
df <- data.frame(repeative_col1 = 1:10,
                 repeative_col2 = 100:109,
                 a = cumsum(rnorm(10)),
                 b = cumsum(rnorm(10)),
                 c = cumsum(rnorm(10)))
df <- melt(df ,  id.vars = c('repeative_col1','repeative_col2'), variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(time,value)) + geom_line(aes(colour = series))

# or plot on different plots
ggplot(df, aes(time,value)) + geom_line() + facet_grid(series ~ .)

library(ggplot2)
data(mpg, package="ggplot2") # alternate source: "http://goo.gl/uEeRGu")
theme_set(theme_bw())  # pre-set the bw theme.
View(mpg)
g <- ggplot(mpg, aes(cty, hwy))

# Scatterplot
g + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Scatterplot with overlapping points", 
       caption="Source: midwest")


#Tidying/reshaping tables using tidyr
#https://mgimond.github.io/ES218/Week03b.html

#smoothing
library(tidyverse)
#install.packages('dslabs')
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

View(polls_2008)
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))
View(fit$y)
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#filling missing values/gap in rainfal:
#install.packages('hyfo')
library(hyfo)
help(fillGap)

data(testdl)
a <- extractPeriod(testdl, commonPeriod = TRUE)
a1 <- list2Dataframe(a)
#View(a1)
a2 <- fillGap(a1,corPeriod = 'daily')
a3 <- fillGap(a1, corPeriod = 'monthly')
plot(a1$Date,a1$AAA)
plot(a1$Date,a2$AAA)
plot(a3$Date,a2$AAA)

#plot pdf
df <- read.csv("https://github.com/nchelaru/data-prep/raw/master/telco_cleaned_renamed.csv")
View(df)
#notice that aes should be string
ggplotly(ggplot(df, aes_string(df$MonthlyCharges, fill = df$Churn)) + 
           geom_density(position='fill', alpha = 0.5) + 
           xlab("MonthlyCharges") + labs(fill='Churn') +
           theme(legend.text=element_text(size=12), 
                 axis.title=element_text(size=14)))

#ordering data frame
attach(mtcars)

# sort by mpg
newdata <- mtcars[order(mpg),]

# sort by mpg and cyl
newdata <- mtcars[order(mpg, cyl),]

#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),]

detach(mtcars)

## example of using 'incomparables'
x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
merge(x, y, by = c("k1","k2")) # NA's match
merge(x, y, by = "k1") # NA's match, so 6 rows
merge(x, y, by = "k2", incomparables = NA) # 2 rows

#taylor diagram: https://en.wikipedia.org/wiki/Taylor_diagram
install.packages('plotrix')
require(plotrix)
# fake some reference data
ref<-rnorm(30,sd=2)
# add a little noise
model1<-ref+rnorm(30)/2
# add more noise
model2<-ref+rnorm(30)
# display the diagram with the better model
oldpar<-taylor.diagram(ref,model1)
# now add the worse model
taylor.diagram(ref,model2,add=TRUE,col="blue")
# get approximate legend position
lpos<-1.5*sd(ref)
# add a legend
legend(lpos,lpos,legend=c("Better","Worse"),pch=19,col=c("red","blue"))
# now restore par values
par(oldpar)
# show the "all correlation" display
taylor.diagram(ref,model1,pos.cor=FALSE)
taylor.diagram(ref,model2,add=TRUE,col="blue")

#overlapping plots in ggplot:
qplot(1:10, 1:10) +
  annotation_custom(
    grob = g,         #here g is grob so if you have ggplot use this:
    xmin = 1,         #ggplotGrob() to solve problem
    xmax = 5,
    ymin = 5,         #if your x axis is date define x threshold by this
    ymax = 10         #to change date to number: as.numeric(date())
  ) 

#3D plot:
cone <- function(x, y){ 
  sqrt(x ^ 2 + y ^ 2) 
}
x <- y <- seq(-1, 1, length = 30) 
z <- outer(x, y, cone) 
persp(x, y, z) 
#####################
#list
x=list(1,232,'hello')
str(x)
#calling atomic element of a list:
#wrong way
x[1]
#right way
x[[1]]
unlist(x[1])
#################################contingency table and HSS calculation:
#install.packages('verification')
library(verification)

a=data.frame(c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE),
             c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE))
colnames(a)=c('obs','pre')
a

#Method 1;
contingency_table<- table(a$obs,a$pre) #col is obs and row is pre
contingency_table #left is first input to table function in above line code (here a$obs) and top is second input of table function(here a$pre)

confusionMatrix(contingency_table)[['overall']][['Accuracy']]

HSS1=table.stats(contingency_table, fudge = 0.01)[['HSS']]
HSS2=table.stats(a$obs,a$pre, fudge = 0.01)[['HSS']] #first two arqument should be binary (T/F or 1/0)

#Method 2:
Statts = verify(a$obs,a$pre, frcst.type = "cat", obs.type = "cat")
summary(Statts)
Statts[['hss']]