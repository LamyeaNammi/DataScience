g<-"My First list"
h<-c(25,24,56,30)
j<-matrix(1:10,nrow = 5)
k<-c("one","two","three")
mylist<-list(title=g,ages=h,j,k)
mylist
mylist[[2]] #just this index element print
Item<-list("mango","apple")
append(Item,"Orange",after=1)
Item

var1=readline(prompt = "Enter any value:")
var2=readline(prompt = "Enter any number:")
var2=as.integer(var2)
print(var1)
print(var2)



var3=readline(prompt = "Enter any value:")
var4=readline(prompt = "Enter any number:")
var4=as.integer(var4)
print(var3)
print(var4)

x=scan()#terminate enter 2 times ,print(x) for print the values
print(x)

mydata<-data.frame(age=numeric(0),
gender=character(0),weight=numeric(0))#create data editor
mydata<-edit(mydata)
mydata[[3]]

#important
write.csv(mydata,"C:/Data/newfile.csv",row.names = FALSE)
#dataset export excel/csv file e write 


install.packages("dplyr")
library(dplyr)
stats<-data.frame(player=c('A','B','C','D'),
                  runs=c(100,200,300,40),
                  wickets=c(17,20,NA,5))
filter(stats,runs>100)

mydata<-read.csv("C:/Users/ASUS/Downloads/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/iris.csv",header = TRUE,sep = ",")
mydata[5:10]
mydata$variety





#lab 4

library(dplyr)
stats<-data.frame(player=c('A','B','C','D','A','A'),
                  runs=c(100,200,408,19,56,100),
                  wickets=c(17,20,NA,5,2,17)) #NA missing value
distinct(stats) #remove duplicate rows
distinct(stats,player,.keep_all=TRUE) #Remove duplicate based on a column

library(dplyr)
stats<-data.frame(player=c('A','B','C','D','A','A'),
                  runs=c(100,200,408,19,56,100),
                  wickets=c(17,20,NA,5,2,17)) 
arrange(stats,desc(runs))#assending order

#decending orderdesc

stats<-data.frame(player=c('A','B','C','D','A','A'),
                  runs=c(100,200,408,19,56,100),
                  wickets=c(17,20,NA,5,2,17)) 
rename(stats,runs_scored=runs)#rename column name
select(stats,players,wickets)

stats<-data.frame(player=c('A','B','C','D','A','A'),
                  runs=c(100,200,408,19,56,100),
                  wickets=c(17,20,NA,5,2,17)) 
select(stats,player,wickets)


stats<-data.frame(player=c('A','B','C','D','A','A'),
                  runs=c(100,200,408,19,56,100),
                  wickets=c(17,20,NA,5,2,17)) 
mutate(stats,avg=runs/4)#avg create, with other column 
transmute(stats,avg=runs/4)# drop all and created new column,dlt old variables

stats<-data.frame(player=c('A','B','C','D','A','A'),
                  runs=c(100,200,408,19,56,100),
                  wickets=c(17,20,NA,5,2,17)) 
summarize(stats,sum(runs),mean(runs))# many function under one function as parameter


#descriptve analysis
mydata<-read.csv("C:/Users/ASUS/Downloads/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/iris.csv",header = TRUE,sep = ",")
head(mydata)
summary(mydata) # 1st q e 25% and 75% 3rd q

#annotaed instant label,factor funtion cata-numo

mydata$variety<-factor(mydata$variety,
                        levels=c("setosa","versicolor","virginica"),labels=c(1,2,3))

mydata


mydata<-read.csv("C:/Users/ASUS/Downloads/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/iris.csv",header = TRUE,sep = ",")



mydata$variety<-factor(mydata$variety,levels=c("Setosa","Versicolor","Virginica"),labels=c(1,2,3))

mydata

#normalization without species
mydata<-read.csv("C:/Users/ASUS/Downloads/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/8836201-6f9306ad21398ea43cba4f7d537619d0e07d5ae3/iris.csv",header = TRUE,sep = ",")
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
iris_normalized <- iris
iris_normalized[1:4] <- as.data.frame(lapply(iris[1:4], normalize))
head(iris_normalized)


#without target attribute

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
iris_normalized <- iris
iris_normalized <- as.data.frame(lapply(iris[1:4], normalize))
head(iris_normalized)

#dataset structure
str(mydata)

# attribute standard deviation
s<-mydata$sepal.length
sd(s)

s<-mydata$sepal.width
sd(s)

s<-mydata$Petal.Length
sd(s)

s<-mydata$Petal.Width
sd(s)

#multipale attribute deviation at a time
mydata%>%summarise_if(is.numeric,sd)# is define numeric, sd high spread high, not close with each other

