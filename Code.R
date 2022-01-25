data<-read.csv("Placement_Data_Full_Class.csv")
str(data)
summary(data)
View(data)
#renaming column names
colnames(data)[1]="ID"
colnames(data)[3]="ssc_percentage"
colnames(data)[4]="ssc_board"
colnames(data)[5]="hsc_percentage"
colnames(data)[6]="hsc_boaard"
colnames(data)[7]="hsc_subject"
colnames(data)[8]="degree_percentage"
colnames(data)[9]="degree_type"
colnames(data)[11]="etest_percentage"
colnames(data)[13]="mba_percentage"
#removing null values
data[is.na(data)] = 0
data_clean<-data
View(data_clean)
#Ranking
library(ggplot2)
salary_sorted<-sort(data$salary)
ggplot(data_clean, aes(x=gender, 
                       y=salary_sorted)) +   
  geom_bar(stat="identity", color='red',fill='red')
ggplot(data_clean, aes(x=gender, 
                       y=ssc_percentage)) +   
  geom_bar(stat="identity", color='red',fill='red')
ggplot(data_clean, aes(x=gender, 
                       y=hsc_percentage)) +   
  geom_bar(stat="identity", color='red',fill='red')
ggplot(data_clean, aes(x=gender, 
                       y=degree_percentage)) +   
  geom_bar(stat="identity", color='red',fill='red')
ggplot(data_clean, aes(x=gender, 
                       y=mba_percentage)) +   
  geom_bar(stat="identity", color='red',fill='red')
#Boxplot
fact<-factor(data_clean$status)
ggplot(data_clean, aes(x=fact, y=mba_percentage)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(data_clean, aes(x=fact, y=degree_percentage)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(data_clean, aes(x=fact, y=hsc_percentage)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(data_clean, aes(x=fact, y=ssc_percentage)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
#scatterplot
ggplot(data_clean, aes(x=ssc_percentage, y=salary)) + 
  geom_point()+
  geom_smooth(method =lm)
ggplot(data_clean, aes(x=hsc_percentage, y=salary)) + 
  geom_point()+
  geom_smooth(method =lm)
ggplot(data_clean, aes(x=mba_percentage, y=salary)) + 
  geom_point()+
  geom_smooth(method =lm)
ggplot(data_clean, aes(x=degree_percentage, y=salary)) + 
  geom_point()+
  geom_smooth(method =lm)
ggplot(data_clean, aes(x=etest_percentage, y=salary)) + 
  geom_point()+
  geom_smooth(method =lm)
#4th observation
ggplot(data_clean, aes(x=specialisation, 
                       y=count,color=status)) +   
  geom_bar(stat="identity", color='blue',fill='blue')
ggplot(data_clean, aes(x=workex, 
                       y=count,color=status)) +   
  geom_bar(stat="identity", color='blue',fill='blue')
#Corelation
library(ggcorrplot)
numeric<-data_clean[,c(3,5,8,11,13,15)]#taking only nemeric value
corr <- round(cor(numeric), 1)
print(corr)
p.mat <- cor_pmat(numeric)
ggcorrplot(corr,hc.order=TRUE,type="full",
           lab=TRUE,lab_size=3,method="square",
           colors=c("red","blue","green","grey","orange","pink"),
           title="Correlation of Placement Data",ggtheme = ggplot2::theme_gray)

#Histogram
ggplot(data_clean, aes(x=mba_percentage))+
geom_histogram(color="darkblue", fill="lightblue",bins=30)
ggplot(data_clean, aes(x=hsc_percentage))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
ggplot(data_clean, aes(x=ssc_percentage))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
ggplot(data_clean, aes(x=degree_percentage))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)

#Pie chart
library(lessR)
PieChart(degree_type,data = data_clean,
rows = (gender == "F" & salary > 45000),
           main = NULL)
PieChart(degree_type,data = data_clean,
         rows = (gender == "M" & salary > 45000),
         main = NULL)
PieChart(degree_type,data = data_clean,
         rows = (workex == "Yes" & salary > 45000),
         main = NULL)
PieChart(degree_type,data = data_clean,
         rows = (workex == "No" & salary > 45000),
         main = NULL)  
PieChart(specialisation,data = data_clean,
         rows = (gender == "F" & salary > 45000),
         main = NULL)
PieChart(specialisation,data = data_clean,
         rows = (gender == "M" & salary > 45000),
         main = NULL)
PieChart(specialisation,data = data_clean,
         rows = (workex == "Yes" & salary > 45000),
         main = NULL)
PieChart(specialisation,data = data_clean,
         rows = (workex == "No" & salary > 45000),
         main = NULL)


