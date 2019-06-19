age.data <- data.frame(age=c(10,15,20,25,30,35),salary=c(50,55,60,65,70,75))
age.data <- data.frame(age=c(10,15,20,25,30,35),salary=c(50,50,50,50,50,50))
age.data <- data.frame(age=c(10,15,20,25,30,35,40,45),salary=c(50,55,50,55,50,55,55,55))
age.data <- data.frame(age=c(0,1,2,3,4,5,6,7,8,9),salary=c(50,55,50,55,5,5,5,5,5,5))
attach(age.data)
sd(age.data$salary)
quantile(age.data$age,probs = .1)
cor(age.data$age,age.data$salary,method = "spearman")

library(dplyr) 
age.data <- data.frame(age=c(10,15,15,20,25,30,35),salary=c(50,70,55,60,65,70,75))
age.data %>% filter(age > 10)%>% group_by(age) %>% summarise(n = sum(n))
age.data %>%  group_by(age.data$age) 

age.data.1 <- data.frame(age4=c(10,15,20,25,30,35),salary=c(50,55,60,65,70,75))
age.data.1 %>%  group_by(age) %>% filter(any(age > 1))

