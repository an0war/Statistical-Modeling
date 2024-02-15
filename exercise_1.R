## Anowar Hussain( 151934177)
#exercise 1

##############################################################
# answer to the quesstion no 1

data<-read.table("paper.txt", sep="\t", dec=".", header=TRUE)
attach(data)
names(data)

# a)
model <- lm(strength ~ hardwood + pressure) # model M1|2
summary(model)
summary(model)$"coef"[3] # maximum likelihood estimate for the parameter β2 

# b)
summary(model)$sigma^2 # unbiased estimate for the the variance parameter σ2.

# c) 
fitted(model) # fitted value for the model
fitted(model)[1] # fitted  value for the first observation

# d)
new.data <- data.frame(hardwood = c(7), pressure = c(500))
predict(model, newdata=new.data)
mu.hat<-predict(model, newdata=new.data, interval="confidence", level=0.95)
mu.hat
mu.hat[1] # maximum likelihood estimate for the expected value

# e)
new.data <- data.frame(hardwood = c(7), pressure = c(500))
y.hat<-predict(model, newdata=new.data, interval="prediction", level=0.8)
y.hat 
y.hat[2] # lower bound of the prediction interval

# f)
model.H0<-lm(strength ~ hardwood)
summary(model.H0)

anova(model.H0, model, test="F")
anova(model.H0, model, test="F")$"F"[2] # the value of the test statistic
anova(model.H0, model, test="F")$"Pr(>F)"[2]  # p-value
# since the p value is very less than 0.05 so the null hypothesis is rejected



#####################################################
# Answer to the question no 2

data2 <- read.table("makiwaraboard.txt", sep="\t", dec=".", header=TRUE ) 
attach(data2)
names(data2)
data2
data2$WoodType = factor(WoodType,levels = c("Cherry", "Ash", "Fir", "Oak"), labels = c(1, 2, 3, 4))
data2$BoardType = factor(BoardType,levels = c("Stacked", "Tapered"), labels = c(1, 2))
attach(data2)
data2

# a)
model.main <- lm(Deflection ~ WoodType + BoardType)
summary(model.main)
new.data2 <- data.frame(WoodType = c("4"), BoardType = c("2"))
new.data2
mu.hat<-predict(model.main, newdata=new.data2)
mu.hat
mu.hat[1]

# b)


combination <- expand.grid(WoodType = c("1","2", "3","4"), BoardType = c("1", "2"))
mean <- predict(model.main, newdata = com)
mean  


#c)
model.full <- lm(Deflection ~ WoodType + BoardType + WoodType:BoardType)
summary(model.full)
summary(model.full)$"coef"[7]
coef(model.full)
coef(model.full)[7]


#d)

residuals(model.full)
residuals(model.full)[336]

#e)
anova(model.main, model.full, test="F")
anova(model.main, model.full, test="F")$"F"[2] # the value of the test statistic
anova(model.main, model.full, test="F")$"Pr(>F)"[2]  # p-value






