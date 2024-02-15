data<-read.table("ozone.txt", header=TRUE, sep="\t", dec=".")
attach(data)
data

#a)
model_id <- glm(ozone ~ rad + temp + wind, family = gaussian(link = "identity"), data = data)
model_log <- glm(ozone ~ rad + temp + wind, family = gaussian(link = "log"), data = data)
model_in <- glm(ozone ~ rad + temp + wind, family = gaussian(link = "inverse"), data = data)
model_ex <- glm(ozone ~ log(rad) + log(temp) + log(wind), family = gaussian(link = "log"), data = data)

AIC(model_id)
AIC(model_log)# good fit
AIC(model_in)
AIC(model_ex)

#b)
model_log.nm <- glm(ozone ~ rad + temp + wind, family = gaussian(link = "log"), data = data)
model_log.g <- glm(ozone ~ rad + temp + wind, family = Gamma(link = "log"), data = data)#most suitable
model_log.ig <- glm(ozone ~ rad + temp + wind, family = inverse.gaussian(link = "log"), data = data)

phi.n<-summary(model_log.nm)$dispersion
phi.g<-summary(model_log.g)$dispersion
phi.ig<-summary(model_log.ig)$dispersion

o2<-(1/phi.n)*residuals(model_log.nm, type="pearson")^2
mu<-fitted(model_log.nm, type="response")
model.pearsonN<-lm(o2~mu)
summary(model.pearsonN)

o2<-(1/phi.g)*residuals(model_log.g, type="pearson")^2#slightly good fit
mu<-fitted(model_log.g, type="response")
model.pearsonG<-lm(o2~mu)
summary(model.pearsonG)

o2<-(1/phi.ig)*residuals(model_log.ig, type="pearson")^2
mu<-fitted(model_log.ig, type="response")
model.pearsonIG<-lm(o2~mu)
summary(model.pearsonIG)


#c)
model_log.g <- glm(ozone ~ rad + temp + wind, family = Gamma(link = "log"), data = data)
gammaH0<-glm(ozone ~ rad + temp, family = Gamma(link = "log"), data = data)
anova(gammaH0, model_log.g, test="F")#45.732


#d)
betahat<-coef(model_log.g)
betahat

K<-c(0, 0, 1, 1)
q<-2
Wald<-(t(t(K)%*%betahat)%*%solve(t(K)%*%vcov(model_log.g)%*%K)%*%t(K)%*%betahat)/q
Wald#0.7546224




#question 2
data2 <- read.table("denim.txt", header=TRUE, sep="\t", dec=".")
attach(data2)
data2 

#a)
nm.id.1 <- glm(Abrasion ~ Laundry + factor(Denim), family = gaussian(link = "identity"), data = data2)
nm.id.12 <- glm(Abrasion ~ Laundry + factor(Denim) + Laundry*factor(Denim), family = gaussian(link = "identity"), data = data2)
nm.log.1 <- glm(Abrasion ~ Laundry + factor(Denim), family = gaussian(link = "log"), data = data2)
nm.log.12 <- glm(Abrasion ~ Laundry + factor(Denim) + Laundry*factor(Denim), family = gaussian(link = "log"), data = data2)
nm.in.1 <- glm(Abrasion ~ Laundry + factor(Denim), family = gaussian(link = "inverse"), data = data2)
nm.in.12 <- glm(Abrasion ~ Laundry + factor(Denim) + Laundry*factor(Denim), family = gaussian(link = "inverse"), data = data2)

AIC(nm.id.1)
AIC(nm.id.12)
AIC(nm.log.1)
AIC(nm.log.12)
AIC(nm.in.1)#good fit
AIC(nm.in.12)


#b)

nm.in.1 <- glm(Abrasion ~ Laundry + factor(Denim), family = gaussian(link = "inverse"), data = data2)#most suitable
phi.n<-summary(nm.in.1)$dispersion
o2<-(1/phi.n)*residuals(nm.in.1, type="pearson")^2
mu<-fitted(nm.in.1, type="response")
model.pearsonN<-lm(o2~mu)
summary(model.pearsonN)

g.in <- glm(Abrasion ~ Laundry + factor(Denim), family = Gamma(link = "inverse"), data = data2)
phi.g<-summary(g.in)$dispersion
o2<-(1/phi.g)*residuals(g.in, type="pearson")^2
mu<-fitted(g.in, type="response")
model.pearsonG<-lm(o2~mu)
summary(model.pearsonG)

ig.in <- glm(Abrasion ~ Laundry + factor(Denim), family = inverse.gaussian(link = "inverse"), data = data2)
phi.ig<-summary(ig.in)$dispersion
o2<-(1/phi.ig)*residuals(ig.in, type="pearson")^2
mu<-fitted(ig.in, type="response")
model.pearsonIG<-lm(o2~mu)
summary(model.pearsonIG)


AIC(nm.in.1)#good fit
AIC(g.in)
AIC(ig.in)


#c)
nm.in.1 <- glm(Abrasion ~ Laundry + factor(Denim), family = gaussian(link = "inverse"), data = data2)
newdata<-data.frame(Laundry=c(0,25), Denim=c("1", "3"))
newdata

pred<-predict(nm.in.1, newdata=newdata, type="response")
pred
summary(nm.in.1)
x1f<-cbind(c(1,0,0,0)) 
x2f<-cbind(c(1,25,0,1)) 
Xf<-t(cbind(x1f,x2f))

k<-cbind(c(-1,1))

phi<-summary(nm.in.1)$dispersion
Var.Y1f<-phi*(pred[1]^2)
Var.Y2f<-phi*(pred[2]^2)

D.f<-diag(pred)  

Var.ef<-Var.Y1f+Var.Y2f+t(k)%*%D.f%*%Xf%*%vcov(nm.in.1)%*%t(Xf)%*%D.f%*%k

lower.diff<-(pred[2]-pred[1])-qnorm(0.9)*sqrt(Var.ef)
upper.diff<-(pred[2]-pred[1])+qnorm(0.9)*sqrt(Var.ef)
lower.diff
upper.diff

Q<-(pred[2]-pred[1])/sqrt(Var.ef)
d<-2*pnorm(abs(Q), lower.tail = FALSE)
d#0.5250198

#d)
nm.in.1 <- glm(Abrasion ~ Laundry + factor(Denim), family = gaussian(link = "inverse"), data = data2)
normalH0<-glm(Abrasion ~ factor(Denim), family = gaussian(link = "inverse"), data = data2)
anova(normalH0, nm.in.1, test="F")#23.354 
