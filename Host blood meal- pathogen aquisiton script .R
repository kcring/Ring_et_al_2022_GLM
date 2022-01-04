
setwd("~/Desktop/SFSU/Host_Legacy_GLM")
tick<-read.csv("Host_Blood_Meal_Data_Lisa+Kacie_AllData.csv", header=T)

#install.packages("lme4")
library(lme4)
#install.packages("glmm")
library(glmm)
library(ggplot2)

tick$Infection.Status<-as.factor(tick$Infection.Status)
tick$Larval.Blood.Meal<-as.factor(tick$Larval.Blood.Meal)

#glm model 
model<-glmer(Infection.Status~Larval.Blood.Meal+(1|Trial), data=tick, family=binomial(link="logit"))

summary(model)

#create a contigency table 
ctable<- table(tick$Larval.Blood.Meal, tick$Infection.Status)
ctable
#chi squared teest 
test <- chisq.test(table(tick$Larval.Blood.Meal, tick$Infection.Status))
test

#visualization of infected vs non infected ticks (lizard vs mouse host)
mosaicplot(ctable, main = "Infection status of Bb engorged nymphs",
           xlab= "Larval blood meal",
           ylab = "Infection status",
           las = 1,
           color = c("azure4", "darkcyan"))
#simple barplot comparison 
ggplot (tick, 
        aes(x = Larval.Blood.Meal, fill = Infection.Status)) + 
  geom_bar(position = "dodge", color = "black") + 
  labs(title = "Infection status of Bb engorged nymphs", x ="Larval host   blooodmeal", y = "Tick count", fill = "Infection status") + 
  theme_bw() + scale_fill_manual(values = c("azure4", "darkcyan"))  

##############predict script########

#make a prediction data frame by larval host 

pframe <- data.frame(Larval.Blood.Meal=unique(tick$Larval.Blood.Meal))

#predict on the logit predictor scale with standard errors




linkinv<-family(model)$linkinv ##inverse-link function 

pframe$pred0 <- pp$fit
pframe$pred <- linkinv(pp$fit)
alpha <- 0.95
sc <- abs(qnorm((1-alpha)/2))  ## Normal approx. to likelihood
alpha2 <- 0.5
sc2 <- abs(qnorm((1-alpha2)/2))  ## Normal approx. to likelihood
pframe <- transform(pframe,
                    lwr=linkinv(pred0-sc*pp$se.fit),
                    upr=linkinv(pred0+sc*pp$se.fit),
                    lwr2=linkinv(pred0-sc2*pp$se.fit),
                    upr2=linkinv(pred0+sc2*pp$se.fit))

with(pframe,
     {
       plot(Larval.Blood.Meal,pred, ylim=c(0,1))
       arrows(as.numeric(Larval.Blood.Meal),lwr,as.numeric(Larval.Blood.Meal),upr,
              angle=90,code=3,length=0.1)
     })

boxplot
with(pframe,
     {
       bxp(list(stats=rbind(lwr,lwr2,pred,upr2,upr),
                n = rep(1,nrow(pframe)),
                conf = NA,
                out = NULL,
                group = NULL,
                names=as.character(Larval.Blood.Meal)))
     })


##ggplot with glm 
library("ggplot2")
ggplot(pframe,aes(Larval.Blood.Meal,pred), x ="Probabilty of infection") +
  labs(title = "Nymphal infection probability", x ="Larval host blooodmeal", y = "Probabilty of infection") +
  geom_pointrange(aes(ymin=lwr,ymax=upr))+
  geom_linerange(aes(ymin=lwr2,ymax=upr2),lwd=1.5)  + 
  ylab("Proportion Infected")+
  ggtitle("Comparison of larval blood meal")
  theme_bw() 









