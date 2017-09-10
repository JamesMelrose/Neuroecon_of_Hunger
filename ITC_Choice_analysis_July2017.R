library(lme4)
library(ggplot2)
library(sjPlot)

ITC <- read.csv("/Users/james/Documents/NeuroEcon_of_Hunger/Analysis/Data/Combined_Scanner_ITC.csv")

#remove data where they didn't respond
ITC <- subset(ITC, RT < 6)
#remove condition 2 catch trials
ITC <- subset(ITC, Condition == 1)
ITC <- subset(ITC, Subject != 101)

ITC["logK"] <- log(ITC$startingK)

#Figure out the full model needed
#need fixed and random slope of logK
m1 <- glmer(Choice ~ 1 + (1|Subject),ITC,family=binomial,nAGQ=1)
m2 <- glmer(Choice ~ logK + (1|Subject),ITC,family=binomial,nAGQ=1)
m3 <- glmer(Choice ~ logK + (1 + logK|Subject),ITC,family=binomial,nAGQ=1)
an1 <- anova(m1,m2,m3)
an1

#no fixed effect of Run but random slope
m4 <- glmer(Choice ~ logK + Run + (1 + logK|Subject),ITC,family=binomial,nAGQ=1)
m5 <- glmer(Choice ~ logK + (1 + logK + Run|Subject), ITC, family=binomial,nAGQ=1)
an2 <- anova(m3,m5)
an2

#SSamount has an effect on both fixed and random slope levels
m6 <- glmer(Choice ~ logK + scale(SSamount) + (1 + logK + Run|Subject), ITC, family=binomial,nAGQ=1)
m7 <- glmer(Choice ~ logK + scale(SSamount) + (1 + logK + Run + scale(SSamount)|Subject), ITC, family=binomial,nAGQ=1)
an3 <- anova(m5,m6,m7)
an3

#don't need to include trial as a fixed effect but include random slope
m8 <- glmer(Choice ~ logK + scale(SSamount) + scale(Trial) + (1 + logK + Run + scale(SSamount)|Subject), ITC, family=binomial,nAGQ=1)
an4 <- anova(m7,m8)
an4
m9 <- glmer(Choice ~ logK + scale(SSamount) + (1 + logK + Run + scale(SSamount) + scale(Trial)|Subject), ITC, family=binomial,nAGQ=1)
an5 <- anova(m7,m9)
an5

#does Day need to be a random slope, yes
m10 <- glmer(Choice ~ logK + scale(SSamount) + (1 + logK + Run + scale(SSamount) + scale(Trial) + factor(Day)|Subject), ITC, family=binomial,nAGQ=1)
an6 <- anova(m9,m10)
an6

ITC$Day <- as.factor(ITC$Day)
#USE m10 AS THE BASE MODEL!!!!!!
#test for fixed effect of Day, and interaction between Day:logk
Daym1 <- glmer(Choice ~ logK + scale(SSamount) + Day + (1 + logK + Run + scale(SSamount) + scale(Trial) + Day|Subject), ITC, family=binomial,nAGQ=1)
DayAN1 <- anova(m10,Daym1)
DayAN1

#no effect of day, but what we are really interested in is whether the logistic curve
#shifts based on a Day:logK interaction, the adaptive algorithm keeps choice near 50%
#so it is more a question of where on the logK axis the logistic curve gets dragged
#need to use Daym1 so main effect of day is removed so interaction is interpretable
Daym2 <- glmer(Choice ~ logK + scale(SSamount) + Day + Day:logK + (1 + logK + Run + scale(SSamount) + scale(Trial) + Day|Subject), ITC, family=binomial,nAGQ=1)
DayAN2 <- anova(Daym1,Daym2)
DayAN2
summary(Daym2)
ranef(Daym2)

#predicted probabilities don't look to be happening, so lets just plot coefficients
sjp.glmer(Daym2,type="fe")

#plot a correlation matrix of the fixed effects
sjp.glmer(Daym2, type = "fe.cor")
sjp.glmer(Daym2, type = "re.qq")





#don't need this crap
sjp.glmer(Daym2,
          type = "ri.pc",
          show.se = TRUE)

sjp.glmer(Daym2,
          type = "ri.pc",
          facet.grid = FALSE)


plot(effect("logK:Day", Daym2),grid=TRUE)

#get the log odds for the estimates
se <- sqrt(diag(vcov(Daym2)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(Daym2), LL = fixef(Daym2) - 1.96 * se, UL = fixef(Daym2) + 1.96 *se))
#the output of this will give you the log odds for the different conditions
exp(tab)
cnd <- "Condition"
tab2 <- data.frame(tab[c(2,5),])
tab2[,cnd] <- c("log of K:Fast","log of K:Fed")
ggplot(tab2, aes(x=factor(Condition), y=Est))+geom_errorbar(aes(ymin=LL, ymax=UL), width=.1)+geom_point()+geom_abline(intercept=0,slope=0,col="red")+labs(title="Feeding Increases SS Choice When log of K is Increased")+ylab("Beta Value")+xlab("")

lattice::dotplot(ranef(Daym2, condVar=TRUE))



lsmip(Choice ~ logK + scale(SSamount) + factor(Day) + factor(Day):logK | 1 + logK + Run + scale(SSamount) + scale(Trial) + factor(Day),type="response")

sjp.glmer(Daym2, y.offset = .4)

u0 <- ranef(Daym2, condVar = TRUE)
u0se <- sqrt(attr(u0[[1]], "condVar")[1, , ])
> commid <- as.numeric(rownames(u0[[1]]))
> u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se)
> colnames(u0tab)[2] <- "u0"
> u0tab <- u0tab[order(u0tab$u0), ]
> u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
> u0tab <- u0tab[order(u0tab$commid), ]
> colnames(u0tab)[4] <- "u0rank" 








warlpiri.lmer = glmer(CaseMarking ~ WordOrder + AgeGroup +
                        AnimacyOfSubject + (1|Text) + (1|Speaker),
                      control=glmerControl(optimizer="optimx",optCtrl=list(method="nlminb")),
                      family = "binomial", data = warlpiri)

pframe0 <- with(ITC, expand.grid(Day=levels(ITC$Day)))

pframe0


#plot out the effects
tmpdat <- ITC[, c("SSamount","Day","logK","Subject","Run","Trial")]
jvalues <- with(ITC, seq(from = min(logK), to = max(logK), length.out = 99))
#jvalues <- c(jvalues,jvalues)
#conds <- c(rep("Fed",99),rep("Fast",99))
#tmpdat$Day = conds
pp <- lapply(jvalues, function(j) {
  tmpdat$logK <- j
  #tmpdat$Day <- "Fed"
  predict(Daym2, newdata = tmpdat, type = "response")
})
sapply(pp[c(1:99)], mean)
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, jvalues))
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "cond")
head(plotdat)
cnd <- "condition"
x <- c("neutral","happy","fear")
plotdat[,cnd] <- x
ggplot(plotdat, aes(x=logK, y=PredictedProbability))+geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1)+geom_point()+geom_abline(intercept=0,slope=0,col="red")+labs(title="Predicted Probability of choosing the LL option following face primes")+ylab("Predicted Probability")+xlab("")



library(effects)
>> plot(effect("recipe:temp", fm1), grid=TRUE)
>>

