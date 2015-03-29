library(aod)
library(MASS) # family=negative.binomial
head(dja)

#1 
mod1 <- glm(y/n ~ group, family=quasibinomial(link="logit"), weights=n, data=dja)
summary(mod1)

#2
mod2 <- betabin(cbind(y,n-y) ~ group, ~1, link="logit", data=dja)
summary(mod2)

#3
mod3 <- glm(y ~ group, family=quasipoisson(link="log"), offset=trisk, data=dja)
summary(mod3)

#4
mod4 <- glm(y ~ group, family=negative.binomial(theta=1), offset=trisk, data=dja)
summary(mod4)

#5
# Which model do I prefer:
#  - mdoel diagnostics
#  - similarities / differences in moels
