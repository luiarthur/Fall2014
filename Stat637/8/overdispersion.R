library(aod)
head(dja)

glm(y/n ~ group, family=quasibinomial, data=dja)
