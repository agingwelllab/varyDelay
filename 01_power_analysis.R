# power analysis for varyDelay study
library(WebPower)
# https://rdrr.io/cran/WebPower/man/wp.rmanova.html

# repeated-measures anova with 2 groups, 4 measurements, effect size = .2 (small), interaction effect
pwr <- wp.rmanova(ng = 2, nm = 4, f = .2, power = 0.8, type = 2)
print(paste0('The sample size needed is ', round(pwr$n)))
