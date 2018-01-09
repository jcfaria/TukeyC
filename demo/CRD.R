##
## Examples: Completely Randomized Design (CRD)
##

## The parameters can be: formula, aov, lm or lmer.

## Example 1: an small experiment
library(TukeyC)
data(CRD1)

## From: formula - balanced
tk1 <- with(CRD1,
            TukeyC(y ~ x,
                   dfm))
tk1
summary(tk1)
plot(tk1)

## From: formula - unbalanced
utk1 <- with(CRD1,
             TukeyC(y ~ x,
                    dfm[-1,]))
utk1
summary(utk1)

## From: aov - balanced
av1 <- with(CRD1,
            aov(y ~ x,
                data=dfm))
summary(av1)

tk2 <- TukeyC(av1)
tk2
summary(tk2)

## From: lm - unbalanced
ulm1 <- with(CRD1,
             lm(y ~ x,
                data=dfm[-1,]))
summary(ulm1)

utk2 <- TukeyC(ulm1)
utk2
summary(utk2)

## Example 2: a lot of groups
data(CRD2)

## From: data.frame (dfm) - balanced
tk3 <- with(CRD2,
            TukeyC(y ~ x,
                   dfm))
plot(tk3,
     id.las=2,
     rl=FALSE)

## From: data.frame (dfm) - unbalanced
utk3 <- with(CRD2,
             TukeyC(y ~ x,
                    dfm[-1,]))
plot(utk3,
     id.las=2,
     rl=FALSE)

## From: aov - balanced
av2 <- with(CRD2,
            aov(y ~ x ,
                data=dfm))
summary(av2)

tk4 <- TukeyC(av2)
plot(tk4,
     id.las=2,
     rl=FALSE)

## From: lm - unbalanced
ulm2 <- with(CRD2,
             lm(y ~ x,
                data=dfm[-1,]))
summary(ulm2)

utk8 <- TukeyC(ulm2)

plot(utk8,
     id.las=2,
     rl=FALSE)
