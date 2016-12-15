##
## Example: Randomized Complete Block Design (RCBD)
##

## The parameters can be: vectors, design matrix and the response variable,
## data.frame, aov or lm.

library(TukeyC)
data(RCBD)

## From: data.frame (dfm), which='tra'
tk1 <- with(RCBD,
            TukeyC(y ~ blk + tra,
                   dfm,
                   which='tra'))
summary(tk1)
plot(tk1)

## From: formula, which='blk' implicit (due to be the first arg of the model)
tk2 <- with(RCBD,
            TukeyC(y ~ blk + tra,
                   dfm))
summary(tk2)
plot(tk2)


av1 <- with(RCBD,
            aov(y ~ blk + tra,
                data=dfm))
summary(av1)

## From: aov, which='blk' implicit (due to be the first arg of the model)
tk3 <- TukeyC(av1)
summary(tk3)

## From: aov, which='blk' explicit
tk4 <- TukeyC(x=av1,
              which='blk')
summary(tk4)

## From: aov, which='tra' explicit
tk5 <- TukeyC(x=av1,
              which='tra')
summary(tk5)
