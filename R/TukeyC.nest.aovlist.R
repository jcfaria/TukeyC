##
## S3 method to 'aovlist' object
##
TukeyC.nest.aovlist <- function(x,
                                which,
                                fl1,
                                fl2,
                                MSE,
                                dfr, 
                                sig.level,
                                round,
                                adjusted.pvalue,...)
{

  my <- as.character(attr(x,'terms')[[2]])
  m1 <- gsub('\\:',
             '\\+', 
             which)
  #   m2 <- unlist(strsplit(which,
  #                         '[[:punct:]]'))  
  # 
  forminter <- as.formula(paste(my, '~', m1))

  dat <- model.frame(x)

  aux_mt1 <- aggregate(forminter, 
                       data = dat,
                       function(x) c(means = mean(x),
                                     r = length(x)))

  aux_mt2 <- aux_mt1[order(aux_mt1[[my]][,1], 
                           decreasing = TRUE),]

  aux_mt3 <- data.frame(aux_mt2[1:length(names(aux_mt2))-1],
                        means = aux_mt2[[my]][,1],
                        reps = aux_mt2[[my]][,2])

  #row.names(mt) <- aux_mt1[,1]

  #names(aux_mt2) <- gsub(my,'x',names(aux_mt2))
  # 
  #   aux_r <- aggregate(forminter, 
  #                      data = x$model,
  #                      function(x) r = length(x))
  #   reps <- aux_r[[my]]
  # 
  #   aux_mt <- LSmeans(x,
  #                     effect = m2)
  # 
  #   aux_mt1 <- aux_mt$coef[,1]
  # 
  #   aux_mt2 <- data.frame(aux_r[1:length(names(aux_r))-1],
  #                         means = aux_mt1,
  #                         reps = reps)
  # 
  #   aux_mt3 <- aux_mt2[order(aux_mt2[['means']],
  #                            decreasing = TRUE),]
  # 
  nf1 <- unlist(strsplit(which,
                         split = ':'))[1] # nome do primeiro fator do which

  nf2 <- unlist(strsplit(which,
                         split = ':'))[2] # nome do segundo fator do which

  nf3 <- unlist(strsplit(which,
                         split = ':'))[3] # nome do terceiro fator do which

  if(is.null(fl2)){
    # Interesse apenas na interação dupla
    f1 <- levels(model.frame(x)[,nf2]) # correspondem aos fatores que se quer comparar!

    f2 <- levels(model.frame(x)[,nf1])[fl1] # corresponde ao fator onde se está fazendo o desdobramento!

    mt <- subset(aux_mt3, 
                 eval(parse(text = nf1)) == f2) # pegando as médias de interesse

    row.names(mt) <- paste(f2,
                           f1,
                           sep='/')  
  } # Interesse na interação tripla 
  else {

    f1 <- levels(model.frame(x)[,nf3])

    f2 <- levels(model.frame(x)[,nf2])[fl2] 

    f3 <- levels(model.frame(x)[,nf1])[fl1]

    mt <- subset(aux_mt3, 
                 eval(parse(text = nf1)) == f3 & eval(parse(text=nf2)) == f2) # pegando as médias de interesse

    row.names(mt) <- paste(f3,
                           f2,
                           f1,
                           sep='/')   

  } 

  out <- make.TukeyC.test(obj             = mt,
                          MSE             = MSE,
                          sig.level       = sig.level,
                          dfr             = dfr,
                          round           = round,
                          adjusted.pvalue = adjusted.pvalue)  

  m.inf <- m.infos.nest.aovlist(x         = x,
                                my        = my,
                                forminter = forminter,
                                which     = which,
                                fl1       = fl1,
                                fl2       = fl2,
                                sig.level = sig.level,
                                aux_mt    = aux_mt1,
                                MSE       = MSE)

  res <- list(out  = out,
              info = m.inf)

}
