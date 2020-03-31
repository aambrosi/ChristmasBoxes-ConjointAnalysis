########################################################################
### Functions for predicting preference shares from a MNL model with ### 
### bootstrap percentiles prediction intervals                       ### 
########################################################################

# model: mlogit object returned by mlogit()
# data: a data frame containing the set of designs for which you want to 
#       predict shares.  Same format at the data used to estimate model. 
# nsim: number of bootsrap samples, default is 500
# conflevel: desired confidence level, default is 0.95

BootCI.predict.mnl <- function(model, data, nsim = 500, conflevel = 0.95) {
  dataModel               <- model$model
  dataModel$probabilities <- NULL
  dataModel$linpred       <- NULL
  dataModel               <- cbind(dataModel, attr(model$model, "index"))
  dataModel$idRespQues    <- paste(dataModel$id, "-", dataModel$chid)
  idVar                   <- unique(dataModel$idRespQues)
  bootDistr               <- NULL 
  
  for(i in 1:nsim) {
    cat("Doing boostrapping ", i, "/", nsim, "\n")
    idbootsamp      <- data.frame(idRespQues = sample(idVar, replace = T))
    bootsamp        <- merge(idbootsamp, dataModel,   by      = "idRespQues", all.x = T)
    bootsamp.mlogit <- mlogit.data(data   = bootsamp, choice  = "choice"    , shape = "long", 
                                   id.var = "id"    , alt.var = "alt")
    bootfit         <- update(model, data = bootsamp.mlogit)
    bootDistr       <- cbind(bootDistr, predict.mnl(bootfit, data)$share)
  }
  
  lowl <- (1 - conflevel) / 2
  upl  <- 1 - lowl  
  bootperc                  <- t(apply(bootDistr, 1, function(x) quantile(x, probs = c(lowl, upl))))
  pointpred                 <- predict.mnl(model, data)
  predictedShares           <- cbind(pointpred[ , 1], bootperc, pointpred[ , 2:ncol(pointpred)])
  names(predictedShares)[1] <- "share" 
  predictedShares
}

BootCI.predict.mixed.mnl  <- function(model, data, nsim = 500, conflevel = 0.95, nresp = 1000) {
  dataModel               <- model$model
  dataModel$probabilities <- NULL
  dataModel$linpred       <- NULL
  dataModel               <- cbind( dataModel, attr(model$model, "index"))
  dataModel$idRespQues    <- paste( dataModel$id, "-", dataModel$chid)
  idVar                   <- unique(dataModel$idRespQues)
  bootDistr               <- NULL 
  
  for(i in 1:nsim) {
    cat("Doing boostrapping ", i, "/", nsim, "\n")
    idbootsamp      <- data.frame(idRespQues=sample(idVar, replace = T))
    bootsamp        <- merge(idbootsamp, dataModel  , by      = "idRespQues", all.x = T)
    bootsamp.mlogit <- mlogit.data(data   = bootsamp, choice  = "choice"    , shape = "long", 
                                   id.var = "id"    , alt.var = "alt")
    bootfit         <- update(model, data = bootsamp.mlogit)
    bootDistr       <- cbind(bootDistr, predict.mixed.mnl(bootfit, data)$"colMeans(shares)")
  }
  
  lowl <- (1 - conflevel) / 2
  upl  <- 1 - lowl  
  bootperc                  <- t(apply(bootDistr, 1, function(x) quantile(x, probs = c(lowl, upl))))
  pointpred                 <- predict.mixed.mnl(model, data)
  predictedShares           <- cbind(pointpred[ , 1], bootperc, pointpred[ , 2:ncol(pointpred)])
  names(predictedShares)[1] <- "share" 
  predictedShares
}