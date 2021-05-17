#' @title Fit Simulated Data to a Model.
#'
#' @param model a model object, from (eg) lm
#' @param xpred the predictor for the x axis on the graph
#' @param ci confidence interval for fit curve (defaults
#'   to 0.95)
#' @param npoints number of data points for fit line. Either specify
#'   a number, or "same" will return a simulation of the same size
#'   as the original dataset.
#'
#' @returns predicted data
#'
#' @import ggplot2 
#' @importFrom stats qnorm
#' @importFrom stats predict
#' 
#' @export pred.fit
#' 
#' @examples 
#' ## Anwar M, Green JA, Norris P, et al 
#' ## Prospective daily diary study reporting of any and all symptoms in healthy 
#' ## adults in Pakistan: prevalence and #' response
#' ## BMJ Open 2017;7:e014998
#' \donttest{data(symptom)
#' glm.symptom <- glm(actual_help_days ~ symp_days_reported, 
#'      family = "poisson", data = symptom)
#' pred.fit(glm.symptom)}
#'
pred.fit <- function(
  model, 
  xpred = NULL,
  ci = 0.95, 
  npoints = "same"
){
  if (attr(model, "class")[1] == "lm") {
    stop("pred.fit() is not yet implementd for lm()")
  }
  predv <- attr(model$terms, "term.labels") #extract predictors from model
  if(is.null(xpred)){    #if xpred not provided, first predictor used
    xpred <- predv[1]
  }
  z <- qnorm(1-(1-ci)/2) #z score for se.fit
  if(npoints == "same") {   #set number of points
    npoints <- length(model$y)
  }
  xp <- list()              #empty list
  for(i in predv){          #fill list with mean of each predictor
    xp[[i]] <- rep(mean(model$model[[i]], na.rm = TRUE), length = npoints)
  }
  xp <- as.data.frame(xp)   #make list to df
  xp[[xpred]] <- seq(min(model$model[[xpred]]), max(model$model[[xpred]]), 
                     length.out = npoints)  #set xpredictor values to range observed
  yp <- predict(model, newdata = xp, se= T) #create predictions
  pred <- xp                                #build data frame of predictions
  pred$fit <- yp$fit
  pred$se.fit <- yp$se.fit
  pred$est <- exp(pred$fit)
  pred$upp <- exp(pred$fit+z*pred$se.fit)
  pred$low <- exp(pred$fit-z*pred$se.fit)
  return(pred)
}
