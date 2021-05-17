#' @title Plot simulated data from a GLM model
#'
#' @param model a model object, from (eg) lm glm (Poisson, Negative binomial)
#' @param xpred the predictor to be plotted on the x axis
#' @param seed random seed so that simulation results are replicable
#' @param fit.line if TRUE (default) adds fit line with SE
#' @param ci passes confidence interval width for fit curve (defaults
#'   to 0.95)
#' @param npoints number of data points to for fit line. Either specify
#'   a number, or "same" will return a simulation of the same size
#'   as the original dataset.
#' @param orig_jitter amount of jitter to apply to original dataset (default 0.10)
#' @param sim_jitter amount of jitter to apply to simulated data (default 0.10)
#'
#' @returns ggplot object with simulated data plotted with original
#'
#' @import ggplot2 
#' @importFrom stats qnorm
#' @importFrom stats predict
#' @importFrom stats simulate
#' 
#' @export sim.plot
#' 
#' @examples 
#' ## Anwar M, Green JA, Norris P, et al 
#' ## Prospective daily diary study reporting of any and all symptoms in healthy 
#' ## adults in Pakistan: prevalence and #' response
#' ## BMJ Open 2017;7:e014998
#' data(symptom)
#' glm.symptom <- glm(actual_help_days ~ symp_days_reported, 
#'      family = "poisson", data = symptom)
#' sim.plot(glm.symptom)
#' 

sim.plot <- function(model, 
                     xpred = NULL, 
                     seed = NULL, 
                     fit.line = TRUE,
                     ci = 0.95, 
                     npoints = "same",
                     orig_jitter = 0.10,
                     sim_jitter = 0.10) {
  data <- model$model
  data$source <- "original"
  yvar <- model$terms[[2]]
  if(is.null(xpred)){    #if xpred not provided, first predictor used
    xpred <- attr(model$terms, "term.labels")[1]
  }
  if(npoints == "same") {   #set number of points
    npoints <- length(model$y)
  }
  # need error warning for fit.line = TRUE and lm
  
  # data2 <- simulate
  if(!is.null(seed)){
    set.seed(seed)
  }
  data$sim <- simulate(model)$sim_1
  colours <- (c("Original" = "black", "Simulated" = "red"))
  sim <- "sim" #need this for aes_ to evaluate properly
  plot <- ggplot(data, aes_(x = as.name(xpred))) +
    geom_point(aes_(y = as.name(yvar), colour = "Original", alpha = 0.5), 
                    position = position_jitter(width = orig_jitter)) +
    geom_point(aes_(y = as.name(sim), colour = "Simulated", alpha = 0.5), 
               data = data,
               position = position_jitter(width = sim_jitter)) +
    labs(colour = "Data") +
    scale_colour_manual(values = colours) +
    guides(alpha = FALSE)  
  if(fit.line == TRUE){
    pred <- pred.fit(model = model, xpred = xpred, ci = ci)
    plot <- plot + 
      geom_line(data = pred, aes_(as.name(xpred), pred$est), colour = "red") +
      geom_line(data = pred, aes_(as.name(xpred), pred$upp), colour = "blue", linetype = 2) +
      geom_line(data = pred, aes_(as.name(xpred), pred$low), colour = "blue", linetype = 2)
  }
  return(plot)
}

#' @title Add model fit line (with SE) to GLM models (Poisson, negative binomial etc)
#'
#' @param model a model object, from (eg) lm glm
#' @param xpred the predictor to be plotted on the x axis
#' @param ci value for confidence interval (defaults to 0.95)
#'
#' @returns ggplot object with fit line
#'
#' @export pred.plot
#' 
#' @examples
#' #' ## Anwar M, Green JA, Norris P, et al 
#' ## Prospective daily diary study reporting of any and all symptoms in healthy 
#' ## adults in Pakistan: prevalence and #' response
#' ## BMJ Open 2017;7:e014998
#' \donttest{data(symptom)
#' glm.symptom <- glm(actual_help_days ~ symp_days_reported, 
#'      family = "poisson", data = symptom)
#' pred.plot(glm.symptom)}

pred.plot <- function(model,
                      xpred = NULL,
                      ci = 0.95
                      ){
  data <- model$model
  predv <- attr(model$terms, "term.labels")
  yvar <- model$terms[[2]]
  if(is.null(xpred)){    #if xpred not provided, first predictor used
    xpred <- predv[1]
  }
  pred <- pred.fit(model = model, xpred = xpred, ci = ci)
  ggplot(data, aes_(as.name(xpred), as.name(yvar))) +
    geom_point() +
    geom_line(data = pred, aes_(as.name(xpred), pred$est), colour = "red") +
    geom_line(data = pred, aes_(as.name(xpred), pred$upp), colour = "blue", linetype = 2) +
    geom_line(data = pred, aes_(as.name(xpred), pred$low), colour = "blue", linetype = 2)
}


