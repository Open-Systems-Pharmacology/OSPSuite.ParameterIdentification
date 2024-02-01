## -----------------------------------------------------------------------------
## The model cost and residuals
## Currently copied from the FME implementation
## -----------------------------------------------------------------------------

modCost <- function(model, obs, x = "time", y = NULL, err = NULL,
                    weight = "none", scaleVar = FALSE, cost = NULL, ...) {
  ## convert vector to matrix
  if (is.vector(obs)) {
    cn <- names(obs)
    obs <- matrix(data = obs, nrow = 1)
    colnames(obs) <- cn
  }
  if (is.vector(model)) {
    cn <- names(model)
    model <- matrix(data = model, nrow = 1)
    colnames(model) <- cn
  }

  # Special use case - no observed data provided. In this case, the error is
  # obviously zero. Return early.
  if (dim(obs)[[1]] == 0) {
    # If previous cost is provided, return it with no changes
    if (!is.null(cost)) {
      return(cost)
    }

    out <- list(
      model = 0,
      minlogp = 0,
      var = data.frame(
        name           = "Values",
        scale          = 1,
        N              = 0,
        SSR.unweighted = 0,
        SSR.unscaled   = 0,
        SSR            = 0
      ),
      residuals = data.frame(
        name = character(),
        x = numeric(),
        obs = numeric(),
        mod = numeric(),
        weight = numeric(),
        res.unweighted = numeric(),
        res = numeric()
      )
    )
    class(out) <- "modCost"
    return(out)
  }

  ## =============================================================================
  ## Observations
  ## =============================================================================

  ## The position of independent variable(s)
  ix <- 0
  if (!is.null(x)) { # mapping required...
    ## For now multiple independent variables are not supported...
    if (length(x) > 1) {
      stop("multiple independent variables in 'obs' are not yet supported")
    }

    if (!is.character(x)) {
      stop("'x' should be the *name* of the column with the independent variable in 'obs' or NULL")
    }
    ix <- which(colnames(obs) %in% x)
    if (length(ix) != length(x)) {
      stop(paste("Independent variable column not found in observations", x))
    }
  } else {
    ix <- NULL
  }

  ## The position of weighing values
  ierr <- 0
  if (!is.null(err)) {
    if (!is.character(err)) {
      stop("'err' should be the *name* of the column with the error estimates in obs or NULL")
    }
    ierr <- which(colnames(obs) == err) # only one
    if (length(ierr) == 0) {
      stop(paste("Column with error estimates not found in observations", err))
    }
  }

  ## The dependent variables
  type <- 1 # data input type: type 2 is table format, type 1 is long format...

  if (!is.null(y)) { # it is in table format; first column are names of observed data...

    Names <- as.character(unique(obs[, 1])) # Names of data sets, all data should be model variables...
    Ndat <- length(Names) # Number of data sets
    ilist <- 1:Ndat
    if (!is.character(y)) {
      stop("'y' should be the *name* of the column with the values of the dependent variable in obs")
    }
    iy <- which(colnames(obs) == y)
    if (length(iy) == 0) {
      stop(paste("Column with value of dependent variable not found in observations", y))
    }
    type <- 2
  } else { # it is a matrix, variable names are column names
    Ndat <- NCOL(obs) - 1
    Names <- colnames(obs)
    ilist <- (1:NCOL(obs)) # column positions of the (dependent) observed variables
    exclude <- ix # exclude columns that are not
    if (ierr > 0) {
      exclude <- c(ix, ierr)
    } # exclude columns that are not
    if (length(exclude) > 0) {
      ilist <- ilist[-exclude]
    }
  }

  # ================================
  # The model results
  # ================================

  ModNames <- colnames(model) # Names of model variables
  if (length(ix) > 1) {
    ixMod <- NULL

    for (i in 1:length(ix)) {
      ix2 <- which(colnames(model) == x[i])
      if (length(ix2) == 0) {
        stop(paste("Cannot calculate cost: independent variable not found in model output", x[i]))
      }
      ixMod <- c(ixMod, ix2)
    }

    xMod <- model[, ixMod] # Independent variable, model
  } else if (length(ix) == 1) {
    ixMod <- which(colnames(model) == x)
    if (length(ixMod) == 0) {
      stop(paste("Cannot calculate cost: independent variable not found in model output", x))
    }
    xMod <- model[, ixMod] # Independent variable, model
  }
  Residual <- NULL
  CostVar <- NULL

  # ================================
  # Compare model and data...
  # ================================
  xDat <- 0
  iDat <- 1:nrow(obs)

  for (i in ilist) { # for each observed variable ...
    ii <- which(ModNames == Names[i])
    if (length(ii) == 0) stop(paste("observed variable not found in model output", Names[i]))
    yMod <- model[, ii]
    if (type == 2) { # table format
      iDat <- which(obs[, 1] == Names[i])
      if (length(ix) > 0) xDat <- obs[iDat, ix]
      obsdat <- obs[iDat, iy]
    } else {
      if (length(ix) > 0) xDat <- obs[, 1]
      obsdat <- obs[, i]
    }
    ii <- which(is.na(obsdat))
    if (length(ii) > 0) {
      xDat <- xDat[-ii]
      obsdat <- obsdat[-ii]
    }

    if (length(ix) > 0) {
      # Only interpolate if more than one x point is available
      if (length(unique(xMod)) > 1) {
        ModVar <- approx(xMod, yMod, xout = xDat)$y
      } else {
        # Otherwise simply wort simulated y values based on the order of
        # observed x values
        ModVar <- yMod[match(xDat, xMod)]
      }
    } else {
      ModVar <- mean(yMod)
      obsdat <- mean(obsdat)
    }
    iex <- which(!is.na(ModVar))
    ModVar <- ModVar[iex]
    obsdat <- obsdat[iex]
    xDat <- xDat[iex]
    if (ierr > 0) {
      Err <- obs[iDat, ierr]
      Err <- Err[iex]
    } else {
      if (weight == "std") {
        Err <- sd(obsdat)
      } else if (weight == "mean") {
        Err <- mean(abs(obsdat))
      } else if (weight == "none") {
        Err <- 1
      } else {
        stop("error: do not recognize 'weight'; should be one of 'none', 'std', 'mean'")
      }
    }
    if (any(is.na(Err))) {
      stop(paste("error: cannot estimate weighing for observed variable: ", Names[i]))
    }
    if (min(Err) <= 0) {
      stop(paste("error: weighing for observed variable is 0 or negative:", Names[i]))
    }
    if (scaleVar) {
      Scale <- 1 / length(obsdat)
    } else {
      Scale <- 1
    }
    Res <- (ModVar - obsdat)
    res <- Res / Err
    resScaled <- res * Scale
    Residual <- rbind(
      Residual,
      data.frame(
        name = Names[i],
        x = xDat,
        obs = obsdat,
        mod = ModVar,
        weight = 1 / Err,
        res.unweighted = Res,
        res = res
      )
    )

    CostVar <- rbind(
      CostVar,
      data.frame(
        name           = Names[i],
        scale          = Scale,
        N              = length(Res),
        SSR.unweighted = sum(Res^2),
        SSR.unscaled   = sum(res^2),
        SSR            = sum(resScaled^2)
      )
    )
  } # end loop over all observed variables

  ## SSR
  Cost <- sum(CostVar$SSR * CostVar$scale)

  ## Corrected a bug in version 1.2
  # Lprob <- -sum(log(pmax(0, dnorm(Residual$mod, Residual$obs, Err))))
  Lprob <- -sum(log(pmax(0, dnorm(Residual$mod, Residual$obs, 1 / Residual$weight)))) # avoid log of negative values

  if (!is.null(cost)) {
    Cost <- Cost + cost$model
    CostVar <- rbind(CostVar, cost$var)
    Residual <- rbind(Residual, cost$residuals)
    Lprob <- Lprob + cost$minlogp
  }
  out <- list(model = Cost, minlogp = Lprob, var = CostVar, residuals = Residual)
  class(out) <- "modCost"
  return(out)
}

## -----------------------------------------------------------------------------
## S3 methods of modCost
## -----------------------------------------------------------------------------

plot.modCost <- function(x, legpos = "topleft", ...) {
  nvar <- nrow(x$var)

  dots <- list(...)

  dots$xlab <- if (is.null(dots$xlab)) "x" else dots$xlab
  dots$ylab <- if (is.null(dots$ylab)) "weighted residuals" else dots$ylab
  DotsPch <- if (is.null(dots$pch)) (16:24) else dots$pch
  dots$pch <- if (is.null(dots$pch)) rep(16:24, length.out = nvar)[x$residuals$name] else rep(dots$pch, length.out = nvar)[x$residuals$name] # Tom 02/01/2012: changed (16:24)  to rep(16:24,length.out=nvar); same for dots$pch in else part
  DotsCol <- if (is.null(dots$col)) (1:nvar) else dots$col
  dots$col <- if (is.null(dots$col)) (1:nvar)[x$residuals$name] else dots$col[x$residuals$name]

  do.call("plot", c(alist(x$residuals$x, x$residuals$res), dots))

  #  plot(x$residuals$x, x$residuals$res, xlab="x", ylab="weighted residuals",
  #     pch=c(16:24)[x$residuals$name],col=c(1:nvar)[x$residuals$name],...)

  if (!is.na(legpos)) {
    legend(legpos, legend = x$var$name, col = DotsCol, pch = DotsPch)
  }
}

#' Handle Simulation Failure
#'
#' This function checks for NA in simulation results and returns an infinite cost structure if any are found.
#' @param simulationResults A list of simulation results.
#' @return Returns an infinite cost structure if NA values are found in the simulation results, NULL otherwise.
#' @keywords internal
.handleSimulationFailure <- function(simulationResults) {
  if (any(is.na(simulationResults))) {
    return(.createErrorCostStructure(infinite = TRUE))
  }
  return(NULL)
}

#' Create Infinite Cost Structure
#'
#' Generates a cost structure with infinite values, used in cases of simulation failure.
#' @return Returns a list with infinite values for model, minlogp, var, and residuals components.
#' @keywords internal
.createErrorCostStructure <- function(infinite = FALSE) {
  if (infinite) {
    costValue <- Inf
    nValue <- 1
  } else {
    costValue <- 0
    nValue <- 0
  }

  errorCostStructure <- list(
    model = costValue,
    minlogp = costValue,
    var = data.frame(
      name = "Values", scale = 1, N = nValue,
      SSRUnweighted = costValue, SSRUnscaled = costValue, SSR = costValue
    ),
    residuals = data.frame(
      name = "Values", x = 0, obs = 0, mod = costValue,
      weight = 1, resUnweighted = costValue, res = costValue
    )
  )
  class(errorCostStructure) <- "modCost"
  return(errorCostStructure)
}

#' Apply Log Transformation to Data Frame
#'
#' Transforms the `yValues` and `lloq` columns in the given data frame using a log
#' transformation. Currently, this function only supports `obsVsPredDf` data frames,
#' which must contain `yDimension`, `yUnit`, `yValues`, and `lloq` columns.
#'
#' @param df A `tbl_df` representing the observed vs predicted data frame (`obsVsPredDf`).
#'
#' @return A transformed data frame with log-transformed `yValues` and `lloq`.
#' @keywords internal
#'
#' @examples
#' # Assuming df is a valid obsVsPredDf data frame
#' transformedDf <- applyLogTransformation(df)
.applyLogTransformation <- function(df) {
  ospsuite.utils::validateIsOfType(df, "tbl_df")
  ospsuite.utils::validateIsIncluded(
    c("yDimension", "yUnit", "yValues", "lloq"), colnames(df)
  )

  UNITS_EPSILON <- ospsuite::toUnit(
    quantityOrDimension = df$yDimension[1],
    values = ospsuite::getOSPSuiteSetting("LOG_SAFE_EPSILON"),
    targetUnit = df$yUnit[1],
    molWeight = 1
  )

  df$yValues <- ospsuite.utils::logSafe(
    df$yValues, epsilon = UNITS_EPSILON, base = exp(1)
  )
  df$lloq <- ospsuite.utils::logSafe(
    df$lloq, epsilon = UNITS_EPSILON, base = exp(1)
  )

  return(df)
}
