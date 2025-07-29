#' @title Optimizer Class
#'
#' @description Internal class for handling optimization in parameter identification.
#' Provides a unified interface for different optimization algorithms and supports
#' multiple methods for estimating parameter uncertainty.
#'
#' @keywords internal
#' @noRd
Optimizer <- R6::R6Class(
  "Optimizer",
  cloneable = FALSE,
  active = list(
    #' @field algorithm Optimization algorithm name. Read only. See
    #' [`ospsuite.parameteridentification::Algorithms`] for a list of supported
    #' algorithms.
    algorithm = function(value) {
      if (missing(value)) {
        private$.configuration$algorithm
      } else {
        stop(messages$errorPropertyReadOnly("algorithm"))
      }
    },
    #' @field ciMethod Confidence interval estimation method. Read only. See
    #' [`ospsuite.parameteridentification::CIMethods`] for a list of supported
    #' methods.
    ciMethod = function(value) {
      if (missing(value)) {
        private$.configuration$ciMethod
      } else {
        stop(messages$errorPropertyReadOnly("ciMethod"))
      }
    },
    #' @field modelCostField Name of the cost field returned in optimization.
    #' Read only,
    modelCostField = function(value) {
      if (missing(value)) {
        private$.configuration$modelCostField
      } else {
        stop(messages$errorPropertyReadOnly("modelCostField"))
      }
    }
  ),
  private = list(
    # `PIConfiguration` object instance
    .configuration = NULL,
    # Print optimization status messages
    .verbose = NULL,

    # Hooke-Jeeves optimization using dfoptim::hjkb
    .runHJKB = function(par, fn, lower, upper, controlOptim, fixedParams = NULL) {
      dfoptim::hjkb(
        par = par,
        fn = if (!is.null(fixedParams)) {
          function(p) fn(private$.updateFixedParams(p, fixedParams))
        } else {
          fn
        },
        lower = lower,
        upper = upper,
        control = controlOptim
      )
    },

    # BOBYQA optimization using nloptr::bobyqa
    .runBOBYQA = function(par, fn, lower, upper, controlOptim, fixedParams = NULL) {
      nloptr::bobyqa(
        x0 = par,
        fn = if (!is.null(fixedParams)) {
          function(p) fn(private$.updateFixedParams(p, fixedParams))
        } else {
          fn
        },
        lower = lower,
        upper = upper,
        control = controlOptim
      )
    },

    # Differential Evolution optimization using DEoptim::DEoptim
    .runDEoptim = function(par, fn, lower, upper, controlOptim, fixedParams = NULL) {
      DEoptim::DEoptim(
        fn = if (!is.null(fixedParams)) {
          function(p) fn(private$.updateFixedParams(p, fixedParams))
        } else {
          fn
        },
        lower = lower,
        upper = upper,
        control = controlOptim
      )
    },

    # Estimate confidence intervals using Hessian matrix approximation
    .estimateHessianCI = function(par, fn, controlCI) {
      result <- private$.initializeCIResult()

      epsilon <- controlCI$epsilon %||% pmax(1e-8, pmin(1e-4, 0.1 * abs(par)))
      zScore <- qnorm(1 - (1 - controlCI$confLevel) / 2)

      # Compute the Hessian matrix numerically
      hess <- tryCatch(
        {
          numDeriv::hessian(fn, x = par, method.args = list(eps = epsilon))
        },
        error = function(e) {
          result$error <- messages$ciEstimationError(
            "Hessian calculation", e$message
          )
          NULL
        }
      )

      # Currently assumes SSR objective; extend with MLE logic as needed
      # For SSR-based objective: Cov(θ) = σ² * (H / 2)^(-1), with σ² = SSR / (n - p)
      # For MLE-based cost, use Cov(θ) = (H / 2)^(-1) without scaling
      cost <- fn(p = par, modelCostField = NULL)
      ssr <- purrr::pluck(
        cost, "costVariables", "weightedSSR",
        .default = 1
      )
      nObs <- purrr::pluck(
        cost, "costVariables", "nObservations",
        .default = length(par) + 1
      )
      dof <- max(1, nObs - length(par)) # ensure DoF ≥ 1
      sigma2 <- ssr / dof # for MLE use: sigma2 = 1

      if (!is.null(hess)) {
        # Invert Hessian to obtain covariance matrix
        covMat <- tryCatch(
          {
            sigma2 * solve(hess / 2)
          },
          error = function(e) {
            if (is.null(result$error)) {
              result$error <- messages$ciEstimationError(
                "Covariance matrix calculation", e$message
              )
            }
            NULL
          }
        )

        # Compute standard errors and CIs
        if (!is.null(covMat)) {
          # Check if covariance matrix is positive definite
          eigenValues <- tryCatch(
            eigen(covMat, symmetric = TRUE, only.values = TRUE)$values,
            error = function(e) NA_real_
          )

          if (any(is.na(eigenValues)) || any(eigenValues <= 0)) {
            result$error <- messages$ciEstimationError(
              "Covariance matrix validity check",
              "Covariance matrix is not positive definite."
            )
          } else {
            result$details$hessian <- hess
            result$details$covMat <- covMat
            result$details$eigen <- eigenValues

            result$sd <- sqrt(diag(covMat))
            result$cv <- result$sd / abs(par)
            result$lowerCI <- par - zScore * result$sd
            result$upperCI <- par + zScore * result$sd

            # Compute correlation matrix
            corMat <- tryCatch(
              covMat / (result$sd %o% result$sd),
              error = function(e) NA_real_
            )
            result$details$corMat <- corMat
          }
        }
      }

      return(result)
    },

    # Estimate confidence intervals using Profile Likelihood method.
    .estimateCIProfileLikelihood = function(par, fn, controlCI, optimizer) {
      result <- private$.initializeCIResult()

      # Calculate cost threshold based on confidence level (chi-squared criterion)
      controlCI$costThreshold <- 0.5 * fn(par) + qchisq(controlCI$confLevel, df = 1)

      zScore <- qnorm(1 - (1 - controlCI$confLevel) / 2)

      lowerCI <- rep(NA_real_, length(par))
      upperCI <- rep(NA_real_, length(par))
      paramHistory <- NULL

      # Loop over each parameter for profiling
      for (p in seq_along(par)) {
        message(messages$statusProfileLikelihood(p, par[p]))

        epsilon <- controlCI$epsilon %||% pmax(1e-8, 0.01 * abs(par[p]))

        lowerResult <- private$.computeProfileCI(par, fn, optimizer, p, -1, controlCI)
        upperResult <- private$.computeProfileCI(par, fn, optimizer, p, 1, controlCI)

        lowerCI[p] <- lowerResult$ci
        upperCI[p] <- upperResult$ci

        paramHistory <- rbind(paramHistory, lowerResult$paramHistory, upperResult$paramHistory)
      }

      # Compute standard errors and CIs
      names(result)[names(result) == "sd"] <- "se"
      names(result)[names(result) == "cv"] <- "rse"
      result$lowerCI <- lowerCI
      result$upperCI <- upperCI
      result$se <- (upperCI - lowerCI) / (2 * zScore)
      result$rse <- result$se / abs(par)
      result$details$paramHistory <- as.data.frame(paramHistory)

      return(result)
    },

    # Profile likelihood confidence interval estimation for one parameter.
    .computeProfileCI = function(par, fn, optimizer, p, direction, controlCI) {
      ci <- NA_real_

      epsilon <- controlCI$epsilon
      if (is.null(epsilon)) {
        epsilon <- pmax(1e-8, 0.01 * abs(par[p]))
      } else if (length(epsilon) > 1) {
        epsilon <- epsilon[p]
      }

      costThreshold <- controlCI$costThreshold
      maxIter <- controlCI$maxIter

      # Set parameter bounds for optimization
      lower <- par - ((maxIter + 1) * epsilon)
      upper <- par + ((maxIter + 1) * epsilon)

      paramHistory <- vector("list", maxIter)

      for (i in seq_len(maxIter)) {
        newPar <- par
        newPar[p] <- par[p] + direction * epsilon * i

        fixedParams <- list(idx = p, values = newPar[p])

        # Run optimization with current fixed parameter
        optimResult <- optimizer$run(
          par = newPar,
          fn = fn,
          lower = lower,
          upper = upper,
          fixedParams = fixedParams
        )

        costNew <- 0.5 * optimResult$value # fn(optimResult$par)
        paramHistory[[i]] <- c(p, i, newPar[p], costNew)

        # Trigger .NET gc
        ospsuite::clearMemory()

        # Stop when cost threshold is crossed
        if (costNew > costThreshold) break
        ci <- newPar[p]
      }

      # If max iterations reached without threshold, set CI to Inf
      if (i == controlCI$maxIter && costNew < costThreshold) {
        warning(messages$plMaxiterWarning(p))
        ci <- Inf * direction
      }

      paramHistory <- do.call(rbind, paramHistory[1:i])
      colnames(paramHistory) <- c("Parameter", "Iteration", "Value", "Cost")

      return(list(ci = ci, paramHistory = paramHistory))
    },

    # Estimate confidence intervals using Bootstrap method.
    .estimateBootstrapCI = function(par, fn, lower, upper, controlCI, optimizer = NULL) {
      result <- private$.initializeCIResult()

      nBootstrap <- controlCI$nBootstrap %||% 100

      # Set global seed to ensure reproducible seedVector generation
      seed <- controlCI$seed %||% sample(1e6, 1)
      set.seed(seed)
      seedVector <- sample(1e6, nBootstrap)

      bootstrapResults <- matrix(nrow = nBootstrap, ncol = length(par))

      # Bootstrap loop
      for (i in seq_len(nBootstrap)) {
        message(messages$statusBootstrap(i, nBootstrap))

        # Pass individual seed to obj. function to resample observed data reproducibly
        bootstrapFn <- function(p) fn(p, bootstrapSeed = seedVector[i])

        # Re-optimize with resampled data
        optimResult <- self$run(
          par = par,
          fn = bootstrapFn,
          lower = lower,
          upper = upper
        )

        bootstrapResults[i, ] <- optimResult$par

        # Trigger .NET gc
        ospsuite::clearMemory()
      }

      bootstrapResults <- bootstrapResults[
        stats::complete.cases(bootstrapResults), ,
        drop = FALSE
      ]

      # Compute standard errors and CIs
      lowerLevel <- (1 - controlCI$confLevel) / 2
      upperLevel <- (1 + controlCI$confLevel) / 2
      result$lowerCI <- apply(bootstrapResults, 2, quantile, probs = lowerLevel)
      result$upperCI <- apply(bootstrapResults, 2, quantile, probs = upperLevel)
      result$sd <- apply(bootstrapResults, 2, function(x) {
        sd(.stabilizeBootstrapCV(x), na.rm = TRUE)
      })
      result$cv <- result$sd / abs(par)

      # Compute correlation matrix
      result$details$bootstrapResults <- bootstrapResults
      result$details$corMat <- cor(bootstrapResults)

      return(result)
    },

    # Update fixed parameters in the parameter vector
    .updateFixedParams = function(par, fixedParams) {
      if (is.null(fixedParams)) {
        return(par)
      }
      if (length(fixedParams$idx) >= length(par)) {
        stop(messages$fixedParamError("fixed"))
      }
      if (length(fixedParams$idx) != length(fixedParams$values)) {
        stop(messages$fixedParamError("length"))
      }

      par[fixedParams$idx] <- fixedParams$values

      return(par)
    },

    # Converts the objective function to handle fixed parameters and extract
    # modelCost with dynamic overrides
    .preprocessFn = function(fn, fixedParams) {
      # If fn is already preprocessed, return it to prevent infinite recursion
      if (inherits(fn, "preprocessedFn")) {
        return(fn)
      }

      wrappedFn <- function(p, ..., modelCostField) {
        result <- fn(p, ...)

        if (missing(modelCostField)) {
          modelCostField <- private$.configuration$modelCostField
        }

        if (is.null(modelCostField) || !is.list(result)) {
          return(result)
        }

        if (!modelCostField %in% names(result)) {
          stop(messages$objectiveFnOutputError(modelCostField))
        }

        return(purrr::pluck(result, modelCostField))
      }

      class(wrappedFn) <- "preprocessedFn"
      return(wrappedFn)
    },

    # Formats the optimization output into a standardized result structure
    .formatOptimizationOutput = function(optimResult) {
      baseResult <- list(
        par = NULL,
        value = NULL,
        convergence = NULL,
        iterations = NULL,
        fnEvaluations = NULL,
        algorithm = private$.configuration$algorithm,
        elapsed = NULL
      )

      if (private$.configuration$algorithm == "HJKB") {
        baseResult$par <- optimResult$par
        baseResult$value <- optimResult$value
        baseResult$convergence <- optimResult$convergence == 0
        baseResult$iterations <- optimResult$niter
        baseResult$fnEvaluations <- optimResult$feval
      } else if (private$.configuration$algorithm == "BOBYQA") {
        baseResult$par <- optimResult$par
        baseResult$value <- optimResult$value
        baseResult$convergence <- optimResult$convergence > 0
        if (!baseResult$convergence) {
          baseResult$convergence$error <- optimResult$convergence
        }
        baseResult$iterations <- optimResult$iter
        baseResult$fnEvaluations <- optimResult$iter
      } else if (private$.configuration$algorithm == "DEoptim") {
        bestvalit <- optimResult$member$bestvalit
        lastN <- min(5, length(bestvalit))
        relChange <- try(
          {
            abs(diff(tail(bestvalit, lastN))) / tail(bestvalit, lastN - 1)
          },
          silent = TRUE
        )
        baseResult$convergence <- if (inherits(relChange, "try-error")) {
          FALSE
        } else {
          all(relChange < 1e-5)
        }
        baseResult$par <- unname(optimResult$optim$bestmem)
        baseResult$value <- optimResult$optim$bestval
        baseResult$iterations <- optimResult$optim$iter
        baseResult$fnEvaluations <- optimResult$optim$nfeval
      }

      return(baseResult)
    },

    # Initialize confidence interval (CI) result list
    .initializeCIResult = function() {
      list(
        sd = NULL,
        cv = NULL,
        lowerCI = NULL,
        upperCI = NULL,
        error = NULL,
        method = private$.configuration$ciMethod,
        elapsed = NULL,
        details = list()
      )
    }
  ),
  public = list(
    #' @description Initialize an Optimizer instance for parameter estimation.
    #'
    #' @param configuration `PIConfiguration` for additional settings. For details
    #' on creating a `PIConfiguration` object, see
    #' [`ospsuite.parameteridentification::PIConfiguration`].
    initialize = function(configuration) {
      ospsuite.utils::validateIsOfType(configuration, "PIConfiguration")

      private$.configuration <- configuration
      private$.verbose <- TRUE
    },

    #' @description Run optimization process using the selected algorithm
    #' @param par Numeric vector of initial parameter values
    #' @param fn Objective function to be minimized
    #' @param lower Numeric vector of lower parameter bounds
    #' @param upper Numeric vector of upper parameter bounds
    #' @param fixedParams Optional list with fixed parameters containing two
    #' elements:
    #' - **`idx`**: Numeric vector specifying indices of parameters to fix
    #' - **`values`**: Numeric vector with corresponding fixed values
    #' @return List with optimization results including parameter estimates,
    #' cost, and residuals
    run = function(par, fn, lower, upper, fixedParams = NULL) {
      ospsuite.utils::validateIsNumeric(par)
      ospsuite.utils::validateIsNumeric(lower)
      ospsuite.utils::validateIsNumeric(upper)
      ospsuite.utils::validateIsOfType(fixedParams, "list", TRUE)
      ospsuite.utils::validateIsIncluded(
        names(fixedParams), c("idx", "values"), TRUE
      )
      if (!inherits(fn, "preprocessedFn")) {
        ospsuite.utils::validateIsOfType(fn, "function", FALSE)
      }

      fn <- private$.preprocessFn(fn)
      algorithm <- private$.configuration$algorithm
      algorithmOptions <- private$.configuration$algorithmOptions %||%
        AlgorithmDefaults[[algorithm]]

      optimizeFn <- switch(algorithm,
        "HJKB" = private$.runHJKB,
        "BOBYQA" = private$.runBOBYQA,
        "DEoptim" = private$.runDEoptim,
        stop(messages$optimizationAlgorithm(algorithm, error = TRUE))
      )

      if (private$.verbose) {
        message(messages$optimizationAlgorithm(algorithm, par, FALSE))
      }

      startTime <- proc.time()
      rawResult <- optimizeFn(
        par          = par,
        fn           = fn,
        lower        = lower,
        upper        = upper,
        controlOptim = algorithmOptions,
        fixedParams  = fixedParams
      )
      elapsedTime <- proc.time() - startTime

      optimResult <- private$.formatOptimizationOutput(rawResult)
      optimResult$elapsed <- elapsedTime[["elapsed"]]

      optimResult$par <- private$.updateFixedParams(optimResult$par, fixedParams)

      return(optimResult)
    },

    #' @description Estimate confidence intervals using the selected method
    #' (Hessian, Profile Likelihood, Bootstrap).
    #'
    #' @param par Numeric vector of parameter values for confidence interval
    #' estimation.
    #' @param fn Objective function used for parameter estimation.
    #' @param lower Numeric vector of lower parameter bounds.
    #' @param upper Numeric vector of upper parameter bounds.
    #' @param optimizer Optional instance of Optimizer used during profile
    #' likelihood or bootstrap CI estimation.
    #' @return List with confidence interval results including lower and upper
    #' bounds, standard errors, coefficient of variation, and method details.
    estimateCI = function(par, fn, lower, upper, optimizer = NULL) {
      ospsuite.utils::validateIsNumeric(par)
      ospsuite.utils::validateIsNumeric(lower)
      ospsuite.utils::validateIsNumeric(upper)
      ospsuite.utils::validateIsOfType(optimizer, "Optimizer", TRUE)
      if (!inherits(fn, "preprocessedFn")) {
        ospsuite.utils::validateIsOfType(fn, "function", FALSE)
      }

      private$.verbose <- FALSE

      fn <- private$.preprocessFn(fn)
      ciMethod <- private$.configuration$ciMethod
      ciOptions <- private$.configuration$ciOptions %||% CIDefaults[[ciMethod]]
      optimizer <- optimizer %||% self

      estimateCIFn <- switch(ciMethod,
        "hessian" = private$.estimateHessianCI,
        "PL" = private$.estimateCIProfileLikelihood,
        "bootstrap" = private$.estimateBootstrapCI,
        stop(messages$ciMethod(ciMethod, error = TRUE))
      )

      allowedArgs <- names(formals(estimateCIFn))
      argsList <- list(
        par = par,
        fn = fn,
        lower = lower,
        upper = upper,
        controlCI = ciOptions,
        optimizer = optimizer
      )
      filteredArgs <- argsList[intersect(names(argsList), allowedArgs)]

      message(messages$ciMethod(ciMethod, par, FALSE))

      startTime <- proc.time()
      rawResult <- do.call(estimateCIFn, filteredArgs)
      elapsedTime <- proc.time() - startTime

      rawResult$elapsed <- elapsedTime[["elapsed"]]

      private$.verbose <- TRUE

      return(rawResult)
    }
  )
)
