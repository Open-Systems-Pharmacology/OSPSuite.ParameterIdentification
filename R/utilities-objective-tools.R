#' Objective function values on a grid
#'
#' @param fn `function(values)` that returns the objective function value of
#'   one vector of parameter values.
#' @param lower,upper Numeric vectors of bounds, one entry per parameter.
#' @param logScaleFlag Logical vector, one entry per parameter.
#' @param totalEvaluations The total number of grid points.
#' @param parameterNames Character vector of the column names of the grid.
#' @return A tibble with one column per parameter and the column `ofv`.
#' @keywords internal
#' @noRd
.ofvGrid <- function(
  fn,
  lower,
  upper,
  logScaleFlag,
  totalEvaluations,
  parameterNames
) {
  nrOfParameters <- length(parameterNames)
  gridSize <- floor(totalEvaluations^(1 / nrOfParameters))
  parameterGrid <- vector("list", nrOfParameters)
  names(parameterGrid) <- parameterNames

  for (idx in seq_len(nrOfParameters)) {
    if (logScaleFlag[idx]) {
      if (lower[idx] <= 0 | upper[idx] <= 0) {
        stop(messages$logScaleFlagError())
      }
      parameterGrid[[idx]] <- exp(seq(
        log(lower[idx]),
        log(upper[idx]),
        length.out = gridSize
      ))
    } else {
      parameterGrid[[idx]] <- seq(
        lower[idx],
        upper[idx],
        length.out = gridSize
      )
    }
  }

  ofvGrid <- expand.grid(parameterGrid)
  ofvGrid[["ofv"]] <- vapply(
    seq_len(nrow(ofvGrid)),
    function(i) fn(as.numeric(ofvGrid[i, ])),
    numeric(1)
  )

  tibble::as_tibble(ofvGrid)
}

#' Objective function value profiles along each parameter
#'
#' @param fn `function(values)` that returns the objective function value of
#'   one vector of parameter values.
#' @param par Numeric vector of the parameter values the profiles are centred
#'   on.
#' @param boundFactor The relative half width of the profile around `par`.
#' @param totalEvaluations The number of grid points per profile.
#' @param parameterNames Character vector of the parameter names.
#' @return A named list of tibbles, one per parameter, with the column of the
#'   parameter values and the column `ofv`.
#' @keywords internal
#' @noRd
.ofvProfiles <- function(fn, par, boundFactor, totalEvaluations, parameterNames) {
  nrOfParameters <- length(parameterNames)
  lower <- ifelse(par < 0, (1 + boundFactor) * par, (1 - boundFactor) * par)
  upper <- ifelse(par < 0, (1 - boundFactor) * par, (1 + boundFactor) * par)

  # One row per evaluation, one column per parameter, indexed by position so
  # that parameters sharing a path do not collide
  defaultGrid <- matrix(
    par,
    nrow = totalEvaluations,
    ncol = nrOfParameters,
    byrow = TRUE
  )

  profileList <- vector(mode = "list", length = nrOfParameters)
  for (idx in seq_len(nrOfParameters)) {
    grid <- seq(lower[[idx]], upper[[idx]], length.out = totalEvaluations)
    currentGrid <- defaultGrid
    currentGrid[, idx] <- grid

    ofvValues <- numeric(totalEvaluations)
    for (gridIdx in seq_len(totalEvaluations)) {
      ofvValues[gridIdx] <- fn(currentGrid[gridIdx, ])
    }

    profileList[[idx]] <- tibble::tibble(
      !!parameterNames[idx] := grid,
      ofv = ofvValues
    )
  }
  names(profileList) <- parameterNames

  profileList
}
