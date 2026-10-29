#' Plot Objective Function Value (OFV) Profiles
#'
#' @description
#' Visualizes the OFV profiles produced by
#' `ParameterIdentification$calculateOFVProfiles()`. Each profile is rendered
#' as a scatter plot of the OFV against the corresponding parameter value,
#' with point color encoding OFV (lower = brighter, reversed Viridis).
#'
#' @details
#' Around a (local) minimum the objective function is expected to be convex
#' and roughly parabolic. Inspecting the OFV profile around a tentative
#' optimum is therefore a quick visual check that the optimization has
#' converged to a sensible point in parameter space.
#'
#' Because parameter paths in OSP simulations can be long
#' (e.g. `Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec`),
#' the x-axis label uses only the last `|`-separated segment, while the full
#' path is shown as the plot title.
#'
#' @param profiles A named list of tibbles as returned by
#'   `ParameterIdentification$calculateOFVProfiles()`. Each tibble must
#'   contain exactly two columns: the parameter values (named after the
#'   parameter's path) and the matching objective function values (named
#'   `ofv`). The list names must match the parameter-value column names.
#'
#' @return A list of `ggplot` objects, one per parameter profile, in the
#'   same order as `profiles`.
#'
#' @seealso [ParameterIdentification] for `calculateOFVProfiles()`.
#'
#' @examples
#' \dontrun{
#' # piTask is a configured ParameterIdentification instance
#' ofvProfiles <- piTask$calculateOFVProfiles()
#' plots <- plotOFVProfiles(ofvProfiles)
#' plots[[1]]
#' }
#'
#' @export
plotOFVProfiles <- function(profiles) {
  if (missing(profiles) || is.null(profiles)) {
    stop(messages$profilesNotSupplied())
  }
  plotList <- vector(mode = "list", length = length(profiles))
  for (idx in seq_along(profiles)) {
    parameterName <- names(profiles)[[idx]]
    axisLabel <- sub(".*\\|", "", parameterName)
    plotList[[idx]] <- ggplot2::ggplot(
      data = profiles[[idx]],
      ggplot2::aes(x = .data[[parameterName]], y = .data[["ofv"]])
    ) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(
        ggplot2::aes(col = .data[["ofv"]]),
        shape = 16,
        size = 2
      ) +
      ggplot2::labs(title = parameterName, x = axisLabel, y = "OFV") +
      ggplot2::scale_color_viridis_c(direction = -1) +
      ggplot2::guides(col = "none")
  }
  return(plotList)
}

#' Observed and predicted data of each output mapping
#'
#' @param simulationResults The results of `SimulationModel$simulate()`, a list
#'   by simulation id of lists of `SimulationResults`.
#' @param outputMappings List of `PIOutputMapping` objects.
#' @return List of `DataCombined` objects, one per output mapping, with the
#'   simulated and the observed data in one group named after the output path.
#' @keywords internal
#' @noRd
.observedVsPredicted <- function(simulationResults, outputMappings) {
  obsVsPredList <- vector("list", length(outputMappings))
  for (idx in seq_along(outputMappings)) {
    obsVsPred <- ospsuite::DataCombined$new()
    currOutputMapping <- outputMappings[[idx]]
    # Construct group names out of output path and simulation id
    groupName <- currOutputMapping$quantity$path
    # In each iteration, only one values set per simulation batch is
    # simulated. Therefore we always need the first results entry of the
    # simulation that is the parent of the output quantity.
    resultObject <- simulationResults[[currOutputMapping$simId]][[1]]
    obsVsPred$addSimulationResults(
      resultObject,
      quantitiesOrPaths = currOutputMapping$quantity$path,
      names = groupName,
      groups = groupName
    )
    # Observed data in the same group, with the data transformations of
    # the output mapping
    .addObservedData(obsVsPred, currOutputMapping)
    obsVsPredList[[idx]] <- obsVsPred
  }
  obsVsPredList
}

#' Plots of the observed and predicted data of each output mapping
#'
#' @inheritParams .observedVsPredicted
#' @return A list of `patchwork` objects, one per output mapping, with the
#'   time profile, the predicted against the observed values and the
#'   residuals over time.
#' @keywords internal
#' @noRd
.plotObsVsPred <- function(simulationResults, outputMappings) {
  dataCombined <- .observedVsPredicted(simulationResults, outputMappings)

  previousTheme <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(previousTheme), add = TRUE)
  ggplot2::theme_update(legend.title = ggplot2::element_blank())

  # ospsuite.plots sets explicit guide titles via guide_legend(title = ...),
  # which override theme(legend.title). Strip them with labs(NULL) so the
  # collected legend has no title, and drop redundant aesthetic legends so
  # the three sub-plots can be merged into a single legend.
  stripGuides <- ggplot2::labs(
    colour = NULL,
    fill = NULL,
    shape = NULL,
    linetype = NULL
  )

  multiPlot <- lapply(seq_along(dataCombined), function(idx) {
    scaling <- outputMappings[[idx]]$scaling
    axisScale <- if (scaling == "lin") "linear" else "log"

    # The simulated line (linetype = name) and observed point (shape =
    # name) each carry their own clean, specifically-labelled legend guide.
    # Show those and hide the redundant colour/group guide (whose key mixes
    # line and point). Pin the two guides' order so the collected legend is
    # deterministic, which ggplot otherwise leaves unstable across runs.
    indivTimeProfile <- ospsuite::plotTimeProfile(
      dataCombined[[idx]],
      yScale = axisScale
    ) +
      stripGuides +
      ggplot2::guides(
        colour = "none",
        fill = "none",
        linetype = ggplot2::guide_legend(order = 1),
        shape = ggplot2::guide_legend(order = 2)
      )
    predVsObs <- ospsuite::plotPredictedVsObserved(
      dataCombined[[idx]],
      xyScale = axisScale
    ) +
      stripGuides +
      ggplot2::guides(
        colour = "none",
        fill = "none",
        shape = "none"
      )
    resVsTime <- ospsuite::plotResidualsVsCovariate(
      dataCombined[[idx]],
      xAxis = "time",
      residualScale = axisScale
    ) +
      stripGuides +
      ggplot2::guides(
        colour = "none",
        fill = "none",
        shape = "none",
        linetype = "none"
      )

    patchwork::wrap_plots(
      list(indivTimeProfile, predVsObs, resVsTime),
      ncol = 1
    ) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(
        legend.position = "top",
        legend.title = ggplot2::element_blank(),
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.spacing.y = ggplot2::unit(0, "pt"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        aspect.ratio = 0.4
      )
  })

  multiPlot
}
