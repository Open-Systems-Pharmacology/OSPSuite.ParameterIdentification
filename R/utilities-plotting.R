#' @description
#' Plot a heatmap of the objective function calculated by the gridSearch() method
#' @param grid A tibble with three columns. The first two columns should contain parameter values
#' for a 2-parameter optimization problem, and the `ofv` column should contain the matching objective function values.
#' @param point A vector of length 2 with the X- and Y-coordinates of a point that would be plotted in white
#' on top of the heatmap.
#' @return A ggplot2 object.
plot2DOFVGrid = function(grid, point = NULL) {
  # This plot only makes sense for 2-parametric problems, the `grid` data frame should have 3 columns
  if (ncol(grid) != 3) {
    stop(messages$plotGridParameterCount(ncol(grid)))
  }
  xParameterName <- names(grid)[[1]]
  yParameterName <- names(grid)[[2]]
  plot <- ggplot2::ggplot(data = grid,
                          ggplot2::aes(x = .data[[xParameterName]], y = .data[[yParameterName]])) +
    ggplot2::theme_bw() +
    ggplot2::geom_contour_filled(ggplot2::aes(z = 1/ofv)) +
    ggplot2::labs(x = xParameterName, y = yParameterName) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::guides(fill = "none")
  if (!missing(point)) {
    plot <- plot + ggplot2::geom_point(x = point[[1]],
                                       y = point[[2]],
                                       size = 3,
                                       color = "white")
  }
  return(plot)
}


#' @description
#' Plot the profiles of the objective function calculated by the calculateOFVProfiles() method.
#' @param profiles A list of tibbles, with each tibble containing two columns. The first column should contain
#' several values for the parameter, and the second column, called `ofv`, should contain the matching objective function value.
#' @return A list of ggplot2 objects, one for each parameter.
plotOFVProfiles = function(profiles) {
  if (missing(profiles)) {
    stop(messages$profilesNotSupplied)
  }
  plotList <- vector(mode = "list", length = length(profiles))
  for (idx in seq_along(profiles)) {
    parameterName <- names(profiles)[[idx]]
    plotList[[idx]] <- ggplot2::ggplot(data = profiles[[idx]],
                                       ggplot2::aes(x = .data[[parameterName]], y = .data[["ofv"]])) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(ggplot2::aes(col = 1/ofv)) +
      ggplot2::labs(x = parameterName, y = "OFV") +
      ggplot2::scale_color_viridis_c() +
      ggplot2::guides(col = "none")
  }
  return(plotList)
}
