# plotOFVProfiles()

resetTestFactories()

test_that("plotOFVProfiles() errors when called with no argument", {
  expect_error(
    plotOFVProfiles(),
    "calculateOFVProfiles"
  )
})

test_that("plotOFVProfiles() errors when called with NULL", {
  expect_error(
    plotOFVProfiles(NULL),
    "calculateOFVProfiles"
  )
})

test_that("plotOFVProfiles() returns one ggplot per profile", {
  profiles <- list(
    `paramX` = tibble::tibble(`paramX` = seq(0, 1, length.out = 5), ofv = c(3, 2, 1, 2, 3)),
    `Group|Sub|paramY` = tibble::tibble(`Group|Sub|paramY` = seq(-1, 1, length.out = 5), ofv = c(5, 4, 3, 4, 5))
  )
  plots <- plotOFVProfiles(profiles)
  expect_type(plots, "list")
  expect_length(plots, length(profiles))
  expect_true(all(vapply(plots, inherits, logical(1), what = "ggplot")))
})

test_that("plotOFVProfiles() handles end-to-end output of calculateOFVProfiles()", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  ofvProfiles <- piTask$calculateOFVProfiles(totalEvaluations = 5)
  plots <- plotOFVProfiles(ofvProfiles)
  expect_length(plots, length(ofvProfiles))
  expect_true(all(vapply(plots, inherits, logical(1), what = "ggplot")))
})

test_that("plotOFVProfiles() x-axis label is the last `|`-segment of the parameter path", {
  longPath <- "A|B|paramZ"
  profileTibble <- tibble::tibble(!!longPath := 1:3, ofv = c(2, 1, 2))
  profiles <- stats::setNames(list(profileTibble), longPath)
  plot <- plotOFVProfiles(profiles)[[1]]
  expect_equal(plot$labels$x, "paramZ")
  expect_equal(plot$labels$title, longPath)
})
