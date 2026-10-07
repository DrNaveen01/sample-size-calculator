# Run from the project root. Every output uses the same report source.
for (path in c("single_proportion.R", "planning.R", "report.R", "reports_extended.R", "reports_objectives.R", "exports.R")) source(file.path("R", path))
dir.create("output", showWarnings = FALSE)
results <- list(
  single_proportion = single_proportion(.5, .05, nonresponse = .1),
  single_mean = single_mean(sd = 10, precision = 2, nonresponse = .1),
  two_proportions = two_proportions(.2, .3, power = .8, ratio = 2, nonresponse = .1),
  two_means = two_means(100, 105, sd1 = 15, sd2 = 20, power = .8, ratio = 2, nonresponse = .1),
  two_proportions_power = two_proportions(.2, .3, n1 = 100, n2 = 200),
  two_means_power = two_means(100, 105, sd1 = 15, sd2 = 20, n1 = 100, n2 = 200),
  two_means_pooled = two_means(100, 105, sd_method = "pooled", pooled_sd = 15, nonresponse = .1,
    group1 = "Standard care", group2 = "New treatment"),
  two_means_superiority = two_means(100, 105, 15, objective = "superiority", alpha = .025, nonresponse = .1,
    group1 = "Standard care", group2 = "New treatment"),
  two_means_noninferiority = two_means(100, 100, 15, 20, objective = "noninferiority", margin = 5,
    alpha = .025, ratio = 2, nonresponse = .1, sd_method = "reference", ref_n1 = 50, ref_n2 = 100,
    group1 = "Standard care", group2 = "New treatment"),
  two_means_equivalence = two_means(100, 101, 15, 20, objective = "equivalence", lower = -5, upper = 5,
    alpha = .05, ratio = 2, nonresponse = .1, group1 = "Standard care", group2 = "New treatment"),
  two_proportions_superiority = two_proportions(.2, .3, objective = "superiority", alpha = .025,
    ratio = 2, nonresponse = .1, group1 = "Standard care", group2 = "New treatment"),
  two_proportions_noninferiority = two_proportions(.2, .2, objective = "noninferiority", margin = .05,
    alpha = .025, nonresponse = .1, group1 = "Standard care", group2 = "New treatment"),
  two_proportions_equivalence = two_proportions(.2, .21, objective = "equivalence", lower = -.05, upper = .04,
    alpha = .05, ratio = 2, nonresponse = .1, group1 = "Standard care", group2 = "New treatment"),
  yamane = yamane(1000, precision = .05, nonresponse = .1)
)
formats <- strsplit(Sys.getenv("CALCULATOR_EXAMPLE_FORMATS", "markdown,html,pdf,docx"), ",", fixed = TRUE)[[1]]
if (any(!formats %in% c("markdown", "html", "pdf", "docx"))) stop("Unsupported example format.")
for (name in names(results)) {
  print(results[[name]])
  for (format in formats) {
    extension <- if (format == "markdown") "md" else format
    calculation_export(results[[name]], file.path("output", paste0(name, "_example.", extension)), format)
  }
}
