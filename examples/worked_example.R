# Run from the project root.
for (path in c("single_proportion.R", "planning.R", "report.R", "reports_extended.R", "reports_objectives.R", "exports.R")) source(file.path("R", path))
dir.create("output", showWarnings = FALSE)
result <- single_proportion(p = 0.50, precision = 0.05, confidence = 0.95, nonresponse = 0.10)
print(result)
# qnorm(0.975) is used internally, not a pre-rounded 1.96.
# n0 = 384.145882...; n_adjusted = 426.828757...; n_final = 427.
for (format in c("markdown", "html", "pdf", "docx")) {
  extension <- if (format == "markdown") "md" else format
  single_proportion_export(result, file.path("output", paste0("single_proportion_example.", extension)), format)
}

# Equivalent relative-precision calculation: 10% of p=0.50 gives d=0.05.
relative <- single_proportion(p = 0.50, precision = 0.10,
                             precision_type = "relative", confidence = 0.95, nonresponse = 0.10)
stopifnot(isTRUE(all.equal(result$n_final, relative$n_final)))

# Explicit Z value, for comparison with textbook calculations.
textbook <- single_proportion(p = 0.50, precision = 0.05, z = 1.96, nonresponse = 0.10)
stopifnot(textbook$n_final == 427)
