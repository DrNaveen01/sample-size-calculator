# Convert the same report used by the app to HTML, PDF, or DOCX.

# Original three-file single-proportion scripts also remain supported.
export_report_content <- function(x) {
  if (inherits(x, "single_proportion_result")) return(single_proportion_markdown(x))
  calculation_markdown(x)
}

export_available <- function(format) {
  if (format == "markdown") return(TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available()) return(FALSE)
  if (format == "pdf") return(nzchar(Sys.which("pdflatex")))
  TRUE
}

calculation_export <- function(x, file, format = c("markdown", "pdf", "docx", "html"),
                               reference_doc = file.path("templates", "reference.docx")) {
  format <- match.arg(format)
  key <- if (inherits(x, "single_proportion_result")) "single_proportion" else calculation_key(x)
  if (!export_available(format)) {
    stop(if (format == "pdf") {
      "PDF export needs Pandoc and a LaTeX installation. Run source('scripts/setup.R') with PDF setup enabled."
    } else {
      "This export needs rmarkdown and Pandoc. Install rmarkdown and use RStudio, or install Pandoc."
    }, call. = FALSE)
  }
  if (format == "markdown") {
    writeBin(charToRaw(enc2utf8(export_report_content(x))), file)
    return(invisible(file))
  }
  work <- tempfile("calculation-report-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  input <- file.path(work, "report.md")
  target <- file.path(work, paste0("report.", format))
  writeLines(export_report_content(x), input, useBytes = TRUE)
  options <- "--standalone"
  if (format == "pdf") {
    options <- c(options, "--pdf-engine=pdflatex", "-V", "geometry:margin=0.8in",
                 "-V", "papersize:letter", "-V", "fontsize=11pt", "-V", "colorlinks=true",
                 "-V", "linkcolor=black", "-V", "urlcolor=black")
    template_directory <- dirname(reference_doc)
    pdf_styles <- file.path(template_directory, "pdf_styles.lua")
    pdf_header <- file.path(template_directory, "pdf_header.tex")
    if (file.exists(pdf_styles) && file.exists(pdf_header)) {
      options <- c(options, paste0("--lua-filter=", normalizePath(pdf_styles)),
                   paste0("--include-in-header=", normalizePath(pdf_header)))
    }
  }
  if (format == "docx") {
    if (!file.exists(reference_doc)) stop("The Word reference document is missing.", call. = FALSE)
    styles_filter <- file.path(dirname(reference_doc), "word_styles.lua")
    options <- c(options, paste0("--reference-doc=", normalizePath(reference_doc)))
    if (file.exists(styles_filter)) options <- c(options, paste0("--lua-filter=", normalizePath(styles_filter)))
  }
  if (format == "html") options <- c(options, "--mathml", paste0("--metadata=pagetitle:", gsub("_", " ", key)))
  rmarkdown::pandoc_convert(input, from = "markdown+tex_math_dollars", to = if (format == "pdf") "latex" else format,
                           output = target, options = options, verbose = FALSE)
  if (!file.exists(target) || file.info(target)$size < 1) stop("The report could not be created.", call. = FALSE)
  if (!file.copy(target, file, overwrite = TRUE)) stop("The report could not be saved.", call. = FALSE)
  invisible(file)
}

# Retain the original public export function for existing scripts.
single_proportion_export <- function(x, file, format = c("markdown", "pdf", "docx", "html"),
                                     reference_doc = file.path("templates", "reference.docx")) {
  stopifnot(inherits(x, "single_proportion_result"))
  calculation_export(x, file, match.arg(format), reference_doc)
}

# MathML is generated locally by Pandoc. No CDN or external font is needed.
report_html_fragment <- function(x) {
  work <- tempfile("report-preview-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  input <- file.path(work, "report.md")
  output <- file.path(work, "report.html")
  writeLines(export_report_content(x), input, useBytes = TRUE)
  rmarkdown::pandoc_convert(input, from = "markdown+tex_math_dollars", to = "html",
                           output = output, options = "--mathml", verbose = FALSE)
  paste(readLines(output, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
