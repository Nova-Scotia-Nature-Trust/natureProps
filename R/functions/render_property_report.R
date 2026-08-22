#' Render a per-property Word report from a Quarto template
#'
#' @param record List with elements `info`, `pids`, `contacts`, `comms`,
#'   `contact_comms`, `actions` (same shape as `selected_record()` in
#'   `module_review_projects.R`).
#' @param property_name Character. Name of the property, used as the
#'   report title.
#' @param output_file Character. Destination path for the rendered `.docx`.
render_property_report <- function(record, property_name, output_file) {
  record$property_name <- property_name

  # Render in an isolated temp dir so concurrent users/renders don't collide
  work_dir <- tempfile("property_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  qmd_copy <- file.path(work_dir, "property_report.qmd")
  file.copy("reports/property_report.qmd", qmd_copy)

  data_path <- file.path(work_dir, "data.rds")
  saveRDS(record, data_path)

  quarto::quarto_render(
    input = qmd_copy,
    output_file = "property_report.docx",
    execute_params = list(data_path = data_path),
    quiet = TRUE
  )

  rendered_path <- file.path(work_dir, "property_report.docx")
  file.copy(rendered_path, output_file, overwrite = TRUE)

  invisible(output_file)
}
