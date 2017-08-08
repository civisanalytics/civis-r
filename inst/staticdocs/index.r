sd_section(
  "Data Import and Export",
  "Moving data to and from databases in the Civis Platform",
  c("read_civis", "write_civis", "export_csv", "csv_to_civis", "query_civis")
)

sd_section(
  "Files and R Objects",
  "Saving files and R objects in the Civis Platform",
  c("save_civis", "load_civis", "file_to_civis", "file_from_civis")
)

sd_section(
  "Modeling",
  "Building and scoring models in the Civis Platform",
  c("model_civis", "download_ensemble", "predict.civis_model",
    "predict.civis_model_failed", "predict.civis_model_ensemble",
    "summary.civis_model", "summary.civis_model_failed",
    "summary.civis_model_ensemble", "print.civis_model",
    "print.civis_model_failed", "print.civis_model_ensemble")
)

sd_section(
  "Reports",
  "Creating sharable Civis reports from R Markdown and custom HTML",
  c("publish_rmd", "publish_html")
)
