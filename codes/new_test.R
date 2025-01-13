library(obissdm)
library(sp)
library(tidyverse)
library(terra)
library(raster)
library(stars)

#' Convert GeoTIFF files to Cloud Optimized GeoTIFF format (COG)
#'
#' @param id path to the GeoTIFF file to be optimized
#' @param remove_files if `TRUE` (default), the original file is removed if the
#'   optimization succeeds or the COG is removed if the optimization failed
#'   somehow
#' @param verbose if `TRUE` output messages
#'
#' @return optimization status and saved files
#' @export
#'
#' @details
#' There are three possible status returned by the function:
#' - optimization-succeeded: optimization was successful and file is saved. 
#' The non-optimized file is removed if `remove_files = TRUE`.
#' - optimization-failed: optimization failed.
#' - optimization-gen-failed: file was probably generated, but by some reason 
#' it was not correctly optimized. This was included more for control, as such a
#'  case should be extremely rare.
#'  
#' This function depends on the [rio-cogeo plugin](https://cogeotiff.github.io/rio-cogeo/), which
#' is a Python program with CLI interface. To install the plugin, you should use:
#' 
#' ``` 
#' $ pip install rio-cogeo
#' ```
#' 
#' @examples
#' \dontrun{
#' cogeo_optim("test_file.tif")
#' }
cogeo_optim <- function(id, remove_files = TRUE, verbose = FALSE) {
  
  if (verbose) cli::cli_alert_info("Optimizing {.path {id}} to COG using {.var rio}")
  outid <- gsub("\\.tif", "_cog.tif", id)
  
  out <- sys::exec_internal("rio",
                            c("cogeo", "create", id, outid),
                            error = F)
  if (out$status == 1) {
    if (verbose) cli::cli_alert_danger("Optimization failed")
    to_return <- data.frame(file = id, status = "optimization-failed")
  } else {
    # Validate
    out <- sys::exec_internal("rio",
                              c("cogeo", "validate", outid),
                              error = F)
    if (out$status == 1) {
      to_return <- data.frame(file = id, status = "optimization-failed")
      if (file.exists(outid) & remove_files) file.remove(outid)
      if (verbose) cli::cli_alert_danger("Optimization failed")
    } else {
      out <- sys::as_text(out$stdout)
      if (grepl("is a valid cloud optimized GeoTIFF", out)) {
        to_return <- data.frame(file = id, status = "optimization-succeeded")
        if (verbose) cli::cli_alert_success("Optimization succeeded. Output file: {.path {outid}}")
        if (remove_files) file.remove(id)
      } else {
        to_return <- data.frame(file = id, status = "optimization-gen-failed")
        if (file.exists(outid) & remove_files) file.remove(outid)
        if (verbose) cli::cli_alert_danger("Optimization is invalid")
      }
    }
  }
  return(to_return)
}


source("functions/model_species.R")
species <- 278400
group <- "anemonefish"

model_species(
  species = species,
  group = group,
  species_dataset = species_dataset,
  outfolder = outfolder,
  outacro = outacro,
  algorithms = algos,
  algo_opts = algo_opts,
  limit_by_depth = limit_by_depth,
  depth_buffer = depth_buffer,
  assess_bias = assess_bias,
  post_eval = c("sst", "niche", "hyper"),
  tg_metric = tg_metric,
  tg_threshold = tg_threshold,
  quad_samp = quad_samp,
  verbose = TRUE
)