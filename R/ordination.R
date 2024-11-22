#' Principal coordinates analysis, plus
#'
#' @param data A data frame giving information on the objects for the PCoA.
#' @param distmat A matrix or \code{dist} object giving the distances between
#'   objects in \code{data}. The distance matrix can contain distances between
#'   additional objects not found in \code{data}, but if any of the objects in
#'   \code{data} is missing, an error will occur.
#' @param sample_id_var The column in \code{data} that gives the identifiers
#'   for each row, used to find the matching distances in \code{distmat}.
#' @param num_axes The number of PCoA axes to return in the output.
#' @param x A tibble returned by \code{pcoaplus}.
#' @param ... Additional aesthetic mappings for ggplot.
#' @return For \code{pcoaplus}, a tibble with new columns giving the position
#'   of each object along the principal coordinate axes. We add an additional
#'   class to the resultant tibble (\code{"pcoaplus"}) to facilitate plotting.
#'   The attribute "pctvar" gives the percent variation explained by each
#'   principal coordinate axis. The attribute "axislabel" contains formatted
#'   axis labels for plotting.
#'
#'   For \code{plot.pcoaplus}, a ggplot object.
#' @export
pcoaplus <- function(data, distmat, sample_id_var = SampleID, num_axes = 2) {
  num_axes <- as.integer(num_axes)
  stopifnot("num_axes must be 2 or more" = num_axes >= 2)
  axis_numbers <- 1:num_axes
  axis_names <- paste0("Axis.", axis_numbers)

  sample_ids <- data %>%
    dplyr::pull({{ sample_id_var }}) %>%
    as.character()
  stopifnot("Duplicated sample IDs" = anyDuplicated(sample_ids) == 0)

  distmat <- usedist::dist_subset(distmat, sample_ids)
  sample_id_var_name <- rlang::as_name(rlang::ensym(sample_id_var))
  pcoa_obj <- ape::pcoa(distmat)
  pcoa_df <- pcoa_obj$vectors %>%
    `[`(sample_ids, axis_names) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(sample_id_var_name) %>%
    tibble::as_tibble() %>%
    dplyr::left_join(data, by = sample_id_var_name)

  pctvar <- (pcoa_obj$values$Relative_eig * 100)[axis_numbers]
  names(pctvar) <- axis_names
  attr(pcoa_df, "pctvar") <- pctvar

  axislabel <- stringr::str_glue("PCoA axis {axis_numbers} ({round(pctvar)}%)")
  names(axislabel) <- axis_names
  attr(pcoa_df, "axislabel") <- axislabel

  class(pcoa_df) <- c("pcoaplus", class(pcoa_df))
  pcoa_df
}


#' @describeIn pcoaplus Make a principal coordinates scatter plot
#' @export
plot.pcoaplus <- function(x, ...) {
  x %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = Axis.1, y = Axis.2, ...)) +
    ggplot2::coord_equal() +
    ggplot2::xlab(attr(x, "axislabel")[1]) +
    ggplot2::ylab(attr(x, "axislabel")[2])
}
