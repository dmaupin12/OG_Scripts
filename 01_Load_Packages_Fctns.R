library(data.table)
library(purrr)
library(tidyverse)
library(sf)
library(nngeo)
library(geosphere)
library(ggplot2)
library(lwgeom)
library(furrr)
library(rmapshaper)
library(openxlsx)
library(sp)
library(stringr)
library(stplanr)
library(tigris)
library(gt)
library(gtExtras)
library(nngeo)
library(buffeRs)
library(scales)
library(tidyr)
library(officer)
library(patchwork)
library(magick)
library(flextable)
library(png)
library(forcats)
library(tidytext)
library(ggpattern)
library(cowplot)

sf_use_s2(FALSE)



sfc_as_cols <- function(x, geometry, names = c("x", "y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x, "sf") &&
              inherits(geometry, "sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[, !names(x) %in% names]
  ret <- setNames(ret, names)
  dplyr::bind_cols(x, ret)
}
