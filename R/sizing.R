#' Determine ggplot size from panel size
#'
#' This function calculates the total size of a ggplot based on the desired
#' panel size.
#'
#' @param plot A ggplot object or ggplotGrob
#' @param panel_width Desired panel width in mm
#' @param panel_height Desired panel height in mm
#'
#' @return Returns a list of computed dimensions
#'
#' @export
panel2plotsize <- function(
  plot,
  panel_width,
  panel_height
) {
  # Convert to gtable if needed
  if (!inherits(plot, "grob")) {
    gt <- ggplot2::ggplotGrob(plot)
  } else {
    gt <- plot
  }

  # For the case of a single panel
  panel_index <- which(gt$layout$name == "panel")

  if (length(panel_index) == 1) {
    # For one panel
    panel_dimensions <- get_panel_size(gt, panel_index)
    total_panel_width <- panel_dimensions$panel_width
    total_panel_height <- panel_dimensions$panel_height
  } else {
    # For multiple panels
    panel_cols <- which(grepl("^panel-1", gt$layout$name))
    panel_rows <- which(grepl("^panel-[1-9]-1", gt$layout$name))
    total_panel_width <- sum(sapply(
      panel_cols,
      \(i) get_panel_size(gt, i)$panel_width
    ))
    total_panel_height <- sum(sapply(
      panel_rows,
      \(i) get_panel_size(gt, i)$panel_height
    ))
  }

  # Get full plot size in mm
  total_width <- sum(grid::convertWidth(gt$widths, "mm", valueOnly = TRUE))
  total_height <- sum(grid::convertHeight(gt$heights, "mm", valueOnly = TRUE))

  # Compute overhead
  width_overhead <- total_width - total_panel_width
  height_overhead <- total_height - total_panel_height

  # Compute required full size for requested panel size
  target_width <- panel_width * 25.4 + width_overhead
  target_height <- panel_height * 25.4 + height_overhead

  list(
    panel_width = panel_width,
    panel_height = panel_height,
    total_width = target_width / 25.4,
    total_height = target_height / 25.4
  )
}

# Get panel size in mm
get_panel_size <- function(gt, panel_index) {
  panel_col <- gt$layout[panel_index, ]$l
  panel_row <- gt$layout[panel_index, ]$t

  panel_width <- grid::convertWidth(
    gt$widths[panel_col],
    "mm",
    valueOnly = TRUE
  )

  panel_height <- grid::convertHeight(
    gt$heights[panel_row],
    "mm",
    valueOnly = TRUE
  )

  list(
    panel_width = panel_width,
    panel_height = panel_height
  )
}

#' @export
apply_size_attributes <- function(
  plot,
  panel_width,
  panel_height
) {
  sizing <- panel2plotsize(
    plot = plot,
    panel_width = panel_width,
    panel_height = panel_height
  )

  attr(plot, "ggdim.panel_width") <- sizing$panel_width
  attr(plot, "ggdim.panel_height") <- sizing$panel_height
  attr(plot, "ggdim.total_width") <- sizing$total_width
  attr(plot, "ggdim.total_height") <- sizing$total_height

  plot
}

#' @export
get_size_attributes <- function(plot) {
  list(
    panel_width = attr(plot, "ggdim.panel_width"),
    panel_height = attr(plot, "ggdim.panel_height"),
    total_width = attr(plot, "ggdim.total_width"),
    total_height = attr(plot, "ggdim.total_height")
  )
}

#' @export
copy_attributes <- function(a, b) {
  attr(a, "ggdim.panel_width") <- attr(b, "ggdim.panel_width")
  attr(a, "ggdim.panel_height") <- attr(b, "ggdim.panel_height")
  attr(a, "ggdim.total_width") <- attr(b, "ggdim.total_width")
  attr(a, "ggdim.total_height") <- attr(b, "ggdim.total_height")
  a
}
