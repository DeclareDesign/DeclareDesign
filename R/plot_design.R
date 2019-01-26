#' Plot a flowchart of a design
#'
#' This method plots objects generated in DeclareDesign.
#'
#' @param x An object of class `design`, typically created by
#' @param ... Plotting and visualization arguments; currently supported `main`,
#' `theme`.
#' @importFrom grid grid.newpage viewport unit pushViewport
#' @importFrom grid grid.text gpar popViewport
#' @importFrom rlang node_car get_expr
#' @export
plot.design = function(x, ...) {
  # Fill out object name for plot
  object_argument_name <- substitute(x)
  if (is.null(object_argument_name)) {
    object_argument_name <- "design object"
  }
  
  # Make sure we didn't end up in object hell.
  if (!"design" %in% class(x)) {
    stop("This method must be passed an object of class `design`.")
  }
  
  # Arguments
  arguments <- list(...)
  main_text <- ifelse(
    is.null(arguments[["main"]]),
    paste0("Design Flowchart"),
    arguments[["main"]]
  )
  theme_default <- list("dgp" = "deepskyblue1",
                        "estimand" = "darkgoldenrod1",
                        "estimator" = "darkgoldenrod1")
  if (!is.null(arguments[["theme"]])) {
    theme <- arguments[["theme"]]
  } else {
    theme <- theme_default
  }
  
  # Layout details
  node_width <- 0.12
  node_height <- 0.1
  lane_height <- 2 * node_height
  main_title_y <- 0.9
  main_title_scale <- 1.6
  label_offset <- 0.03
  node_offset <- 0.23
  node_interstitial <- 0.05
  sub_offset_x <- 0.5
  
  # Setup grid
  grid.newpage()
  initial_setup <- viewport(
    x = unit(0, "npc"),
    y = unit(0, "npc"),
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    just = c("left", "bottom")
  )
  pushViewport(initial_setup)
  
  # Main
  grid.text(y = main_title_y,
            label = main_text,
            gp = gpar(cex = main_title_scale))
  
  # Lane labels
  lane_map <- list("dgp" = 1,
                   "estimand" = 2,
                   "estimator" = 3)
  grid.text(
    "DGP",
    x = label_offset,
    y = main_title_y - lane_height -
      (lane_height * (lane_map[["dgp"]] - 1)),
    just = "left",
    gp = gpar(fontface = "bold")
  )
  grid.text(
    "Estimands",
    x = label_offset,
    y = main_title_y - lane_height -
      (lane_height * (lane_map[["estimand"]] - 1)),
    just = "left",
    gp = gpar(fontface = "bold")
  )
  grid.text(
    "Estimators",
    x = label_offset,
    y = main_title_y - lane_height -
      (lane_height * (lane_map[["estimator"]] - 1)),
    just = "left",
    gp = gpar(fontface = "bold")
  )
  
  global_index <- 1
  old_node <- NULL
  for (i in seq.int(length(x))) {
    if (!"design_step" %in% class(x[[i]])) {
      print(paste0("Something that is not a design step found, index: ", i))
      next
    }
    
    current_node <- list(
      lane = attr(x[[i]], "causal_type"),
      step = attr(x[[i]], "step_type"),
      label = attr(x[[i]], "label")
    )
    if (current_node[["step"]] == "wrapped" &&
        !is.null(current_node[["label"]])) {
      current_node[["label"]] = tryCatch({
        paste0(as.character(node_car(get_expr(
          attr(x[[i]], "call")
        )))[c(2, 1, 3)],
        collapse = "")
      }, error = function(e) {
        return("Unknown")
      })
    }
    
    global_index_modifier <- ifelse(current_node[["lane"]] == "dgp",
                                    0,
                                    sub_offset_x)
    
    current_node[["y"]] <- main_title_y - lane_height -
      (lane_height * (lane_map[[current_node[["lane"]]]] - 1))
    current_node[["x"]] <-
      node_offset + ((node_width + node_interstitial) *
                       (global_index -
                          global_index_modifier - 1))
    
    flow_box(
      current_node[["x"]],
      current_node[["y"]],
      process_node_label(current_node),
      fill = theme[[current_node[["lane"]]]],
      boxwidth = node_width,
      boxheight = node_height
    )
    
    if (!is.null(old_node)) {
      if (current_node[["lane"]] == "dgp") {
        straight_line(
          old_node[["x"]] + (node_width / 2),
          old_node[["y"]],
          current_node[["x"]] - (node_width / 2),
          current_node[["y"]]
        )
      } else {
        curvy_line(
          old_node[["x"]] + (node_width / 2),
          old_node[["y"]],
          current_node[["x"]] - (node_width / 2),
          current_node[["y"]]
        )
      }
    }
    
    if (current_node[["lane"]] == "dgp") {
      old_node <- current_node
      global_index <- global_index + 1
    }
    
  }
  
  popViewport()
}

process_node_label = function(node) {
  base_name <- map_plot_box_name(node[["step"]])
  ifelse(is.null(node[["label"]]) || nchar(node[["label"]]) == 0,
         base_name,
         paste0(base_name, ":\n", node[["label"]]))
}

#' @importFrom tools toTitleCase
map_plot_box_name = function(name) {
  name_map <- list("sampling" = "Sample",
                   "dgp" = "DGP",
                   "wrapped" = "External")
  
  if (is.null(name)) {
    return(NULL)
  }
  if (name %in% names(name_map)) {
    return(unlist(unname(name_map[name])))
  }
  return(toTitleCase(gsub("_", "\n", name)))
}

trim_white_space_line <- function(text) {
  unname(paste(vapply(strsplit(text, "\n")[[1]], trimws, ""),
               collapse = "\n"))
}

#' @importFrom grid gpar grid.text popViewport grid.roundrect
flow_box <- function(x, y, text, ...) {
  arguments <- list(...)
  
  fill_color <- ifelse(is.null(arguments[["fill"]]),
                       "white",
                       arguments[["fill"]])
  box_width <- ifelse(is.null(arguments[["boxwidth"]]),
                      unit(0.12, "npc"),
                      arguments[["boxwidth"]])
  box_height <- ifelse(is.null(arguments[["boxheight"]]),
                       unit(0.10, "npc"),
                       arguments[["boxheight"]])
  text_left_margin <- ifelse(is.null(arguments[["leftmargin"]]),
                             0.05,
                             arguments[["leftmargin"]])
  text_scale <- ifelse(is.null(arguments[["text.cex"]]),
                       0.8,
                       arguments[["text.cex"]])
  
  text <- trim_white_space_line(text)
  box_vp <-
    viewport(
      x = x,
      y = y,
      width = box_width,
      height = box_height
    )
  pushViewport(box_vp)
  grid.roundrect(gp = gpar(fill = fill_color))
  grid.text(
    text,
    x = text_left_margin,
    gp = gpar(cex = text_scale,
              lineheight = 1),
    just = "left"
  )
  popViewport()
}

#' @importFrom grid grid.lines arrow unit gpar
straight_line = function(x1, y1, x2, y2, ...) {
  arguments <- list(...)
  arrow_size <- ifelse(is.null(arguments[["arrow_size"]]),
                       0.02,
                       arguments[["arrow_size"]])
  line_type <- ifelse(is.null(arguments[["lty"]]),
                      "solid",
                      arguments[["lty"]])
  
  grid.lines(
    x = c(x1, x2),
    y = c(y1, y2),
    arrow = arrow(
      angle = 30,
      type = "closed",
      length = unit(arrow_size, "npc")
    ),
    gp = gpar(lty = line_type)
  )
}

#' @importFrom grid grid.xspline arrow unit gpar
curvy_line <- function(x1, y1, x2, y2, ...) {
  arguments <- list(...)
  arrow_size <- ifelse(is.null(arguments[["arrow_size"]]),
                       0.02,
                       arguments[["arrow_size"]])
  line_type <- ifelse(is.null(arguments[["lty"]]),
                      "dashed",
                      arguments[["lty"]])
  
  x_set <- c(x1,
             x1 + 0.01,
             x1 + 0.01,
             x2 - 0.03,
             x2 - 0.03,
             x2)
  y_set <- c(y1,
             y1,
             y2 + ((y1 - y2) * 0.5),
             y2 + ((y1 - y2) * 0.5),
             y2,
             y2)
  
  grid.xspline(
    x = x_set,
    y = y_set,
    shape = 0.5,
    arrow = arrow(
      angle = 30,
      type = "closed",
      length = unit(arrow_size, "npc")
    ),
    gp = gpar(lty = line_type)
  )
}
