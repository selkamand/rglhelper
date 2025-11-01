#' Add a plane and optional normal arrows to an rgl scene
#'
#' Draws a plane defined by a normal vector and offset using `rgl::planes3d()`,
#' with optional visualisation of the plane's normal via `rgl::arrow3d()`.
#'
#' @param normal Numeric length-3 vector giving the plane's normal direction.
#' @param offset Numeric scalar giving the plane's offset (distance from the origin
#'   along the normal).
#' @param lit Logical; if `TRUE`, the plane and arrows are affected by scene lighting.
#' @param alpha Numeric in `[0, 1]` giving plane transparency.
#' @param show_normal Logical; if `TRUE`, draws one or more arrows indicating
#'   the plane normal direction.
#' @param color_plane colour of plane.
#' @param arrow_color colour of normal arrow
#' @param arrow_type value of rgl::arrow3d type argument.
#' @param arrow_theta value of rgl::arrow3d theta argument.
#' @param arrow_alpha arrow alpha.
#' @param arrow_lit should arrow be affected by lighting.
#' @param add_invisible_point add an invisible point in the plane to guarantee it gets rendered (rgl calculates viewbox based on finite elements only, ignoring infinite elements like planes causing them to not get rendered)
#' @inheritDotParams rgl::planes3d
#'
#' @return Invisibly returns a length-3 list with the rgl shape Ids for the plane and arrow objects (named plane, arrow, and point respectively)
#' @seealso [add_plane_by_position_and_normal()]
#' @export
add_plane <- function(normal, offset=0, color_plane="pink", lit=FALSE, alpha = 0.5, show_normal = TRUE, arrow_color = "pink", arrow_alpha = 1, arrow_lit = FALSE, add_invisible_point = TRUE, ...){
  stopifnot(length(normal) == 3)
  nlen <- move::magnitude(normal)
  if (nlen == 0) stop("normal must be non-zero")
  normal <- move::normalise(normal)

  # new_arrow_pos = slide_segment(p0 = c(0, 0, 0), p1 = normal, d = -offset)

  # Define positions for arrows / invisible point
  p0 = move::translate_position_in_direction(c(0, 0, 0), direction = normal, magnitude = -offset)
  p1 = move::translate_position_in_direction(normal, normal, magnitude = -offset)

  if (show_normal) {
    # rgl::arrow3d(p0 = c(0, 0, 0), p1 = c(0, 1, 0), type = "rotation", width = 0.2, n=8, smooth = TRUE, lit=FALSE, color="red", s = 0.2)
    arrow_id <- rgl::arrow3d(
      p0 = p0,
      p1 = p1,
      color = arrow_color, alpha = arrow_alpha, lit = arrow_lit, type = "rotation", width = 0.2, n=8, s=0.2
    )
  }
  else arrow_id <- NULL

  # Draw invisible point to guarantee plane is rendered
  if (add_invisible_point) {
    points_id <- rgl::points3d(x = p0[1], y = p0[2], z = p0[3], lit = FALSE, alpha = 0)
  } else points_id <- NULL

  # Draw plane
  plane_id <- rgl::planes3d(normal, d = offset, lit = lit, color = color_plane, alpha = alpha, ...)


  return(invisible(list(plane = plane_id, arrow = arrow_id, point = points_id)))
}


#' Add a plane defined by position and normal
#'
#' Computes the plane offset from a point and normal, then draws it with
#' [add_plane()]. Accepts the same visual arguments as add_plane().
#'
#' @param normal Numeric length-3 vector giving the plane normal.
#' @param position Numeric length-3 vector specifying a point on the plane.
#' @inheritParams add_plane
#' @inheritDotParams add_plane
#' @return Invisibly returns a length-2 list with the rgl shape Ids for the plane and arrow objects (named plane and arrow respectively)
#' @seealso [add_plane()]
add_plane_by_position_and_normal <- function(normal,
                                             position,
                                             color_plane = "orange",
                                             lit = FALSE,
                                             alpha = 0.5,
                                             show_normal = TRUE,
                                             arrow_color = color_plane,
                                             arrow_type = "rotation",
                                             arrow_theta = pi/6,
                                             arrow_alpha = 1,
                                             arrow_lit = lit,
                                             add_invisible_point = TRUE,
                                             ...){
  # validate inputs
  stopifnot(length(normal) == 3)
  stopifnot(length(position) == 3)
  if (any(is.na(position))) stop("plane_position must not contain NA")
  if (move::magnitude(normal) == 0) stop("plane_normal must be non-zero")

  # ensure normal is unit (your helper already returns that pair)
  plane_normal <- move::normalise(normal)

  plane <- move::convert_plane_point_normal_to_normal_offset(
    normal = normal,
    point = position
  )

  offset <- plane$offset
  normal <- plane$normal

  # forward all visual args to add_plane; keep parameter names consistent
  add_plane(
    normal = normal,
    offset = offset,
    color_plane = color_plane,
    lit = lit,
    alpha = alpha,
    show_normal = show_normal,
    arrow_color = arrow_color,
    arrow_type = arrow_type,
    arrow_theta = arrow_theta,
    arrow_alpha = arrow_alpha,
    arrow_lit = arrow_lit,
    add_invisible_point = add_invisible_point,
    ...
  )
}
