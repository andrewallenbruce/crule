#' Save filter conditions
#'
#' Save a series of filter conditions, and support
#' logical operating among conditions
#'
#' @param x expression
#'
#' @param .env environment
#'
#' @returns cruler object
#'
#' @examples
#' c1 <- cruler(cut == "Fair")
#'
#' c2 <- cruler(x > 8)
#'
#' !c1
#'
#' c1 | c2
#'
#' c1 & c2
#'
#' @autoglobal
#'
#' @export
cruler <- function(x = expression(), .env = NULL) {

  x <- enquo(x)

  if (not_null(.env)) x <- quo_set_env(x, .env)

  structure(x, class = c("cruler", "quosure", "formula"))
}

#' @export
`&.cruler` <- function(exp1, exp2) {

  env1 <- quo_get_env(exp1)

  env2 <- quo_get_env(exp2)

  exp1 <- quo_get_expr(exp1)

  exp2 <- quo_get_expr(exp2)

  envr <- env_clone(env1)

  env_coalesce(envr, env2)

  cruler(!!exp1 & !!exp2, .env = envr)
}

#' @export
`|.cruler` <- function(exp1, exp2) {

  env1 <- quo_get_env(exp1)

  env2 <- quo_get_env(exp2)

  exp1 <- quo_get_expr(exp1)

  exp2 <- quo_get_expr(exp2)

  envr <- env_clone(env1)

  env_coalesce(envr, env2)

  cruler(!!exp1 | !!exp2, .env = envr)
}

#' @export
`!.cruler` <- function(x) {

  env <- quo_get_env(x)

  x <- quo_get_expr(x)

  cruler(!(!!x), .env = env)
}

#' Is expression atomic?
#'
#' @param x expression
#'
#' @returns logical
#'
#' @examples
#' is_atomic_expr(rlang::expr(x))
#'
#' is_atomic_expr(rlang::expr(!x))
#'
#' is_atomic_expr(rlang::expr(x + y))
#'
#' is_atomic_expr(rlang::expr(x > 1))
#'
#' is_atomic_expr(rlang::expr(!x + y))
#'
#' is_atomic_expr(rlang::expr(x > 1 | y < 2))
#'
#' @autoglobal
#'
#' @export
is_atomic_expr <- function(x) {

  if (!is_expression(x)) {

    stop("not an expression object")

  }

  if (length(x) > 1) {

    if (
      all(
        purrr::map_lgl(
          as.list(x), ~ length(.x) == 1
          )
        )
      ) {

      return(TRUE)

    } else {

      return(FALSE)

      }
  } else if (length(x) == 1) {

    return(TRUE)

  } else {

    stop("error")
  }
}

#' Gather atomic expressions
#'
#' @param x expression
#'
#' @returns `<chr>` vector of expressions
#'
#' @examples
#' ex <- rlang::expr(a == 2 & b == 3 | !b & x + 2)
#'
#' gather_expr(ex)
#'
#' @autoglobal
#'
#' @export
gather_expr <- function(x) {

  if (!is_expression(x)) stop("not an expression object")

  if (is_atomic_expr(x)) return(deparse(ex))

  res <- c()

  for (i in as.list(x)) {

    if (is_atomic_expr(i)) {

      res <- c(res, deparse(i))

    } else {

      res <- c(res, gather_expr(i))

    }
  }
  return(res)
}

#' Evaluate cruler object
#'
#' @param .data tibble
#'
#' @param cruler cruler object
#'
#' @param .by group by, same as `.by` argument in `dplyr::filter`
#'
#' @param usecol if `TRUE` (default), use the default behavior of
#'    `dplyr::filter()`, which allows the usage of same variable
#'    in colnames, and filter by the data column. If `FALSE`, will
#'    check whether the variables on the right side of `==,>,<,>=,<=`
#'    have the same names as columns and raise error. Ignore this
#'    argument when using `.env` or `!!`
#'
#' @returns tibble
#'
#' @examplesIf FALSE
#' c1 <- cruler(hcpcs == "A9000")
#'
#' c2 <- cruler(x > 8)
#'
#' ex |> cruleval(c1)
#'
#' ex |> cruleval(c1 & c2)
#'
#' x <- 8
#' cnd <- cruler(y > x)
#'
#' cruleval(ex, cnd)
#'
#' try(cruleval(ex, cnd, usecol = FALSE))
#'
#' cnd <- cruler(y > !!x)
#'
#' cruleval(ex, cnd)
#'
#' cnd <- cruler(y > .env$x)
#' cruleval(ex, cnd)
#'
#' @autoglobal
#'
#' @export
cruleval <- function(.data, cruler = NULL, .by = NULL, usecol = TRUE) {

  if (null(cruler)) return(.data)

    right_in_expr <- gather_expr(
      quo_get_expr(cruler)) |>
      stringr::str_split("==|>|<|>=|<=") |>
      purrr::map_chr(
        ~ ifelse(
        length(.x) == 2,
        stringr::str_trim(.x[2]),
        NA
        )
      )

    samecol <- intersect(right_in_expr, colnames(.data))

    if (length(samecol) > 0 && false(usecol)) {

      stop(
        stringr::str_glue(
          "same columns in cruler expression: {samecol}"
          )
        )
    }

    dplyr::filter(.data, eval(cruler), .by = .by)
}
