#' Return GitHub raw url
#'
#' @param x `<chr>` string
#'
#' @returns `<chr>` GitHub raw url
#'
#' @examples
#' gh_raw("andrewbruce/example/main/inst/pins/")
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
gh_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @param package `<chr>` package name
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @importFrom pins board_folder board_url
#' @importFrom fs path_package
#' @importFrom glue glue
#'
#' @keywords internal
#'
#' @export
mount_board <- function(source = c("local", "remote"), package = "<package_name>") {

  source <- match.arg(source)

  switch(
    source,
    local = board_folder(
      path_package("extdata/pins", package = package)
    ),
    remote = board_url(
      gh_raw(
        glue("andrewallenbruce/{package}/master/inst/extdata/pins/")
      )
    ),
    stop("Invalid source")
  )
}

#' Get a pinned dataset from a [pins][pins::pins-package] board
#'
#' @param pin `<chr>` string name of pinned dataset
#'
#' @param ... additional arguments passed to `mount_board()`
#'
#' @returns `<tibble>` or `<data.frame>`
#'
#' @autoglobal
#'
#' @importFrom pins pin_read
#'
#' @keywords internal
#'
#' @export
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- match.arg(pin, list_pins())

  pin_read(board, pin)
}

#' List pins from a [pins][pins::pins-package] board
#'
#' @param ... arguments to pass to [mount_board()]
#'
#' @returns `<list>` of [pins][pins::pins-package]
#'
#' @autoglobal
#'
#' @importFrom pins pin_list
#'
#' @keywords internal
#'
#' @export
list_pins <- function(...) {

  board <- mount_board(...)

  pin_list(board)
}
