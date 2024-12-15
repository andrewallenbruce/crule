#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
mount_board <- \(source = c("local", "remote")) {

  switch(
    source,
    local = pins::board_folder(
      fs::path_package("extdata/pins",
                       package = "crule")),
    remote = pins::board_url(paste0(
        "https://raw.githubusercontent.com/",
        "andrewallenbruce/crule/master/inst/extdata/pins/")),
    stop("Invalid source"))
}

#' Get a pinned dataset from a [pins][pins::pins-package] board
#'
#' @param pin `<chr>` string name of pinned dataset
#'
#' @param ... arguments passed to `mount_board()`
#'
#' @returns `<tibble>` or `<data.frame>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
get_pin <- \(pin, ...) {

  board <- mount_board(...)

  pin <- match.arg(pin, list_pins())

  pins::pin_read(board, pin)
}

#' List pins from a [pins][pins::pins-package] board
#'
#' @param ... arguments to pass to [mount_board()]
#'
#' @returns `<list>` of [pins][pins::pins-package]
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
list_pins <- function(...) {

  board <- mount_board(...)

  pins::pin_list(board)
}
