# Takes a vector of full 5-character HCPCS codes and
# outputs a list grouping the most similar together

x <- hcpcs_tests

hcpcs_tests <- vctrs::vec_c(
  c(0:9, "J*", LETTERS),         # "^[0-9A-Z]{5}$"
  c(as.character(90:98)),        # "^[9][0-8][0-9]{3}$"
  c(as.character(921:929)),      # "^[9][2][1-9][0-9]{2}$"
  c(as.character(9202:9208)),    # "^[9][2][0][2-8][0-9]{1}$"
  c(as.character(92015:92019)),  # "^[9][2][0][1][5-9]$"
  long_test)

long_test2 <- vctrs::vec_c(
  stringfish::sf_substr(long_test, 1, 1),
  stringfish::sf_substr(long_test, 1, 2),
  stringfish::sf_substr(long_test, 1, 3),
  stringfish::sf_substr(long_test, 1, 4),
  stringfish::sf_substr(long_test, 1, 5)
  )



# vctrs::vec_chop(long_test, nchar(long_test))

split_lengths <- function(x) {
  x <- gsub("\\*", "", x)
  x <- gsub(" ", "", x)
  x <- fuimus::uniq_rmna(x)
  x <- collapse::rsplit(x, collapse::vlengths(x))
  names(x) <- paste0("x", names(x))
  x
}

x <- split_lengths(long_test2)

xsp <- split_lengths(hcpcs_tests)

x$x1

group_hcpcs_5 <- function(x) {

  base <- dplyr::tibble(
    code = x$x5,
    a1 = substr(code, 1, 1),
    a2 = substr(code, 2, 2),
    a3 = substr(code, 3, 3),
    a4 = substr(code, 4, 4),
    a5 = substr(code, 5, 5))

  indices <- base |>
    dplyr::mutate(i1 = dplyr::consecutive_id(a1), .after = code) |>
    dplyr::mutate(i2 = dplyr::consecutive_id(a2), .after = i1, .by = a1) |>
    dplyr::mutate(i3 = dplyr::consecutive_id(a3), .after = i2, .by = c(a1, a2)) |>
    dplyr::mutate(i4 = dplyr::consecutive_id(a4), .after = i3, .by = c(a1, a2, a3)) |>
    dplyr::mutate(i5 = dplyr::consecutive_id(a5), .after = i4, .by = c(a1, a2, a3, a4)) |>
    dplyr::select(code, a1:a5, i1:i5)

  singles <- indices |>
    dplyr::add_count(i1, name = "n1", sort = TRUE) |>
    dplyr::filter((i2 + i3 + i4 + i5 + n1) == 5) |>
    dplyr::pull(code)

  last <- indices |>
    dplyr::filter(!code %in% singles) |>
    dplyr::select(code, a1:a3, a5) |>
    fuimus::combine(group_id, columns = c("a1", "a2", "a3"), sep = "")

  last <- dplyr::left_join(
    last|> dplyr::count(group_id, a5),
    last|> dplyr::count(group_id, name = "g"),
    by = dplyr::join_by(group_id)) |>
    dplyr::filter(n == g) |>
    dplyr::right_join(last,
      by = dplyr::join_by(group_id, a5)) |>
    dplyr::filter(!is.na(n))

  rest <- indices |>
    dplyr::filter(!code %in% c(singles, dplyr::pull(last, code))) |>
    fuimus::combine(group_id, columns = c("a1", "a2", "a3", "a4"), sep = "")

  groups <- vctrs::vec_c(
    as.list(singles),
    vctrs::vec_chop(last$code, sizes = vctrs::vec_run_sizes(last$group_id)),
    vctrs::vec_chop(rest$code, sizes = vctrs::vec_run_sizes(rest$group_id))
  )

  groups[which(collapse::vlengths(groups) == 1)]
}

split_lengths(long_test2) |>
  group_hcpcs_5()
