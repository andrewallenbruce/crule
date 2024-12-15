# Functions ####
head_tail <- function(x, n = 5, by = NULL) {
  dplyr::bind_rows(
    dplyr::slice_head(x, n = n, by = dplyr::all_of(by)),
    dplyr::slice_tail(x, n = n, by = dplyr::all_of(by)))
}

deparse_substitute <- \(x) deparse(substitute(x))

mask <- \(data, expr) rlang::eval_tidy(rlang::enquo(expr), data)

# mtcars |> mask(mpg * 20)

# REGEX FUNCTIONS ####
known_patterns <- c("[0-9]", "[A-Za-z]", "[[:punct:]]")

split_by_length <- \(x) split(x, nchar(x))

escape_period <- \(x) gsub(".", "\\.", x, fixed = TRUE)

find_common_substrings <- \(s, tolerance = 0.95, missing = "#") {
  s <- splits

  df <- t(
    as.data.frame(
      purrr::map(s, ~strsplit(.x, "")[[1]])
      )
    )

  most_matching <- apply(df, 2, \(x) names(which.max(table(x))))

  prop_matching <- sapply(seq_len(ncol(df)), \(x) sum(df[ , x] == most_matching[x]) / nrow(df))

  exact_matches <- prop_matching >= tolerance

  paste0(ifelse(exact_matches, most_matching, missing), collapse = "")

}

detect_pattern <- function(s, ...) {

  charvec <- strsplit(find_common_substrings(s, ...), NULL)[[1]]

  unknown_symbols <- which(charvec == missing_char)

  best_pat <- unknown_symbols[NA]

  for (symbol in unknown_symbols) {

    s_char <- purrr::map_chr(strsplit(s, NULL), symbol)

    pat <- known_patterns[NA]

    pat <- sapply(known_patterns, function(kp) sum(!is.na(purrr::map_chr(s_char, ~stringr::str_match(.x, kp)))))

    best_pat[symbol] <- known_patterns[which.max(pat)]
  }

  detected_pattern <- escape_period(paste0(ifelse(charvec == missing_char, best_pat, charvec), collapse = ""))

  detected_pattern

}

categorise_regex <- function(strings, tolerance = 0.95) {

  string_list <- split_by_length(strings)

  guess <- purrr::map(string_list, detect_pattern, tolerance = tolerance)

  matches <- purrr::map2(string_list, guess, ~stringr::str_match(.x, .y))

  ## remove non-matches
  matches <- purrr::map(matches, ~.x[!is.na(.x)])

  nonmatches <- purrr::map2(string_list, matches, ~.x[! .x %in% .y])

  result <- purrr::pmap(list(guess, matches, nonmatches),
                        ~list(regex = ..1, matches = ..2, nonmatches = ..3))

  message("   ** CATEGORISATION SUMMARY **")



  message("   ** Detected ",
          length(result),
          " categories and matched\n    ",
          length(unlist(purrr::map(result, "matches"))) ," / ", (length(unlist(purrr::map(result, "nonmatches"))) + length(unlist(purrr::map(result, "matches")))),
          " ( ",
          format(length(unlist(purrr::map(result, "matches"))) / (
            length(unlist(purrr::map(result, "nonmatches"))) + length(unlist(purrr::map(result, "matches")))), digits = 3),
          "% ) strings **\n")

  purrr::walk2(
    result,
    names(result),
    ~{
    n_match <- length(.x$matches)
    n_nonmatch <- length(.x$nonmatch)
    n_results <- n_match + n_nonmatch
    message("  nchar: ", .y,
            "\nexample: ", .x$matches[[1]],
            "\n  regex: ", .x$regex,
            "\n  match: ", n_match, " / ", n_results,
            " ( ", format(100 * n_match / n_results, digits = 3), "% )\n")
  })

  return(invisible(result))

}

#####################

gt_var <- \(df) {
  df |>
    gt() |>
    cols_align(align = "left") |>
    opt_table_font(font = google_font(name = "Roboto Condensed")) |>
    opt_all_caps() |>
    tab_style(
      style = cell_text(
        align = "left",
        weight = "bold",
        size = px(14),
        whitespace = "break-spaces",
        font = google_font(name = "JetBrains Mono")
      ),
      locations = cells_body(columns = c(value))
    ) |>
    tab_style(
      style = cell_text(
        align = 'left',
        weight = "bold",
        font = google_font(name = "Roboto Mono")
      ),
      locations = cells_body(columns = c(condition))
    ) |>
    opt_stylize() |>
    tab_options(table.width = pct(50),
                quarto.disable_processing = TRUE)
}

# str_remove_all(value, regex(r"{\(\d+\)}"))

print_rule <- \(n, x = descriptors, w = 40) {

  x <- stringr::str_replace_all(x[n, 4, drop = TRUE], stringr::fixed(" AND "), " AND \n")

  x <- stringr::str_replace_all(x, stringr::fixed(" OR "), " OR \n")

  cat(x, sep = "\n")

}

print_desc <- \(n) {

  x <- cleaned_definitions[n, 2, drop = TRUE]

  x <- stringr::str_replace_all(x, stringr::fixed(" %AND% "), "\n%AND%\n")

  x <- stringr::str_replace_all(x, stringr::fixed(" %OR% "), "\n%OR%\n")

  cat(x, sep = "\n")

}

print_list <- function(x, pre = "") {

  if (length(x) == 0) {cat("<empty>\n")}

  ns <- names(x)

  if (length(ns) != length(x)) {stop("all elements must be named")}

  x <- lapply(x, as.character)

  cat(sprintf("%s%s : %s", pre, format(ns), x), sep = "\n")

  invisible(x)
}

# pos_expr <- northstar::search_pos() |>
#   reframe(name = toupper(pos_name) |> str_replace_all("PATIENT’S", "PATIENT"),
#           pos = pos_code) |>
#   filter(name != "UNASSIGNED") |>
#   mutate(expr = glue::glue('"{{ col }}" := stringfish::sf_gsub({{ col }}, "<<name>>", "<<pos>>", nthreads = getOption("stringfish.nthreads", 4L))', .sep = ", ", .open = "<<", .close = ">>")) |>
#   pull(expr)
#
# glue::glue('dplyr::mutate(
# {glue::glue_collapse(pos_expr, last = "", sep = ",\n")})')

#
# pos_expr |>
#   cat(sep = "\n")

pos_to_code <- \(df, col) {
  dplyr::mutate(
    df,
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "PHARMACY", "01", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME|TELEHEALTH PROVIDED OTHER THAN IN PT HOME|TELEHEALTH PROVIDED OTHER THAN PT HOME", "02", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "SCHOOL", "03", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "HOMELESS SHELTER", "04", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "INDIAN HEALTH SERVICE FREE-STANDING FACILITY|INDIAN HEALTH SERVICE - FREE-SANDNG", "05", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "INDIAN HEALTH SERVICE PROVIDER-BASED FACILITY|INDIAN HEALTH SERVICE - PROVIDER-BASED", "06", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "TRIBAL 638 FREE-STANDING FACILITY|TRIBAL 638 FREE-SANDNG FACILITY", "07", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "TRIBAL 638 PROVIDER-BASED FACILITY|TRIBAL 638 PROVIDER-BASED", "08", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "PRISON/CORRECTIONAL FACILITY", "09", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "TELEHEALTH PROVIDED IN PATIENT HOME", "10", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "OFFICE", "11", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "HOME", "12", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "ASSISTED LIVING FACILITY", "13", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "GROUP HOME", "14", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "MOBILE UNIT", "15", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "TEMPORARY LODGING", "16", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "WALK-IN RETAIL HEALTH CLINIC", "17", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "PLACE OF EMPLOYMENT/WORKSITE", "18", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "OFF CAMPUS-OUTPATIENT HOSPITAL", "19", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "URGENT CARE FACILITY", "20", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "INPATIENT HOSPITAL", "21", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "ON CAMPUS-OUTPATIENT HOSPITAL", "22", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "EMERGENCY ROOM-HOSPITAL|EMERGENCY ROOM - HOSPITAL", "23", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "AMBULATORY SURGICAL CENTER", "24", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "BIRTHING CENTER", "25", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "MILITARY TREATMENT FACILITY", "26", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "OUTREACH SITE/STREET", "27", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "SKILLED NURSING FACILITY", "31", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "NURSING FACILITY", "32", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "CUSTODIAL CARE FACILITY", "33", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "HOSPICE", "34", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "AMBULANCE-LAND", "41", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "AMBULANCE-AIR OR WATER", "42", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "INDEPENDENT CLINIC", "49", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "FEDERALLY QUALIFIED HEALTH CENTER", "50", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "INPATIENT PSYCHIATRIC FACILITY", "51", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "PSYCHIATRIC FACILITY-PARTIAL HOSPITALIZATION|PSYCHIATRIC FACILITY - PARTIAL HOSPITALIZATION", "52", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "COMMUNITY MENTAL HEALTH CENTER", "53", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "INTERMEDIATE CARE FACILITY/INDIVIDUALS WITH INTELLECTUAL DISABILITIES", "54", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY", "55", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "PSYCHIATRIC RESIDENTIAL TREATMENT CENTER", "56", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "NON-RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY|NON-RESIDENTIAL SUBSTANCE ABUSE FACILITY", "57", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "NON-RESIDENTIAL OPIOID TREATMENT FACILITY", "58", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "MASS IMMUNIZATION CENTER", "60", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "COMPREHENSIVE INPATIENT REHABILITATION FACILITY", "61", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "COMPREHENSIVE OUTPATIENT REHABILITATION FACILITY", "62", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "END-STAGE RENAL DISEASE TREATMENT FACILITY|END STAGE RENAL DISEASE TREATMENT FACILITY", "65", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "PROGRAMS OF ALL-INCLUSIVE CARE FOR THE ELDERLY (PACE) CENTER", "66", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "STATE OR LOCAL PUBLIC HEALTH CLINIC", "71", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "RURAL HEALTH", "72", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "INDEPENDENT LABORATORY", "81", nthreads = getOption("stringfish.nthreads", 4L)),
    "{{ col }}" := stringfish::sf_gsub({{ col }}, "OTHER PLACE OF SERVICE", "99", nthreads = getOption("stringfish.nthreads", 4L))
  )
}


pos_name_to_code <- \(df, col) {
  dplyr::mutate(
    df,
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PHARMACY", "01"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME|TELEHEALTH PROVIDED OTHER THAN IN PT HOME|TELEHEALTH PROVIDED OTHER THAN PT HOME", "02"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "SCHOOL", "03"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "HOMELESS SHELTER", "04"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDIAN HEALTH SERVICE FREE-STANDING FACILITY|INDIAN HEALTH SERVICE - FREE-SANDNG", "05"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDIAN HEALTH SERVICE PROVIDER-BASED FACILITY|INDIAN HEALTH SERVICE - PROVIDER-BASED", "06"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TRIBAL 638 FREE-STANDING FACILITY|TRIBAL 638 FREE-SANDNG FACILITY", "07"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TRIBAL 638 PROVIDER-BASED FACILITY|TRIBAL 638 PROVIDER-BASED", "08"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PRISON/CORRECTIONAL FACILITY", "09"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TELEHEALTH PROVIDED IN PATIENT HOME", "10"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OFFICE", "11"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "HOME", "12"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "ASSISTED LIVING FACILITY", "13"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "GROUP HOME", "14"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "MOBILE UNIT", "15"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TEMPORARY LODGING", "16"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "WALK-IN RETAIL HEALTH CLINIC", "17"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PLACE OF EMPLOYMENT/WORKSITE", "18"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OFF CAMPUS-OUTPATIENT HOSPITAL", "19"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "URGENT CARE FACILITY", "20"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INPATIENT HOSPITAL", "21"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "ON CAMPUS-OUTPATIENT HOSPITAL", "22"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "EMERGENCY ROOM-HOSPITAL|EMERGENCY ROOM - HOSPITAL", "23"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "AMBULATORY SURGICAL CENTER", "24"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "BIRTHING CENTER", "25"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "MILITARY TREATMENT FACILITY", "26"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OUTREACH SITE/STREET", "27"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "SKILLED NURSING FACILITY", "31"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NURSING FACILITY", "32"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "CUSTODIAL CARE FACILITY", "33"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "HOSPICE", "34"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "AMBULANCE-LAND", "41"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "AMBULANCE-AIR OR WATER", "42"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDEPENDENT CLINIC", "49"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "FEDERALLY QUALIFIED HEALTH CENTER", "50"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INPATIENT PSYCHIATRIC FACILITY", "51"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PSYCHIATRIC FACILITY-PARTIAL HOSPITALIZATION|PSYCHIATRIC FACILITY - PARTIAL HOSPITALIZATION", "52"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMMUNITY MENTAL HEALTH CENTER", "53"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INTERMEDIATE CARE FACILITY/INDIVIDUALS WITH INTELLECTUAL DISABILITIES", "54"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY", "55"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PSYCHIATRIC RESIDENTIAL TREATMENT CENTER", "56"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NON-RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY|NON-RESIDENTIAL SUBSTANCE ABUSE FACILITY", "57"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NON-RESIDENTIAL OPIOID TREATMENT FACILITY", "58"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "MASS IMMUNIZATION CENTER", "60"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMPREHENSIVE INPATIENT REHABILITATION FACILITY", "61"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMPREHENSIVE OUTPATIENT REHABILITATION FACILITY", "62"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "END-STAGE RENAL DISEASE TREATMENT FACILITY|END STAGE RENAL DISEASE TREATMENT FACILITY", "65"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PROGRAMS OF ALL-INCLUSIVE CARE FOR THE ELDERLY (PACE) CENTER", "66"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "STATE OR LOCAL PUBLIC HEALTH CLINIC", "71"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "RURAL HEALTH CLINIC", "72"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDEPENDENT LABORATORY", "81"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OTHER PLACE OF SERVICE", "99"))
}


str_vector <- function(x, qchar = c("single", "double")) {

  qchar <- match.arg(qchar, choices = c("single", "double"))
  switch(qchar,
         "single" = return(toString(sprintf("'%s'", x))),
         "double" = return(toString(sprintf("\"%s\"", x)))
  )
}

bracks <- \(x) paste0(r"--{[}--", x, r"--{]}--")

parens <- \(x) paste0(r"--{(}--", x, r"--{)}--")

arrows <- \(x) paste0(r"--{<}--", x, r"--{>}--")

bracks("x")

keywords <- function(x) {

  keys <- c(
    "hcpcs", "unit", "mod_1", "mod_2", "mod_3", "mod_3", "pos",
    "pos", "dos", "age", "sex", "icd", "ndc", "rev", "claim",
    "referring", "rendering", "credential",
    "primary_name", "secondary_name", "tertiary_name",
    "primary_class", "secondary_class", "tertiary_class",
    "primary_state", "secondary_state", "tertiary_state"
  )

  if (grepl(paste0(keys, collapse = "|"), x)) {
    return(
      paste0(r"--(@)--", x)
      )
  }
}

keywords("hcpcs")

glue_bracket <- \(x) glue::glue("[{x}]")

glue_parens <- \(x) glue::glue("({x})")

glue_quote_double <- \(x) glue::glue('"{x}"')

glue_quote_single <- \(x) glue::glue("'{x}'")


print_list2 <- function(x, pre = "* ") {

  if (length(x) == 0) cat("<empty>\n")

  ns <- names(x)

  if (length(ns) != length(x)) stop("all elements must be named")

  x <- lapply(x, as.character)

  cat(sprintf("%s%s : %s", pre, format(ns), x), sep = "\n")

  invisible(x)
}


time_format <- function(secs) {
  if (secs > 60) {
    secs <- as.integer(secs)
    sprintf(
      "%02d:%02d:%02d", secs %/% 3600L, (secs %/% 60L) %% 60L,
      secs %% 60L
    )
  } else {
    sprintf(if (secs >= 10) {
      "%.1fs"
    } else {
      "%.3fs"
    }, secs)
  }
}

hcpcs_to_regex <- \(x) {

  uq_rm_na <- \(x) collapse::funique(collapse::na_rm(x))

  split_by_length <- \(x) split(x, nchar(x))

  postfix <- \(x) {
    d <- 5 - collapse::fmax(collapse::vlengths(x))
    if (d > 0 & d < 5) glue::glue("[0-9A-Z]{<<d>>,<<d>>}", .open = "<<", .close = ">>")
    else NA_character_
  }

  process_split <- \(split, post) {

    len <- collapse::vlengths(split)

    if (collapse::allv(len,  1)) {

      pre <- glue::glue_collapse(split)
      return(glue::glue("[<<pre>>]", .open = "<<", .close = ">>") + post)

    }

    if (collapse::allv(len,  2)) {

      pre <- uq_rm_na(stringfish::sf_substr(split, 1, 1))
      mid <- glue::glue_collapse(uq_rm_na(stringfish::sf_substr(split, 2, 2)))
      return(glue::glue("<<pre>>[<<mid>>]", .open = "<<", .close = ">>") + post)

    }

    if (collapse::allv(len,  3)) {

      pre <- uq_rm_na(stringfish::sf_substr(split, 1, 2))
      mid <- glue::glue_collapse(uq_rm_na(stringfish::sf_substr(split, 3, 3)))
      return(glue::glue("<<pre>>[<<mid>>]", .open = "<<", .close = ">>") + post)

    }

    if (collapse::allv(len,  4)) {

      pre <- uq_rm_na(stringfish::sf_substr(split, 1, 3))
      mid <- glue::glue_collapse(uq_rm_na(stringfish::sf_substr(split, 4, 4)))
      return(glue::glue("<<pre>>[<<mid>>]", .open = "<<", .close = ">>") + post)

    }

    if (collapse::allv(len,  5)) {

      pre <- uq_rm_na(stringfish::sf_substr(split, 1, 4))
      mid <- glue::glue_collapse(uq_rm_na(stringfish::sf_substr(split, 5, 5)))
      return(glue::glue("<<pre>>[<<mid>>]", .open = "<<", .close = ">>"))

    } else {

      glue::glue("ERROR: <<split>>::<<post>>", .open = "<<", .close = ">>")

    }
  }

  x <- gsub(" ", "", uq_rm_na(x))

  split <- split_by_length(x)

  post <- purrr::map_vec(split, postfix)

  processed <- purrr::map2_vec(split, post, process_split)

  glue::glue_collapse(
    glue::glue("^({processed})$"),
    sep = "|")

  # vctrs::vec_slice(
  # hcpcs,
  # stringfish::sf_grepl(
  # hcpcs$hcpcs,
  # "^9201[589]$"
  # )
  # )

}


# 116
# ex_500 <- c(
#   '0214T', '0215T', '0217T', '0218T', '0219T', '0220T', '0221T',
#   '0222T', '0263T', '0265T', '0266T', '0269T', '0274T', '0275T',
#   '0329T', '0330T', '0422T', '0444T', '0445T', '0506T', '0507T',
#   '15777', '20939', '22510', '22511', '22512', '22513', '22514',
#   '22515', '22526', '22527', '27197', '27198', '30801', '30802',
#   '31231', '32673', '34713', '34714', '34715', '34716', '34717',
#   '36221', '34812', '34820', '34833', '34834', '35572', '50300',
#   '50540', '54420', '54430', '55200', '55250', '55300', '58575',
#   '58600', '58605', '58700', '58720', '58800', '58805', '58900',
#   '58920', '58925', '58940', '58943', '61000', '61001', '61253',
#   '63035', '63043', '63044', '63045', '63046', '63047', '63048',
#   '64421', '64480', '64484', '64491', '64492', '64494', '64495',
#   '64634', '64636', '76514', '92025', '92081', '92082', '92083',
#   '92132', '92133', '92134', '92145', '92201', '92202', '92227',
#   '92228', '92229', '92235', '92240', '92242', '95870', 'C7501',
#   'C7502', 'C7504', 'C7505', 'C9771', 'E0675', 'G0279', 'G0412',
#   'G0413', 'G0414', 'G0415', 'S2342')

# dplyr::tibble(
#   hcpcs = c(ex_500)) |>
#   dplyr::arrange(hcpcs) |>
#   dplyr::mutate(x = stringr::str_split(hcpcs, "")) |>
#   tidyr::unnest_wider(x, names_sep = "") |>
  # dplyr::count(x1, x2, x3, x4, x5)


# hcpcs_to_regex(ex_500)
# construct_regex(ex_500)


# vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^[CEGS0-35-79][0-9]{3}[T0-9]$"))
