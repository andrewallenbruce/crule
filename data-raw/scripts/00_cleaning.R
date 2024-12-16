# Packages ####

library(here)
library(tidyverse)
library(fuimus)
library(strex)

# Load CSV ####

rules <- read_csv(
  here::here("data-raw", "data/rules_raw.csv"),
  col_types = cols(
    row       = col_integer(),
    id_rule   = col_integer(),
    id_name   = col_character(),
    id_order  = col_integer(),
    category  = col_factor(),
    alert     = col_character(),
    var       = col_character(),
    action    = col_character(),
    value     = col_character(),
    rule      = col_character(),
    x9        = col_character(),
    x10       = col_character()
  )
) |>
  mutate(
    category = remove_quotes(as.character(category)),
    category = str_remove_all(category, "\\r|\\n"),
    alert = remove_quotes(alert),
    alert = str_remove_all(alert, "\\r|\\n"),
    rule = remove_quotes(rule),
    rule = str_remove_all(rule, "\\r|\\n")
  ) |>
  fill(rule) |>
  select(
    index = row,
    number = id_rule,
    order = id_order,
    identifier = id_name,
    variable = var,
    action,
    value,
    category,
    definition = rule,
    rationale  = alert
  ) |>
  filter(
    !number %in% c(273:274, 293, 450, 466:468, 701),
    !index %in% c(588, 1489),
    definition != "Delete"
  )

rules[rules$index == 1, 9, drop = TRUE]  <- "CPT Code is [43760] AND Encounter Date of Service after [01/01/2019]"
rules[rules$index == 2, 9, drop = TRUE]  <- "CPT Code is [43760] AND Encounter Date of Service after [01/01/2019]"
rules[rules$index == 83, 7, drop = TRUE] <- "SL"

descriptors <- rules |>
  select(index, number, identifier, category, definition, rationale) |>
  distinct() |>
  mutate(
    source = str_extract_all(category, "https?://[^\\s]+"),
    category = str_remove_all(category, "https?://[^\\s]+")
  ) |>
  unnest(source, keep_empty = TRUE) |>
  separate_longer_delim(source, delim = "https:") |>
  mutate(
    source = na_if(source, "") |> str_squish(),
    source = if_else(!is.na(source), str_c("https:", source), source),
    category = na_if(category, "") |> str_squish(),
    definition = str_squish(definition),
    definition = str_remove_all(definition, "►"),
    definition = gsub(",(?!\\s)", ", ", definition, perl = TRUE) |> str_squish(),
    rationale = str_squish(rationale),
    rationale = str_remove_all(rationale, "►"),
    rationale = gsub(",(?!\\s)", ", ", rationale, perl = TRUE) |> str_squish()
  )

identifier <- descriptors |>
  distinct(number, identifier) |>
  reframe(
    number,
    identifier = str_remove_all(identifier, "-"),
    payer = if_else(
      str_detect(identifier, "BCBS|MCD|VFC"),
      str_extract(identifier, "BCBS|MCD|VFC"),
      NA_character_
    ),
    state = if_else(
      !is.na(payer),
      str_extract(identifier, paste0(state.abb, collapse = "|")),
      NA_character_
    ),
    split = str_split_by_numbers(identifier)
  ) |>
  unnest_wider(split, names_sep = "_") |>
  mutate(
    split_1 = case_match(
      split_1,
      c("VACCN", "COVID") ~ "VAX",
      "ICD" ~ "ICD:CM",
      "ANEST" ~ "ANES",
      "TELEH" ~ "TELE",
      "DIGES" ~ "GASTRO",
      "PRVNT" ~ "PREVENT",
      "OPTHL" ~ "OPHTH",
      "FOOT" ~ "PODIA",
      "FERTL" ~ "FERT",
      .default = split_1
    )
  ) |>
  # group_by(payer) |>
  # mutate(split_2 = if_else(!is.na(payer), row_number(), as.integer(split_2))) |>
  # ungroup() |>
  # mutate(split_2 = str_pad(
  #   as.character(as.integer(split_2)),
  #   width = 3,
  #   side = "left",
  #   pad = "0"
  # )) |>
  combine(name = payer_state,
          columns = c("payer", "state"),
          sep = ":") |>
  mutate(split_1 = if_else(!is.na(payer_state), payer_state, split_1),
         payer_state = NULL) |>
  # combine(
  #   name = identifier,
  #   columns = c("split_1", "split_2"),
  #   sep = ":"
  # ) |>
  # mutate(
  #   split_1 = NULL,
  #   identifier = str_replace(identifier, "VAX:19", "VAX:00"),
  #   identifier = str_replace(identifier, "ICD:CM:10", "ICD:CM:")
  # ) |>
  mutate(split_2 = NULL,
         identifier = NULL) |>
  select(number,
         identifier = split_1)

# identifier[1, 2, drop = TRUE] <- "HCPCS:001"
identifier[1, 2, drop = TRUE] <- "HCPCS:EXP"

descriptors <- descriptors |>
  select(-identifier) |>
  left_join(identifier, by = "number") |>
  relocate(identifier, .before = category) |>
  select(-index, -source) |>
  distinct(number, .keep_all = TRUE)

components <- rules |>
  select(number, order, variable, action, value) |>
  distinct() |>
  left_join(identifier, by = "number") |>
  relocate(identifier, .before = variable) |>
  mutate(variable = case_match(variable,
      c(
        "CPT CODE",
        "CPT Code",
        "CPT code",
        "CPT",
        "Code Check Expired CPT Alert (Presence)",
        "CPT is one of"
      ) ~ "hcpcs",
      c(
        "Mod1",
        "CPT Mod1",
        "CPT Mod 1",
        "CPTMod1",
        "Code Check CPT Mod Alert (Presence)",
        "CPT MOD1",
        "CPT Mod1 (Presence)"
      ) ~ "mod_1",
      c(
        "Mod2",
        "CPT MOD2",
        "CPT Mod 2",
        "CPT Mod2 (Presence)",
        "CPTMod2",
        "CPT Mod2"
      ) ~ "mod_2",
      "CPT Mod3" ~ "mod_3",
      "CPT Mod4" ~ "mod_4",
      "CPT Units" ~ "unit",
      c(
        "Dx code",
        "DX Code",
        "Dx Code",
        "Code Check Expired DX Alert (Presence)"
      ) ~ "icd",
      c("Encounter Date of Service", "Date of Service") ~ "dos",
      c("Location", "Place of Service") ~ "pos",
      c("CPT UB04", "UB04 Bill Type") ~ "ub04",
      "CPT NDC (Presence)" ~ "ndc",
      "CPT Rev Code" ~ "rev_code",
      "Code Check CCI Alert (Presence)" ~ "cci",
      "Code Check LCD Alert (Presence)" ~ "lcd",
      "Code Check NCD Alert (Presence)" ~ "ncd",
      c("Patient Age", "Code Check Age Alert (Presence)") ~ "age",
      c("Patient Sex", "Code Check Gender Alert (Presence)") ~ "sex",
      "NA" ~ NA_character_,
      "Rendering Provider" ~ "rendering",
      "Referring Provider (Presence)" ~ "referring",
      c(
        "Primary Insurance Class",
        "Primary Insurance class",
        "Primary insurance class"
      ) ~ "primary_class",
      "Primary Insurance" ~ "primary_name",
      "Secondary Insurance Class" ~ "secondary_class",
      "Secondary Insurance" ~ "secondary_name",
      c(
        "Primary Insurance Authorization (Presence)",
        "Primary Insurance Authorization (Presence) is not [Present]"
      ) ~ "primary_auth",
      .default = variable
    )
  )

rm(identifier)

# components <- components |>
#   mutate(
#     class = case_match(
#       variable,
#       c(
#         "sex",
#         "rendering",
#         "referring",
#         "primary_auth",
#         "primary_class",
#         "primary_name",
#         "secondary_class",
#         "secondary_name",
#         "ncd",
#         "lcd",
#         "cci",
#         "ndc",
#         "ub04",
#         "pos",
#         "icd",
#         "hcpcs",
#         "mod_1",
#         "mod_2",
#         "mod_3",
#         "mod_4",
#         "rev_code"
#       ) ~ "<chr>",
#       c("age", "unit") ~ "<int>",
#       c("dos", "dob") ~ "<date>"
#     ) |> as_factor())
#
# components <- components |>
#     mutate(group = case_match(
#       as.character(variable),
#       c(
#         "sex",
#         "dos",
#         "age",
#         "pos",
#         "rendering",
#         "referring") ~ "encounter",
#       c(
#         "hcpcs",
#         "mod_1",
#         "mod_2",
#         "mod_3",
#         "mod_4",
#         "rev_code",
#         "unit",
#         "ndc",
#         "pos",
#         "icd",
#         "ub04"
#       ) ~ "coding",
#       c("ncd", "lcd", "cci") ~ "ncci",
#       c(
#         "primary_auth",
#         "primary_class",
#         "primary_name",
#         "secondary_class",
#         "secondary_name"
#       ) ~ "payer"
#     ) |> as_factor()
#   )

# Cleaning ####
columns <- c(
  'number',
  'identifier',
  'order',
  'variable',
  'action',
  'value',
  'condition')

clean_dos <- \(x = components) {
  x |>
    filter(variable == "dos") |>
    mutate(
      value = anytime::anydate(value) |> as.character(),
      action = case_match(action,
                          c("is before", "before") ~ "less than",
                          c("is after", "after") ~ "greater than",
                          .default = action),
      method = if_else(str_detect(action, "greater than"), ">", "<"),
      condition = glue::glue('{variable} {method} "{value}"')
    ) |>
    select(all_of(columns))
}

clean_age <- \(x = components) {
  x |>
    filter(variable == "age") |>
    mutate(
      age = strex::str_extract_numbers(value),
      period = strex::str_extract_non_numerics(value),
      .before = variable
    ) |>
    unnest(c(age, period)) |>
    mutate(
      age = as.integer(age),
      period = str_remove_all(period, "\\s|,"),
      value = NULL,
      method = case_match(
        action,
        c("is younger than", "younger than") ~ "<",
        "is older than" ~ ">",
        "is" ~ "==",
        .default = NA_character_
      ),
      action = case_match(action,
                          c("is younger than") ~ "less than",
                          c("is older than") ~ "greater than",
                          # c("is") ~ "is",
                          .default = action),
      value = case_when(
        period == "years" ~ as.duration(years(age)) / ddays(1),
        period == "months" ~ as.duration(months(age)) / ddays(1),
        period == "days" ~ as.duration(days(age)) / ddays(1),
        .default = NA_real_
      ),
      value = as.integer(value) |> as.character(),
      condition = glue::glue('{variable} {method} {value}')
    ) |>
    select(all_of(columns))
}

clean_ndc <- \(x = components) {
  x |>
    filter(variable == "ndc") |>
    mutate(
      action = "is not",
      value = "PRESENT",
      condition = glue::glue('!is.na({variable})')) |>
    select(all_of(columns))
}

clean_unit <- \(x = components) {
  x |>
    filter(variable == "unit") |>
    mutate(
      value = as.integer(value),
      method = if_else(str_detect(action, "is not"), "!=", "=="),
      value = as.integer(value) |> as.character(),
      # action = case_match(action,
      #                     c("is") ~ "equals",
      #                     c("is not") ~ "inequal",
      #                     .default = action),
      condition = glue::glue('{variable} {method} {value}')
    ) |>
    select(all_of(columns))
}

clean_sex <- \(x = components) {
  x |>
    filter(variable == "sex") |>
    mutate(
      value = str_to_upper(value),
      method = if_else(str_detect(action, "is"), "==", "!="),
      action = "is",
      condition = glue::glue('!is.na({variable})'),
      condition = if_else(
        value == "PRESENT",
        glue::glue('!is.na({variable})'),
        glue::glue('{variable} {method} "{value}"')
      )
    ) |>
    select(all_of(columns))
}

clean_ub04 <- \(x = components) {
  x |>
    filter(variable == "ub04") |>
    mutate(
      value = str_to_upper(value),
      value = if_else(action == "is not", "FALSE", value),
      action = "is",
      method = if_else(str_detect(action, "is"), "==", "!="),
      condition = glue::glue('{variable} {method} {value}')
    ) |>
    select(all_of(columns))
}

clean_pos <- \(x = components) {
  components |>
    filter(variable == "pos") |>
    mutate(pos = str_extract_all(value, r'{\((\d+)\)(?:\s*\([^)]*\))?}')) |>
    unnest(pos, keep_empty = TRUE) |>
    mutate(
      pos = substr(pos, 2, 3),
      pos = if_else(
        is.na(pos) &
          str_detect(value, "^\\d{2}$"),
        str_extract(value, "^\\d{2}$"),
        pos
      ),
      pos = if_else(
        is.na(pos) &
          str_detect(value, "^\\d{2}\\s+"),
        str_extract(value, "^\\d{2}"),
        pos
      ),
      value = pos,
      pos = glue::glue('"{pos}"')
    ) |>
    nest(pos = c(pos),
         value = c(value)) |>
    rowwise() |>
    mutate(pos = map(pos, ~ glue::glue_collapse(., sep = ", ")),
           value = map(value, ~ paste0(., collapse = ", "))) |>
    unnest(cols = c(pos, value)) |>
    ungroup() |>
    mutate(
      pos = case_when(
        number == 1007 ~ glue::as_glue('"19", "21", "22"'),
        number == 1013 ~ glue::as_glue('"31", "32", "54", "56"'),
        number == 1014 ~ glue::as_glue('"12", "13", "14", "16", "33", "55"'),
        number %in% c(1891, 930, 1036) ~ glue::as_glue('"02", "10"'),
        number %in% c(275, 276) ~ glue::as_glue('"81"'),
        .default = pos
      ),
      value = case_when(
        number == 1007 ~ "19, 21, 22",
        number == 1013 ~ "31, 32, 54, 56",
        number == 1014 ~ "12, 13, 14, 16, 33, 55",
        number %in% c(1891, 930, 1036) ~ "02, 10",
        number %in% c(275, 276) ~ "81",
        .default = value
      ),
      commas = str_detect(value, ","),
      action = case_when(
        commas & action %in% c("is", "is one of") ~ "any of",
        commas & action %in% c("is not") ~ "none of",
        .default = action),
      pos = if_else(commas, glue::glue("c({pos})"), pos),
      method = case_when(
        action == "any of" ~ "%in%",
        action == "none of" ~ "%nin%",
        action == "is" ~ "==",
        action == "is not" ~ "!=",
        .default = NA_character_
      ),
      condition = glue::glue('{variable} {method} {pos}'),
      pos = NULL,
      commas = NULL,
      method = NULL
      ) |>
    select(all_of(columns))
}

clean_mods <- \(x = components) {
  mod_base <- x |>
    filter(variable %in% c("mod_1", "mod_2", "mod_3", "mod_4")) |>
    mutate(
      value = if_else(identifier == "MCD:CA:013" &
                        variable == "mod_1", "SL", value),
      chars = nchar(value)
    ) |>
    arrange(desc(chars))

  mod_singles <- mod_base |>
    filter(chars == 2 | value == "Present",
           str_detect(value, fixed("*"), negate = TRUE)) |>
    mutate(
      method = if_else(str_detect(action, "is"), "==", "!="),
      condition = if_else(
        value == "Present",
        glue::glue('!is.na({variable})'),
        glue::glue('{variable} {method} "{value}"')
      )
    ) |>
    select(all_of(columns))

  mod_wildcards <- mod_base |>
    filter(str_detect(value, fixed("*"))) |>
    mutate(
      method = case_when(
        action == "is not" &
          value == "F*, T*" ~ glue::glue("^[^F|T][A-Z0-9]$"),
        action == "is" &
          value == "P*" ~ glue::glue("^[P][A-Z0-9]$"),
        .default = NA_character_
      ),
      condition = glue::glue('func({variable}, "{method}")')
    ) |>
    select(all_of(columns))

  mod_multi <- mod_base |>
    filter(chars > 2,
           value != "Present",
           str_detect(value, fixed("*"), negate = TRUE)) |>
    mutate(
      value = str_remove_all(value, " "),
      value = str_replace_all(value, ";", ","),
      chars = NULL
    ) |>
    separate_longer_delim(cols = value, delim = ",") |>
    mutate(mods = glue::glue("'{value}'")) |>
    nest(mods = c(mods), value = c(value)) |>
    rowwise() |>
    mutate(mods = map(mods, ~ paste0(., collapse = ", ")),
           value = map(value, ~ paste0(., collapse = ", "))) |>
    unnest(cols = c(mods, value)) |>
    ungroup() |>
    mutate(
      mods = glue::glue("c({mods})"),
      method = "%in%",
      condition = if_else(
        action == "is not",
        glue::glue('!{variable} {method} {mods}'),
        glue::glue('{variable} {method} {mods}')
      )
    ) |>
    select(all_of(columns))

  vctrs::vec_c(mod_singles, mod_wildcards, mod_multi)
}

# clean_mods2 <- \(x = components) {
#   mod_base <- x |>
#     filter(variable %in% c("mod_1", "mod_2", "mod_3", "mod_4")) |>
#     mutate(value = str_replace_all(value, ", ", ","),
#            value = str_replace_all(value, ",", ", "),
#            value = str_replace_all(value, "; ", ", "),
#            value = str_to_upper(value),
#            commas = str_detect(value, ","),
#            wc = str_detect(value, fixed("*")),
#            action = case_when(
#              commas & action %in% c("is", "is one of") ~ "any of",
#              commas & action %in% c("is not") ~ "none of",
#              !commas & action %in% c("is", "is one of") ~ "is",
#              !commas & action %in% c("is not") ~ "is not",
#              .default = action)) |>
#     separate_longer_delim(cols = value, delim = ", ") |>
#     mutate(mods = glue::glue('"{value}"')) |>
#     nest(mods = c(mods),
#          value = c(value)) |>
#     rowwise() |>
#     mutate(mods = map(mods, ~ glue::glue_collapse(., sep = ", ")),
#            value = map(value, ~ paste0(., collapse = ", "))) |>
#     unnest(cols = c(mods, value)) |>
#     ungroup() |>
#     # mutate(
#     #   mods = case_when(
#     #     wc & commas & action == "is not" ~ glue::glue("^[^{str_replace_all(str_remove_all(mods, fixed('*')), fixed(', '), '|')}]"),
#     #     .default = mods)) #,
#       # method = "%in%",
#       # condition = if_else(
#       #   action == "is not",
#       #   glue::glue('!{variable} {method} {mods}'),
#       #   glue::glue('{variable} {method} {mods}'),
#            # method = case_when(
#            #   !wc & !commas & action == "is not" ~ glue::glue("^[^{value}]"),
#            #   !wc & !commas & action == "is" ~ glue::glue("^[{value}]"),
#            #   # !wc & commas & action == "any of" ~ glue::glue("^[({value})]"),
#            #   # !wc & action %in% c("is not", "none of") ~ glue::glue("^[^{value}]"),
#            #   # !wc & action %in% c("is", "any of") ~ glue::glue("^[{value}]"),
#            #   .default = NA_character_)
#            # )
#     # filter(wc == TRUE, action == "is not") |>
#     print(n = 300)
#
#   mod_singles <- mod_base |>
#     filter(chars == 2 | value == "Present",
#            str_detect(value, fixed("*"), negate = TRUE)) |>
#     mutate(
#       method = if_else(str_detect(action, "is"), "==", "!="),
#       condition = if_else(
#         value == "Present",
#         glue::glue('!is.na({variable})'),
#         glue::glue('{variable} {method} "{value}"')
#       )
#     ) |>
#     select(all_of(columns))
#
#   mod_wildcards <- mod_base |>
#     filter(str_detect(value, fixed("*"))) |>
#     mutate(
#       method = case_when(
#         action == "is not" &
#           value == "F*, T*" ~ glue::glue("^[^F|T][A-Z0-9]$"),
#         action == "is" &
#           value == "P*" ~ glue::glue("^[P][A-Z0-9]$"),
#         .default = NA_character_),
#       condition = glue::glue('func({variable}, "{method}")')
#     ) |>
#     select(all_of(columns))
#
#   mod_multi <- mod_base |>
#     filter(chars > 2,
#            value != "Present",
#            str_detect(value, fixed("*"), negate = TRUE)) |>
#     mutate(
#       value = str_remove_all(value, " "),
#       value = str_replace_all(value, ";", ","),
#       chars = NULL
#     ) |>
#     separate_longer_delim(cols = value, delim = ",") |>
#     mutate(mods = glue::glue("'{value}'")) |>
#     nest(mods = c(mods), value = c(value)) |>
#     rowwise() |>
#     mutate(mods = map(mods, ~ paste0(., collapse = ", ")),
#            value = map(value, ~ paste0(., collapse = ", "))) |>
#     unnest(cols = c(mods, value)) |>
#     ungroup() |>
#     mutate(
#       mods = glue::glue("c({mods})"),
#       method = "%in%",
#       condition = if_else(
#         action == "is not",
#         glue::glue('!{variable} {method} {mods}'),
#         glue::glue('{variable} {method} {mods}')
#       )
#     ) |>
#     select(all_of(columns))
#
#   vctrs::vec_c(mod_singles, mod_wildcards, mod_multi)
# }

clean_rev_code <- \(x = components) {
  x |>
    filter(variable == "rev_code") |>
    mutate(
      method = if_else(str_detect(action, "is not"), "!=", "=="),
      condition = glue::glue('{variable} {method} "{value}"')
    ) |>
    select(all_of(columns))
}

clean_referring <- \(x = components) {
  x |>
    filter(variable == "referring") |>
    mutate(
      action = "is not",
      value = "Present",
      condition = glue::glue('!is.na({variable})')
    ) |>
    select(all_of(columns))
}

clean_primary_auth <- \(x = components) {
  x |>
    filter(variable == "primary_auth") |>
    mutate(value = "Present",
           condition = glue::glue('!is.na({variable})')) |>
    select(all_of(columns))
}

clean_secondary_class <- \(x = components) {
  x |>
    filter(variable == "secondary_class") |>
    mutate(
      value = if_else(number == 537, "ANY class EXCEPT Medicaid & Medicaid CMO", value),
      value = str_replace_all(value, ",,", ","),
      value = str_replace_all(value, ",", ", "),
      value = str_replace_all(value, "MEDICARE", "Medicare"),
      value = str_replace_all(value, "MedicareREPLACEMENT", "Medicare Replacement"),
      value = str_replace_all(value, "MEDICAID", "Medicaid"),
      value = str_replace_all(value, "MedicaidCMO", "Medicaid CMO"),
      value = str_replace_all(value, "COMMERCIAL", "Commercial"),
      condition = case_when(
        number == 339 ~ glue::glue('{variable} == "Medicare"'),
        number == 340 ~ glue::glue('{variable} %in% c("Medicare", "Medicare Replacement")'),
        number == 341 ~ glue::glue('{variable} %in% c("Medicaid", "Medicaid CMO")'),
        number == 342 ~ glue::glue(
          '{variable} %in% c("Commercial", "Medicare", "Medicare Replacement")'
        ),
        number == 344 ~ glue::glue(
          '{variable} %in% c("Commercial", "Medicare", "Medicare Replacement")'
        ),
        number == 537 ~ glue::glue(
          '!is.na({variable}) & !{variable} %in% c("Medicaid", "Medicaid CMO")'
        )
      )
    ) |>
    select(all_of(columns))
}

clean_secondary_name <- \(x = components) {
  x |>
    filter(variable == "secondary_name") |>
    mutate(
      value = str_replace(
        value,
        "Tricare supplement, Selman",
        "Tricare Supplement, Selman"
      ),
      value = str_replace(value, fixed("WA Medicaid (DSHS)"), "Medicaid WA"),
      value = str_replace(value, fixed("WELLCARE OF GEORGIA, INC (129)"), "Wellcare GA"),
      value = str_replace(value, "Wellcare Mediaid", "Medicaid Wellcare"),
      method = if_else(str_detect(action, "is not"), "!=", "=="),
      condition = glue::glue('{variable} {method} "{value}"'),
      condition = if_else(
        value == "Tricare Supplement, Selman",
        glue::glue('{variable} %in% c("Tricare Supplement", "Selman")'),
        value
      )
    ) |>
    select(all_of(columns))
}

clean_primary_class <- \(x = components) {
  x |>
    filter(variable == "primary_class") |>
    mutate(
      value = str_remove_all(value, regex(r"{\(\d+\)}")),
      value = str_remove_all(value, regex(r"{\(\D+\)}")),
      value = str_replace_all(value, " ,", ","),
      value = str_replace_all(value, ", ", ","),
      value = str_replace_all(value, " ; ", ","),
      value = str_replace_all(value, fixed("/"), ","),
      value = str_replace_all(value, fixed("*"), ""),
      value = if_else(str_detect(value, "Select all|pick all"), "All", value),
      value = str_squish(value),
      action = if_else(action == "is on of", "is one of", action)
    ) |>
    separate_longer_delim(cols = value, delim = ",") |>
    mutate(
      value = toupper(value),
      value = case_match(
        value,
        c("MEDICAIDCMO", "MEDICAID MCO", "CMO") ~ "MEDICAID CMO",
        c(
          "MEDICAREREPLACEMENT",
          "MEDICAREADVANTAGE",
          "MEDICARE ADV",
          "MEDICARE ADVANTAGE"
        ) ~ "MEDICARE REPLACEMENT",
        c(
          "WORKER'S COMP",
          "WORKERSCOMPENSATION",
          "WORKERS COMPENSATION",
          "WORK COMP",
          "WORKER'SCOMP",
          "AUTO"
        ) ~ "WORKERS COMP",
        c("KANSAS MEDICAID", "GEORGIA MEDICAID") ~ "MEDICAID",
        c("MEDICARE PART A", "MEDICARE PART B") ~ "MEDICARE",
        c("GROUP", "BCBS", "COMMERICAL") ~ "COMMERCIAL",
        "TRIARE" ~ "TRICARE",
        .default = value
      )
    ) |>
    mutate(payers = glue::glue('"{value}"')) |>
    nest(payers = c(payers), value = c(value)) |>
    rowwise() |>
    mutate(
      payers = map(payers, ~ glue::glue_collapse(., sep = ", ")),
      value = map(value, ~ paste0(., collapse = ", "))
    ) |>
    unnest(cols = c(payers, value)) |>
    ungroup() |>
    mutate(
      method = case_when(
        action == "is" ~ "==",
        action == "is not" ~ "!=",
        action == "is one of" ~ "%in%"
      ),
      condition = case_when(
        action %in% c("is", "is not") ~ glue::glue('{variable} {method} {payers}'),
        action == "is one of" ~ glue::glue('{variable} {method} c({payers})')
      )
    ) |>
    select(all_of(columns))
}

clean_primary_name <- \(x = components) {
  x |>
    filter(variable == "primary_name") |>
    mutate(
      value = str_remove_all(value, regex(r"{\(\d+\)}")),
      value = str_remove_all(value, regex(r"{\(\D+\)}")),
      value = str_replace_all(value, " ,", ","),
      value = str_replace_all(value, ", ", ","),
      value = str_replace_all(value, " ; ", ","),
      value = str_replace_all(value, fixed("/"), ","),
      value = str_replace_all(value, fixed("*"), ""),
      value = str_replace_all(value, fixed("("), ""),
      value = str_replace_all(value, fixed("-"), ""),
      value = str_replace_all(value, fixed(" - "), ""),
      value = str_replace_all(value, fixed(" -- "), ""),
      value = str_squish(value)
    ) |>
    separate_longer_delim(cols = value, delim = ",") |>
    mutate(
      value = str_squish(value),
      value = toupper(value),
      value = case_match(
        value,
        # UHC
        c(
          "UNITED HEALTHCARE",
          "UNITED HEALTHCARE MEDICARE",
          "UNITED HEALTHCARE MEDICARE PLANS",
          "SELECT ALL OCCURANCES OF UHC",
          "SELECT ALL UHC",
          "& UHC INSURANCES",
          "UHC ADV PLAN",
          "UNITEDHEALTHCARE",
          "AARP UHC",
          "UHC COMMUNITY PLAN",
          "UHC DUAL COMPLETE",
          "UHC MEDICAID MO",
          "UHCMEDICARECOMPLETE",
          "UNITED HEALTH CARE",
          "UNITED HEALTH SHARED SERVICES",
          "UNITED HEALTHCARE CLAIM DIVISION",
          "UNITED HEALTHCARE DUAL COMPLETE",
          "UNITED HEALTHCARE MEDICARE SOLUTIONS",
          "UNITED HEALTHCARE RIVER VALLEY",
          "UNITED HEALTHCARE STUDENT",
          "UNITED HEALTHCARE STUDENT RESOURCES"
        ) ~ "UHC",
        # BCBS
        c(
          "BLUE SHIELD OF CALIFORNIA",
          "BLUE CROSS BLUE SHIELD OF NC",
          "BLUE CROSS BLUE SHIELD",
          "BLUE CROSS",
          "BLUECROSSKS",
          "ALL BLUE CROSS",
          "BCBS FEDERAL EMPLOYEE PROGRAM",
          "BCBS FLORIDA",
          "BCBS ILLINOIS",
          "BCBS MI",
          "BCBSMI",
          "BCBS WI",
          "BCBS OF NC",
          "BLUE CROSS BLUE SHIELD OF SOUTH CAROLINA",
          "BCBS AL",
          "BCBS AL BLUE ADVANTAGE MEDICARE REPLAC",
          "BCBS BCBS",
          "BCBS AL ALL KIDS MEDICAID CHIP",
          "BCBS AL FEDERAL EMPLOYEE PROGRAM",
          "BCBS AL FEDERAL EMPLOYEE PROGRAM PPO",
          "BCBS AL HSA PPO",
          "BCBS AL INDEMNITY",
          "BCBS AL MEDICARE SUPPLEMENT",
          "BCBS AL OUT OF STATE BLUE CARD",
          "BCBS AL OUT OF STATE BLUE CARD PPO",
          "BCBS AL PEEHIP PPO",
          "BCBS AL POS",
          "BCBS AL PPO",
          "BCBS AL PREFERRED CARE",
          "BCBS AL PREFERRED CARE PPO",
          "BCBS BCBS HMO",
          "BLUE CROSS BLUE OPEN ACCESS HMO",
          "BLUE CROSS BLUE SHIELD OF AL",
          "BLUE CROSS BLUE SHIELD OF FLORIDA",
          "BLUE CROSS BLUE SHIELD OF NORTH CAROLINA",
          "BLUE CROSS BLUE SHIELD PPO",
          "BLUE CROSS STATE HEALTH BENEFIT PLAN",
          "BLUE SHIELD",
          "BLUE SHIELD OF GEORGIA FEDERAL",
          "BLUE SHIELD OF GEORGIA PPO",
          "BLUECROSS BLUESHIELD",
          "BCBS CA",
          "BCBS FEP",
          "BCBS MEDICARE",
          "BCBS OF ALABAMA",
          "BCBS OF ARKANSAS",
          "BCBS OF AZ",
          "BCBS OF GA",
          "BCBS OF MS",
          "BCBS OF RI BCBS OF RHODE ISLAND",
          "BCBS OF SC",
          "BCBS OF SOUTH CAROLINA",
          "BCBS OF TEXAS",
          "BCBS OF VA",
          "BCBS SC",
          "BCBS SC BLUE CROSS BLUE SHIELD OF SC",
          "BCBS TX",
          "BLUE CARE",
          "BLUE CROSS & BLUE SHIELD OF MISSISSIPPI",
          "BLUE CROSS BLUE SHIE",
          "BLUE CROSS BLUE SHIELD BLUE CARE NETWORK",
          "BLUE CROSS BLUE SHIELD FASTENAL",
          "BLUE CROSS BLUE SHIELD OF CALIFORNIA",
          "BLUE CROSS BLUE SHIELD OF GEORGIA BCB",
          "BLUE CROSS BLUE SHIELD OF ILLINOIS",
          "BLUE CROSS BLUE SHIELD OF ILLINOIS BC",
          "BLUE CROSS BLUE SHIELD OF ILLINOIS BL",
          "BLUE CROSS BLUE SHIELD OF TENNESSEE",
          "BLUE CROSS BLUE SHIELD PR",
          "BLUE CROSS BLUE SHIELDS CANADIAN SERVICE",
          "BLUE CROSS BLUESHIELD OF TENNESSEE BL",
          "BLUE CROSS FLORIDA",
          "BLUE CROSS MEDICARE ADVANTAGE ALL INSTANCES OF BLUE CROSS BLUE SHIELD MEDICARE",
          "BLUE CROSS OF GA",
          "BLUE CROSS TOTAL BLUE CROSS TOTAL MED",
          "BLUE SHIELD OF TEXAS",
          "BLUECARE PLUS DSNP",
          "BLUECARE TNCARE",
          "BLUECROSS",
          "BLUECROSS BLUE CROSS BLUE SHIELD OF",
          "BLUECROSS BLUECROSS BLUESHIELD HEALT",
          "BLUECROSS BLUESHIELD BLUECROSS BLUESH",
          "BLUECROSS BLUESHIELD OF ILLINOIS BLUE",
          "BLUECROSS CAPITAL BLUE",
          "BLUECROSS PEBA",
          "INDEPENDENCE BLUE CROSS PERSONAL CHOIC",
          "CAREFIRST BCBS",
          "FLORIDA BLUE",
          "SELECT ALL BCBS",
          "SELECT ALL OCCURANCES OF BCBS",
          "SELECT ALL OCCURANCES OF HEALTHY BLUE",
          "VSHP BLUECARE RISK EAST"
        ) ~ "BCBS",
        # ANTHEM
        c(
          "BLUE CROSS ANTHEM",
          "ANTHEM BLUE CROSS",
          "ANTHEM BCBS",
          "BLUE CROSSCA: ANTHEM BLUE CROSS",
          "AND ANTHEM PLANS"
        ) ~ "ANTHEM",
        c(
          "HIGHMARK BCBS",
          "HIGHMARK BLUE CROSS BLUE SHIELD",
          "HIGHMARK BLUE SHIELD",
          "HIGHMARK BLUE SHIELD FREEDOM BLUE MED"
        ) ~ "HIGHMARK",
        c("CARESOURCE OH") ~ "CARESOURCE",
        c("COORDINATED CARE AMBETTER", "COORDINATED CARE") ~ "AMBETTER",
        c("AETNA MEDICARE") ~ "AETNA",
        c("AMERIGROUP COMMUNITY CARE TN") ~ "AMERIGROUP",
        c("MEDCAL", "MEDICAL", "OP MEDICAL") ~ "MEDI-CAL",
        c("GREAT AMERICAN INSURANCE COMPANY") ~ "GREAT AMERICAN",
        c(
          "HUMANA MEDICARE",
          "SELECT ALL HUMANA INSURANCES",
          "SELECT ALL HUMANA"
        ) ~ "HUMANA",
        c(
          "SELECT ALL OCCURENCES OF CIGNA",
          "SELECT ALL OCCURANCES OF CIGNA",
          "CIGNA SELECT ALL CIGNA",
          "CIGNA MEDICARE"
        ) ~ "CIGNA",
        c(
          "LIBERTY MUTUAL INSURANCE",
          "LIBERTY MUTAL CORP",
          "LIBERTY MUTAL",
          "LIBERTY MEDICAL MAIL"
        ) ~ "LIBERTY MUTUAL",
        c("MCR") ~ "MEDICARE",
        c("SELECT ALL OCCURANCES OF RAILROAD MEDICARE", "RR MEDICARE") ~ "RAILROAD MEDICARE",
        c(
          "SELECT ALL OCCURANCES OF HORIZON",
          "HORIZON BLUE CROSS BLUE SHIELD OF NEW JE"
        ) ~ "HORIZON",
        c(
          "SELECT ALL OCCURANCES OF DMERC",
          "DMERCJURISDICTIOND",
          "DMERCB",
          "DMERC",
          "DMERCJURISDICTIONA"
        ) ~ "DMERC",
        c("VA CCN OPTUM") ~ "VA",
        c("MEDCOST PREFERRED") ~ "MEDCOST",
        c("PEACH STATE") ~ "PEACHSTATE",
        c(
          "GA MEDICAID",
          "WA MEDICAID",
          "PRIMARY INSURANCE CLASS IS MEDICAID",
          "MEDICAID CA MEDICAL",
          "MEDICAID OF ARKANSAS",
          "MEDICAID OF IDAHO",
          "MEDICAIDOFMO",
          "MEDICAIDOFVERMONT",
          "MEDICAIDOFVT",
          "ARKIDS B"
        ) ~ "MEDICAID",
        c("MEDICARE PART A") ~ "MEDICARE",
        c("COVID19 HRSA UNINSURED FUND") ~ "COVID HRSA",
        c(
          "REGENCE BCBS",
          "REGENCE BLUECROSS BLUESHIELD OF OREGON",
          "REGENCE BCBS MEDICARE",
          "REGENCE BLUE SHIELD"
        ) ~ "REGENCE",
        c("WELLCARE OF GEORGIA", "WELLCARE MEDICAID") ~ "WELLCARE",
        c(
          "SUMMIT AMERICA",
          "SUMMIT CLAIM CENTER",
          "SUMMIT CLAIMS CENTER",
          "SUMMIT INSURANCE",
          "SUMMITT CLAIMS CENTER"
        ) ~ "SUMMIT",
        c(
          "WEST",
          "TRIWEST VA",
          "TRIWEST",
          "TRICAREWEST",
          "TRICARE EAST",
          "TRICARE WEST",
          "TRICAREFORLIFE",
          "TRICARENORTH",
          "TRICAREPRIME",
          "TRICARESOUTH"
        ) ~ "TRICARE",
        c("ZURICH AMERICAN INSURANCE") ~ "ZURICH",
        c("INC", "POS") ~ NA_character_,
        .default = value
      )
    ) |>
    filter(!is.na(value)) |>
    group_by(identifier) |>
    distinct(value, .keep_all = TRUE) |>
    ungroup() |>
    mutate(payers = glue::glue('"{value}"')) |>
    nest(payers = c(payers), value = c(value)) |>
    rowwise() |>
    mutate(payers = map(payers, ~ glue::glue_collapse(., sep = ", ")),
           value = map(value, ~ paste0(., collapse = ", "))) |>
    unnest(cols = c(payers, value)) |>
    ungroup() |>
    mutate(
      method = case_when(
        action == "is" ~ "==",
        action == "is not" ~ "!=",
        action == "is one of" ~ "%in%"
      ),
      condition = case_when(
        action %in% c("is", "is not") ~ glue::glue('{variable} {method} {payers}'),
        action == "is one of" ~ glue::glue('{variable} {method} c({payers})')
      )
    ) |>
    select(all_of(columns))
}

clean_icd <- \(x = components) {
  x |>
    filter(variable == "icd") |>
    mutate(
      value = str_remove_all(value, regex(r"{\(\d+\)}")),
      value = str_remove_all(value, regex(r"{\(\D+\)}")),
      value = str_replace_all(value, " ,", ","),
      value = str_replace_all(value, ", ", ",")
    ) |>
    separate_longer_delim(cols = value, delim = ",") |>
    mutate(
      wildcard = case_when(str_detect(value, "\\*") ~ 1L, .default = 0L),
      code = str_replace_all(value, fixed("*"), ""),
      code = str_replace_all(code, regex("[\\.]$"), ""),
      action = case_match(action, c("is", "has all", "is one of") ~ "is one of", .default = action),
      .id = row_number()
    ) |>
    tidytext::unnest_tokens(
      output = tokens,
      input = code,
      token = stringr::str_split,
      pattern = "",
      to_lower = FALSE
    ) |>
    mutate(tokens = glue::glue("[{tokens}]")) |>
    nest(tokens = c(tokens)) |>
    rowwise() |>
    mutate(tokens = map(tokens, ~ paste0(., collapse = ""))) |>
    unnest(cols = c(tokens)) |>
    ungroup() |>
    mutate(
      code = tokens,
      tokens = NULL,
      .id = NULL,
      commas = NULL,
      wildcard = NULL
    ) |>
    group_by(number) |>
    mutate(order = min(order)) |>
    nest(code = c(code), value = c(value)) |>
    rowwise() |>
    mutate(code = map(code, ~ paste0(., collapse = "|")),
           value = map(value, ~ paste0(., collapse = ", "))) |>
    unnest(cols = c(code, value)) |>
    ungroup() |>
    mutate(
      code = if_else(
        action == "is not",
        glue::glue("^(?!{code})"),
        glue::glue("^{code}")
      ),
      condition = glue::glue('func({variable}, "{code}")')
    ) |>
    group_by(number) |>
    nest(
      action = c(action),
      code = c(code),
      condition = c(condition),
      value = c(value)
    ) |>
    rowwise() |>
    mutate(
      action = map(action, ~ paste0(., collapse = ", ")),
      code = map(code, ~ glue::glue_collapse(., sep = ", ")),
      condition = map(condition, ~ glue::glue_collapse(., sep = " & ")),
      value = map(value, ~ paste0(., collapse = " NOT "))
    ) |>
    unnest(cols = c(action, code, condition, value)) |>
    ungroup() |>
    select(all_of(columns))
  # icd |>
  #   filter(number %in% c(607, 609, 706, 710, 741, 860, 965)) |>
  #   select(-value) |>
  #   filter(identifier %in% c("ICD:CM:115", "ICD:CM:132", "ICD:CM:50", "ICD:CM:90", "INTEG:017", "LABS:034", "OBGYN:001", "VAX:0028", "VAX:0030"))
}

clean_hcpcs <- \(x = components) {
  x |>
    filter(variable == "hcpcs") |>
    mutate(
      value = str_remove_all(value, regex(r"{\(\d+\)}")),
      value = str_remove_all(value, regex(r"{\(\D+\)}")),
      value = str_replace_all(value, " ,", ","),
      value = str_replace_all(value, ", ", ",")
    ) |>
    separate_longer_delim(cols = value, delim = ",") |>
    mutate(
      wildcard = case_when(str_detect(value, "\\*") ~ 1L, .default = 0L),
      code = str_replace_all(value, fixed("*"), ""),
      code = str_replace_all(code, regex("[\\.]$"), ""),
      action = case_match(action, c("is", "has all", "is one of") ~ "is one of", .default = action),
      .id = row_number()
    ) |>
    tidytext::unnest_tokens(
      output = tokens,
      input = code,
      token = stringr::str_split,
      pattern = "",
      to_lower = FALSE
    ) |>
    mutate(tokens = glue::glue("[{tokens}]")) |>
    nest(tokens = c(tokens)) |>
    rowwise() |>
    mutate(tokens = map(tokens, ~ paste0(., collapse = ""))) |>
    unnest(cols = c(tokens)) |>
    ungroup() |>
    mutate(
      code = tokens,
      tokens = NULL,
      .id = NULL,
      commas = NULL,
      wildcard = NULL
    ) |>
    group_by(number) |>
    mutate(order = min(order)) |>
    nest(code = c(code), value = c(value)) |>
    rowwise() |>
    mutate(code = map(code, ~ paste0(., collapse = "|")),
           value = map(value, ~ paste0(., collapse = ", "))) |>
    unnest(cols = c(code, value)) |>
    ungroup() |>
    mutate(
      code = if_else(
        action == "is not",
        glue::glue("^(?!{code})"),
        glue::glue("^{code}")
      ),
      condition = glue::glue('func({variable}, "{code}")')
    ) |>
    group_by(number) |>
    nest(
      action = c(action),
      code = c(code),
      condition = c(condition),
      value = c(value)
    ) |>
    rowwise() |>
    mutate(
      action = map(action, ~ paste0(., collapse = ", ")),
      code = map(code, ~ glue::glue_collapse(., sep = ", ")),
      condition = map(condition, ~ glue::glue_collapse(., sep = " & ")),
      value = map(value, ~ paste0(., collapse = " NOT "))
    ) |>
    unnest(cols = c(action, code, condition, value)) |>
    ungroup() |>
    select(all_of(columns))
}

clean_rendering <- \(x = components) {
  # Need to define MID-LEVEL PROVIDER credentials, taxonomy, etc.
  x |>
    filter(variable == "rendering") |>
    mutate(
      value = str_remove_all(value, "\\*"),
      value = fuimus::remove_quotes(value),
      action = "is one of",
      value = case_match(
        value,
        c(
          "MID-LEVEL PROVIDER Select all mid-levels billing incident to for BCBS TX",
          "Select all midlevel providers",
          "Select all occurances of MD providers",
          "MID-LEVEL PROVIDER Select all mid-levels billing incident to for UHC",
          "SELECT ALL MIDLEVEL PROVIDERS THAT BILL UNDER A MD FOR CIGNA (see guidelines for provider type)",
          "SELECT ALL MIDLEVEL PROVIDERS THAT BILL UNDER A MD FOR BCBS (see guidelines for provider type)",
          "SELECT ALL MIDLEVEL PROVIDERS THAT BILL UNDER A MD FOR Healthy Blue (see guidelines for provider type)",
          "MID-LEVEL PROVIDER Select all mid-levels billing incident to for AMBETTER"
        ) ~ "Mid-Level (Incident-To)",
        c("Select all occurances of MD providers") ~ "MD",
        c("Select all occurances of FNP, NP, DNP, CNS providers") ~ "FNP, NP, DNP, CNS",
        c(
          "SELECT ALL PHYSICIAN ASSISTANT PROVIDERS",
          "Select all occurances of PA providers"
        ) ~ "PA",
        "choose all Psychologists" ~ "Psychologist",
        "choose all Licensed Clinical Social Workers and Licensed Master Social Workers" ~ "LCSW, LMSW",
        "choose all Licensed Professional Counselors" ~ "LPC",
        "choose all Licensed Marital and Family Therapists" ~ "LMFT",
        "choose all Psychology Interns" ~ "Psychology Intern",
        "choose all Psychiatrists, PCNS, PMHNP, Psychologist, PLP, LCSW, LMSW, LMFT, PLMFT" ~ "Psychiatrist, Psychologist, PCNS, PMHNP, PLP, LCSW, LMSW, LMFT, PLMFT",
        "choose all LPC, PLPC providers" ~ "LPC, PLPC",
        .default = value
      )
    ) |>
    separate_longer_delim(cols = value, delim = ",") |>
    mutate(value = str_squish(value),
           refs = glue::glue('"{value}"')) |>
    nest(refs = c(refs), value = c(value)) |>
    rowwise() |>
    mutate(refs = map(refs, ~ glue::glue_collapse(., sep = ", ")),
           value = map(value, ~ paste0(., collapse = ", "))) |>
    unnest(cols = c(refs, value)) |>
    ungroup() |>
    mutate(condition = glue::glue("{variable} %in% c({refs})")) |>
    select(all_of(columns))
}

# Cleaned ####

cleaned_steps <- vctrs::vec_c(
  clean_dos(),
  clean_age(),
  clean_ndc(),
  clean_unit(),
  clean_sex(),
  clean_ub04(),
  clean_pos(),
  clean_mods(),
  clean_rev_code(),
  clean_referring(),
  clean_primary_auth(),
  clean_secondary_class(),
  clean_secondary_name(),
  clean_primary_class(),
  clean_primary_name(),
  clean_icd(),
  clean_hcpcs(),
  clean_rendering()
) |>
  dplyr::arrange(number, order)

rm(clean_age,
   clean_dos,
   clean_ndc,
   clean_unit,
   clean_sex,
   clean_ub04,
   clean_pos,
   clean_mods,
   clean_rev_code,
   clean_referring,
   clean_primary_auth,
   clean_secondary_class,
   clean_secondary_name,
   clean_primary_class,
   clean_primary_name,
   clean_icd,
   clean_hcpcs,
   clean_rendering)


# clean_combine <- cleaned_steps |>
#   left_join(
#     descriptors,
#     by = join_by(number, identifier)) |>
#   select(-category)

cleaned_steps |>
  select(-identifier) |>
  write_csv(
    here("posts/rules/data/cleaned_steps.csv"),
    )

cleaned_definitions |>
  write_csv(
    here("posts/rules/data/cleaned_definitions.csv"),
    )

descriptors |>
  select(-definition) |>
  write_csv(
    here("posts/rules/data/descriptors.csv"),
    )
