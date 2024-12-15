dirty_definition <- descriptors |>
  select(number, definition) |>
  distinct(number, .keep_all = TRUE) |>
  mutate(definition = str_remove_all(definition, regex("\\s*\\(\\s*\\d*\\s*\\)\\s*")),
         definition = str_remove_all(definition, regex("\\(Presence\\)\\s*")),
         nest1 = str_extract_all(definition, regex("\\(\\s.*?\\s\\)")),
         nest2 = str_extract_all(definition, regex("\\(\\(.*\\)\\)")),
         definition = str_replace_all(definition, regex("\\(\\s.*?\\s\\)"), "<<NESTED>>"),
         definition = str_replace_all(definition, regex("\\(\\(.*\\)\\)"), "<<NESTED>>")) |>
  unnest(c(nest1, nest2), keep_empty = TRUE)

clean_nested <- dirty_definition |>
  select(number, nest1, nest2) |>
  pivot_longer(
    cols = c(nest1, nest2),
    names_to = "variable",
    values_to = "nested") |>
  filter(!is.na(nested)) |>
  select(number, nested) |>
  mutate(nested = str_remove_all(nested, regex("\\(\\s*|\\s*\\)|\\((|\\))")),
         nested = str_replace_all(nested, "HOMEAND", "HOME] AND"),
         nested = str_replace_all(nested, "CPT", "@hcpcs"),
         nested = str_replace_all(nested, "DX", "@icd"),
         nested = str_remove_all(nested, "Code "),
         nested = str_replace_all(nested, "@hcpcs Mod", "@mod_"),
         nested = str_replace_all(nested, "@hcpcs Rev", "@rev"),
         nested = str_replace_all(nested, "Place of [Ss]ervice", "@pos"),
         nested = str_replace_all(nested, "Patient Age", "@age"),
         nested = str_replace_all(nested, "@hcpcs Units", "@unit"),
         nested = str_replace_all(nested, "Location", "@location"),
         nested = str_replace_all(nested, "Primary Insurance", "@primary_name"),
         nested = str_replace_all(nested, "Primary Insurance Class", "@primary_class"),
         nested = str_replace_all(nested, "Secondary Insurance", "@secondary_name"),
         nested = str_replace_all(nested, "Secondary Insurance Class", "@secondary_class"),
         nested = str_replace_all(nested, "false", "FALSE"),
         nested = str_replace_all(nested, "true", "TRUE"),
         nested = str_replace_all(nested, ", months", " MONS"),
         nested = str_replace_all(nested, ", years", " YRS"),
         nested = str_replace_all(nested, ", days", " DAYS"),
         nested = str_replace_all(nested, "younger", "less"),
         nested = str_replace_all(nested, "older", "greater"),
         nested = str_replace_all(nested, " ]", "]"),
         nested = str_replace_all(nested, "is \\[[Nn]ot [Pp]resent\\]", "is not [PRESENT]"),
         nested = str_replace_all(nested, ";", ","),
         nested = case_when(
           str_detect(nested, "AND") ~ str_replace_all(nested, "AND", "%AND%"),
           str_detect(nested, "OR") ~ str_replace_all(nested, "OR", "%OR%"), .default = nested),
         nested = str_replace_all(
           nested,
           "@mod_1 is not 95, GT, GQ OR @mod_1 is 95, GT, GQ %AND% @pos is notHome",
           "@mod_1 is not [95, GT, GQ] %OR% (@mod_1 is [95, GT, GQ] %AND% @pos is not [HOME])"),
         nested = str_replace_all(
           nested,
           "TELEHEALTH PROVIDED OTHER THAN PT HOME, TELEHEALTH PROVIDED IN PATIENT HOME%OR%",
           "[TELEHEALTH PROVIDED OTHER THAN PT HOME, TELEHEALTH PROVIDED IN PATIENT HOME] %OR%"),
         nested = if_else(number == 618, "(@hcpcs is [913*] %AND% @hcpcs is not [90480]) %OR% (@hcpcs is [90480] %AND% @hcpcs is not [913*])", nested),
         nested = if_else(number == 314, "(@pos is one of [02, 10] %AND% @mod_1 is not [95, GT, GQ]) %OR% (@mod_1 is [95, GT, GQ] %AND% @pos is not [12])", nested),
         nested = if_else(number == 210, "(@hcpcs is not [9200*, 92012, 92014]) %OR% (@hcpcs is one of [0*, 1*, 2*, 3*, 4*, 5*, 6*, 7*, 8*, 90*, 91*, 92015, 92018, 92019, 9202*, 9206*, 9207*, 9208*, 921*, 922*, 923*, 924*, 925*, 926*, 927*, 928*, 929*, 93*, 94*, 95*, 96*, 97*, 98*, A*, B*, C*, E*, G*, H*, J*, K*, L*, M*, P*, Q*, R*] %AND% @hcpcs is not [99*])", nested)
  ) |>
  pos_name_to_code(nested) |>
  mutate(.id = row_number(),
         .by = number,
         .after = number) |>
  pivot_wider(
    names_from = .id,
    values_from = nested) |>
  janitor::clean_names() |>
  mutate(
    x1 = if_else(!is.na(x1), glue_chr("<<{x1}>>"), x1),
    x2 = if_else(!is.na(x2), glue_chr("<<{x2}>>"), x2),
    x3 = if_else(!is.na(x3), glue_chr("<<{x3}>>"), x3)
  ) |>
  combine(name = x1, columns = c('x1', 'x2'), sep = " %AND% ") |>
  combine(name = x1, columns = c('x1', 'x3'), sep = " %AND% ")

# 81 - @primary_name Authorization
clean_def <- dirty_definition |>
  select(number, definition) |>
  distinct(number, .keep_all = TRUE) |>
  mutate(definition = str_replace_all(definition, "CPT", "@hcpcs"),
         definition = str_replace_all(definition, "DX", "@icd"),
         definition =  str_remove_all(definition, "Code "),
         definition =  str_remove_all(definition, "\\s\\)"),
         definition =  str_remove_all(definition, fixed("GROUP")), # 197
         definition = str_replace_all(definition, "@hcpcs Mod", "@mod_"),
         definition = str_replace_all(definition, "@hcpcs Rev", "@rev"),
         definition = str_replace_all(definition, "@hcpcs NDC", "@ndc"),
         definition = str_replace_all(definition, "@hcpcs UB04", "@ub04"),
         definition = str_replace_all(definition, "Place of [Ss]ervice", "@pos"),
         definition = str_replace_all(definition, "Encounter Date of Service", "@dos"),
         definition = str_replace_all(definition, "Patient Age", "@age"),
         definition = str_replace_all(definition, "Patient Sex", "@sex"),
         definition = str_replace_all(definition, "@hcpcs Units", "@unit"),
         definition = str_replace_all(definition, "Location", "@location"),
         definition = str_replace_all(definition, "Referring Provider", "@referring"),
         definition = str_replace_all(definition, "Rendering Provider", "@rendering"),
         definition = str_replace_all(definition, "Primary Insurance Class", "@primary_class"),
         definition = str_replace_all(definition, "Secondary Insurance Class", "@secondary_class"),
         definition = str_replace_all(definition, "Primary Insurance", "@primary_name"),
         definition = str_replace_all(definition, "Secondary Insurance", "@secondary_name"),
         definition = str_replace_all(definition, "@primary_name Authorization", "@primary_auth"),
         definition = str_replace_all(definition, "false", "FALSE"),
         definition = str_replace_all(definition, "true", "TRUE"),
         definition = str_replace_all(definition, ", months", " MONS"),
         definition = str_replace_all(definition, ", years", " YRS"),
         definition = str_replace_all(definition, ", days", " DAYS"),
         definition = str_replace_all(definition, "younger", "less"),
         definition = str_replace_all(definition, "older", "greater"),
         definition = str_replace_all(definition, " ]", "]"),
         definition = str_replace_all(definition, "is \\[[Nn]ot [Pp]resent\\]", "is not [PRESENT]"),
         definition = str_replace_all(definition, "Present", "PRESENT"),
         definition = str_replace_all(definition, ";", ","),
         definition = str_replace_all(definition, "\\[Q4017, Q4018, Q4019, Q4020, L\\*", "[Q4017, Q4018, Q4019, Q4020, L*]"),
         definition = str_replace_all(definition, "\\[Q4021, Q4022, Q4023, Q4024, L\\*", "[Q4021, Q4022, Q4023, Q4024, L*]"),
         definition = str_replace_all(definition, "TELEHEALTH PROVIDED IN PATIENT HOMEAND", "TELEHEALTH PROVIDED IN PATIENT HOME] AND"),
         definition = str_replace_all(definition, "TELEHEALTH PROVIDED OTHER THAN IN PT HOME", "TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME"),
         definition = str_replace_all(definition, "TELEHEALTH PROVIDED OTHER THAN PT HOME", "\\[TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME"),
         definition = str_replace_all(definition, "INTERMEDIATE CARE FACILITY / MENTALLY RETARDED", "INTERMEDIATE CARE FACILITY/INDIVIDUALS WITH INTELLECTUAL DISABILITIES"),
         definition = str_replace_all(definition, "PSYCHIATRIC RESIDENTIAL TREATMENT FACILITY", "PSYCHIATRIC RESIDENTIAL TREATMENT CENTER"),
         definition = case_when(
           str_detect(definition, "AND") ~ str_replace_all(definition, "AND", "%AND%"),
           str_detect(definition, "OR") ~ str_replace_all(definition, "OR", "%OR%"), .default = definition),
         definition = if_else(number == 131, "@icd is [H72.00]", definition),
         definition = if_else(number == 127, "@icd has all [M17.11, M17.12]", definition),
         definition = if_else(number == 34, "@primary_class is [MEDICAID] %AND% @hcpcs is one of [1*, 2*, 3*, 4*, 5*, 6*] %AND% @mod_1 is not [AG]", definition),
         definition = if_else(number == 3, "@primary_name is [BCBS] %AND% @ndc is not [PRESENT] %AND% @hcpcs is one of [90476, 90477, 905*, 906*, 9071*, 9072*, 9073*, 9074*, 9075*, 913*]", definition),
         definition = if_else(number == 4, "@primary_name is [BCBS] %AND% @ndc is not [PRESENT] %AND% @hcpcs is one of [90476, 90477, 905*, 906*, 9071*, 9072*, 9073*, 9074*, 9075*, 913*]", definition),
  ) |>
  pos_name_to_code(definition)

clean_def <- clean_def |>
  left_join(clean_nested, by = join_by(number)) |>
  mutate(count = str_count(definition, fixed("<<NESTED>>")))

clean_def_eq2 <- clean_def |>
  filter(count == 2) |>
  separate_longer_delim(x1, delim = " %AND% <<") |>
  mutate(x1 = str_replace(x1, "^@mod_1", "<<@mod_1"),
         x1 = str_replace(x1, "^@hcpcs", "<<@hcpcs"))

clean_def_eq3 <- clean_def |>
  filter(count == 3) |>
  separate_longer_delim(x1, delim = " %AND% <<") |>
  mutate(x1 = str_replace(x1, "^@hcpcs", "<<@hcpcs")) |>
  pull(x1)

clean_def1 <- clean_def_eq2 |>
  slice(1, .by = number) |>
  mutate(definition = str_replace(definition, "<<NESTED>>", x1)) |>
  select(number, definition)

clean_def2 <- clean_def_eq2 |>
  slice(2, .by = number) |>
  select(number, x1)

clean_gt1 <- clean_def1 |>
  left_join(clean_def2, by = join_by(number)) |>
  mutate(definition = str_replace(definition, "<<NESTED>>", x1)) |>
  select(number, definition) |>
  add_row(clean_def |> filter(count == 3) |> select(number, definition)) |>
  mutate(
    definition = str_replace(definition, "<<NESTED>>", clean_def_eq3[1]),
    definition = str_replace(definition, "<<NESTED>>", clean_def_eq3[2]),
    definition = str_replace(definition, "<<NESTED>>", clean_def_eq3[3])
  )

cleaned_definitions <- vctrs::vec_rbind(
  clean_def |>
    filter(count == 1) |>
    mutate(definition = str_replace(definition, "<<NESTED>>", x1)) |>
    select(number, definition),
  clean_def |>
    filter(count == 0) |>
    select(number, definition),
  clean_gt1
) |>
  dplyr::arrange(number)
