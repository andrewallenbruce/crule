r"[nav\to\file\path.ext]"

## Years/Months to Days Functions ####
lubridate::as.duration(lubridate::years(21)) / lubridate::ddays(1)

Sys.Date() - lubridate::as.duration(lubridate::years(21)) / lubridate::ddays(1)

fuimus::construct_regex(c('90476', '90477', '905', '906', '9071', '9072', '9073', '9074', '9075', '913'))

fuimus::construct_regex(hcpcs)

hcpcs <- northstar::search_descriptions() |>
  dplyr::select(hcpcs = hcpcs_code) |>
  dplyr::distinct(hcpcs)

hcpcs |>
  dplyr::mutate(
    char_1 = substr(hcpcs, 1, 1),
    char_12 = substr(hcpcs, 1, 2),
    char_123 = substr(hcpcs, 1, 3)) |>
  dplyr::count(char_1, char_12, char_123, sort = TRUE) |>
  print(n = 300)

c(
  "^[9][0][4][7][67]$"     = '^9047[67]$|^90[56]\\d+$|^907[1-5]\\d+$|^913[01]\\d+$',
  "^[9][0][56][1-9][0-9]$" = '^90[56]\\d+$',
  "^[9][0][7][1-5][02-9]$" = '^907[1-5]\\d+$',
  "^[9][1][3][01][0-9]$"   = '^913[01]\\d+$'
  )

x <- c(hcpcs = c('9071', '9072', '9073', '9074', '9075'))



collapse::vlengths(x, use.names = FALSE)


hcpcs <- stringfish::convert_to_sf(hcpcs$hcpcs)

pattern1 <- "^9047[67]$|^90[56]\\d+$|^907[1-5]\\d+$|^913[01]\\d+$"
pattern2 <- "^9047[67]$|^90[56][0-9A-Z]{2}$|^907[1-5][0-9A-Z]{1}$|^913[01][0-9A-Z]{1}$"

hcpcs[which(stringfish::sf_grepl(hcpcs, "^9047[67]$|^90[56]\\d+$|^907[1-5]\\d+$|^913[01]\\d+$"))]

hcpcs[which(stringfish::sf_grepl(hcpcs, pattern1))]
hcpcs[which(stringfish::sf_grepl(hcpcs, pattern2))]

# regex method
vctrs::vec_slice(
  hcpcs,
  stringfish::sf_grepl(
    hcpcs$hcpcs,
    "^9047[67]$|^90[56]\\d+$|^907[1-5]\\d+$|^913[01]\\d+$"))


# substring method
x <- "S"
x <- c('904', '905', '906', '907', '913')

bench::mark(
  iterations = 1000,
  substr = vctrs::vec_slice(hcpcs, stringfish::sf_substr(hcpcs$hcpcs, 1, collapse::fmin(stringfish::sf_nchar(x))) %in% x),
  regex = vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^(90[4567][0-9]{2}|913[0-9]{2})$"))
)
# Regex is the clear winner
#  expression  min     median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
#  1 substr    1.39ms  1.76ms  542.     444.6KB    5.47    990    10   1.83s
#  2 regex     1.58ms  1.82ms  519.     75.3KB     0.520   999     1   1.92s


"<[^>]+>" # Find any HTML tag
"https?:\/\/[\w\.\/\-?=&%,]+" # Find any URL
"https?://[^\\s]+"

library(tidytext)
descriptors |>
  select(id, description) |>
  unnest_tokens(word, description, to_lower = FALSE, token = "ptb") |>
  mutate(word = if_else(id == 1 & word %in% "*****", "43760", word))


brack_txt <- "This is [some text] between [brackets]"
brack_reg <- r"{\[(.*?)\]}"

paren_txt <- "This is (some text) between (brackets)"
paren_reg <- r"{\(.*?\)}"

nested_txt <- "This (has brackets) and [square brackets] mixed (together [nested])"
nested_reg <- r"{\[.*?\]|\(.*?\)}"

keyword_txt <- "@hcpcs @pos"
keyword_reg <- r"{@(\w+)}"

stringr::str_extract_all(brack_txt, brack_reg)[[1]]
stringr::str_extract_all(paren_txt, paren_reg)[[1]]
stringr::str_extract_all(nested_txt, nested_reg)[[1]]
stringr::str_extract_all(keyword_txt, keyword_reg)[[1]]

gsub(paren_reg, "[XXXXX]", paren_txt)

glob2rx("J*")

cat(c(
  "@hcpcs@ is not [9200*, 92012, 92014]
<OR>
@hcpcs@ is one of ^[0-8A-CEGHJ-MP-R]|^[9][0134-8]|^[9][2][1-9]|^[9][2][0][26-8]|^[9][2][0][1][589]
<AND>
@hcpcs@ is not ^[^9][^9][0-9]"
))

uniq_nona <- \(x) collapse::funique(collapse::na_rm(x))

x <- c(
  "0",
  "1",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "A",
  "B",
  "C",
  "E",
  "G",
  "H",
  "J",
  "K",
  "L",
  "M",
  "P",
  "Q",
  "R",
  "90",
  "91",
  "93",
  "94",
  "95",
  "96",
  "97",
  "98",
  "921",
  "922",
  "923",
  "924",
  "925",
  "926",
  "927",
  "928",
  "929",
  "9202",
  "9206",
  "9207",
  "9208",
  "92015",
  "92018",
  "92019"
)

glue::as_glue(x)

hcpcs <- northstar::search_descriptions()$hcpcscode |>
  dplyr::select(hcpcs = hcpcs_code) |>
  dplyr::distinct(hcpcs)

vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^55[23][05]0$"))

# "^([012345678ABCEGHJKLMPQR][0-9A-Z]{4,4})$|^(9[01345678][0-9A-Z]{3,3})$|^(92[123456789][0-9A-Z]{2,2})$|^(920[2678][0-9A-Z]{1,1})|^(9201[589])$"

vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^[0-8A-CEGHJ-MP-R][0-9A-Z]{4}$")) # 16874
vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^[9][013-8][0-9A-Z]{3}$")) # 802
vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^[9][2][1-9][0-9A-Z]{2}$")) # 172
vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^[9][2][0][26-8][0-9A-Z]{1}$")) # 10
vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^[9][2][0][1][589]$")) # 3
sum(16874, 802, 172, 10, 3) # 17861

bench::mark(
  iterations = 10000,
  regular = vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^([012345678ABCEGHJKLMPQR][0-9A-Z]{4,4})$|^(9[01345678][0-9A-Z]{3,3})$|^(92[123456789][0-9A-Z]{2,2})$|^(920[2678][0-9A-Z]{1,1})|^(9201[589])$")),
  hyphen_1dig_post = vctrs::vec_slice(hcpcs, stringfish::sf_grepl(hcpcs$hcpcs, "^([0-8A-CEGHJ-MP-R][0-9A-Z]{4})$|^(9[013-8][0-9A-Z]{3})$|^(92[1-9][0-9A-Z]{2})$|^(920[26-8][0-9A-Z]{1})|^(9201[589])$"))
)

#   expression      min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# 1 regular      3.29ms 3.81ms      257.     283KB     1.42  9945    55      38.7s
# 2 hyphen       3.23ms 3.77ms      258.     283KB     1.46  9944    56      38.5s
# 2 hyp_1d_post  3.13ms 3.57ms      279.     283KB     1.57  9944    56      35.6s



# data.frame(
#   x = x,
#   ln = ln
# ) |>
#   group_by(ln) |>
#   nest() |>
#   collapse::rsplit(~ ln)

# "^[0-8A-CEGHJ-MP-R]|^[9][0134-8]|^[9][2][1-9]|^[9][2][0][26-8]|^[9][2][0][1][589]"




## LOGICAL TESTING ####

c(TRUE, FALSE, TRUE) & c(TRUE, TRUE, FALSE)

c(1, 0, 1) & c(1, 1, 0)

c(TRUE, FALSE, TRUE) | c(TRUE, TRUE, FALSE)

c(1, 0, 1) | c(1, 1, 0)


head_tail(rules_new, n = 10, by = c("var", "action")) |>
  gt() |>
  cols_align(align = "left") |>
  opt_table_font(font = google_font(name = "Roboto Condensed")) |>
  opt_all_caps() |>
  tab_style(
    style = cell_text(
      align = 'left',
      weight = "bold",
      font = google_font(name = "Roboto Mono")),
    locations = cells_body(columns = c(var, action, value))) |>
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)

rules_new |>
  filter(!is.na(var)) |>
  group_by(var) |>
  count(action) |>
  ungroup() |>
  arrange(var, desc(n), action) |>
  gt(groupname_col = "var", row_group_as_column = TRUE) |>
  cols_align(align = "left") |>
  opt_table_font(
    font = google_font(name = "Roboto Condensed"),
    weight = "bold"
  ) |>
  opt_all_caps() |>
  tab_style(
    style = cell_text(
      align = 'left',
      weight = "normal",
      font = google_font(name = "Roboto Mono")),
    locations = cells_body(columns = c(var, action))) |>
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)


# clean_combine |>
#   pivot_longer(
#     cols = c(definition, nested),
#     names_to = "variable",
#     values_to = "definition") |>
#   filter(!is.na(definition)) |>
#   select(number, nested) |>
#   print(n = 200)
#   separate_longer_delim(cols = definition, delim = " ( ") |>
#   separate_longer_delim(cols = definition, delim = " ) ") |>
#   separate_longer_delim(cols = definition, delim = " AND ") |>
#   mutate(parentheses = str_extract_all(definition, regex("\\s*\\(\\s*\\d+\\s*\\)\\s*")),
#     OR = str_detect(definition, fixed(" OR ")),
#          PARA = str_detect(definition, regex("\\(\\s|\\s\\)")),
#          definition = if_else(PARA, str_remove_all(definition, fixed(" )")), definition),
#          definition = if_else(PARA|OR, glue_chr("<<--{definition}-->>"), definition),
#          OR = NULL,
#          PARA = NULL,
#          definition = str_remove_all(definition, regex(r"{\(\d+\)}")),
#          definition = str_remove_all(definition, regex(r"{\(\D+\)}")),
#          definition = str_replace_all(definition, " ; ", ", "),
#          definition = str_replace_all(definition, ";", ", "),
#          definition = str_replace_all(definition, fixed(" ]"), "]"),
#          definition = str_replace_all(definition, " ] ", "]"),
#          definition = str_replace_all(definition, " , ", ", "),
#          definition = str_replace_all(definition, " ,", ", "),
#          definition = str_squish(definition)
#          ) |>
#   slice(1000:2000) |>
#   print(n = 100)

## REGEX TESTING ####
c(
  "^[9][0][4][7][67]$"     = '9047(6|7)',
  "^[9][0][56][1-9][0-9]$" = '905*|906*',
  "^[9][0][7][1-5][02-9]$" = '907(1|2|3|4|5)',
  "^[9][1][3][01][0-9]$"   = '913'
)

rules_new |>
  filter(wc == 1,
         var == "hcpcs") |>
  mutate(value = str_remove_all(value, "\\*"),
         chars = 5 - str_length(value),
         .after = value) |>
  mutate(
    regex = case_when(
      chars == 0 ~ glue_chr("^<<value>>$", .open = "<<", .close = ">>"),
      chars == 1 ~ glue_chr("^<<value>>[0-9]$", .open = "<<", .close = ">>"),
      chars > 1 ~ glue_chr("^<<value>>[0-9]{<<chars>>}$", .open = "<<", .close = ">>")),
    regex = case_when(neg == 1L ~ glue_chr('!{regex}'), .default = regex),
    .after = regex
  ) |>
  head_tail(n = 10, by = c("var", "action")) |>
  gt() |>
  cols_align(align = "left") |>
  opt_table_font(font = google_font(name = "Roboto Condensed")) |>
  opt_all_caps() |>
  tab_style(
    style = cell_text(
      align = 'left',
      weight = "bold",
      font = google_font(name = "Roboto Mono")),
    locations = cells_body(columns = c(regex))) |>
  tab_options(table.width = pct(100),
              quarto.disable_processing = TRUE)



northstar::search_descriptions() |>
  distinct(hcpcs_code, .keep_all = TRUE) |>
  # dplyr::mutate(not_hcpcs = !grepl("^99[0-9]{3}$", hcpcs_code)) |>
  # filter(!grepl("^99[0-9]{3,3}$", hcpcs_code)) |>
  # dplyr::mutate(has_hcpcs = grepl("^J[0-9]{4}$", hcpcs_code)) |>
  filter(str_detect(hcpcs_code, regex("^[J][0-9A-Z]{4}$"))) |>
  filter(str_detect(hcpcs_code, regex("^(?!5405)(?<![0-9]{1})$"))) |>
  print(n = 200)

# Match strings that don't start with "99" and don't end with 3 digits
pattern <- "^(?!99).*(?<![0-9]{3})$"

# Negation pattern
"^(?!9938[0-9]{1}$)"

# Begins with 0, ends with digit
"^0.*\\d$"

# Begins with 0, ends with letter
"^0.*[A-Z]$"

# Match strings that don't start with "a" and don't end with "z"
pattern <- "^(?!a).*[^z]$"
grep(pattern, c("apple", "banana", "cherry"), value = TRUE)
# Output: "banana" "cherry"

stringr::str_detect("99202", stringr::regex("^[992]{3}.*"))

grep("^[992]{3}.*", "99202", value = TRUE, invert = TRUE)


pattern = dplyr::case_when(
  chars == 0 & negation == FALSE ~ glue::glue("^<value>$", .open = "<", .close = ">"),
  chars > 0  & negation == FALSE ~ glue::glue("^<value>[0-9]{<chars>}$", .open = "<", .close = ">"),
  chars == 0 & negation == TRUE ~ glue::glue("^(?!<value>)$", .open = "<", .close = ">"),
  chars > 0  & negation == TRUE ~ glue::glue("^(?!<<value>>)(?<![0-9]{<<chars>>})$", .open = "<<", .close = ">>")
)

# - `rule` class
# - `index`: Unique Rule Number
# - `identifier`: Unique Alphanumeric Identifier
# - `category`: High-Level Classification
# - `definition`: Human Readable Description of Rule Steps
# - `rationale`: Reason for Implementation
# - `source`: URLs, References, or Documentation
# - **`steps`**: `<list>` of Steps to Evaluate

# - `step` class
# - `type`:
#   - `qualifier`: All steps in the `<rule>` but the last (`steps[1:length(steps) - 1]`). Identify claims that meet the criteria for inspection. Claim is skipped if not met.
# - `terminator`: Final step, the result of which indicates a pass or fail. Claim is skipped if it passes. If it fails, the full claim information should be returned, along with the `rationale` for the failure
# - `order`: Order in which steps are validated. Certain steps must be ahead of others:
#   - Dates should always be first
# - `<dos>` should always be first if present, unless patient `<age>` is required
# - If patient `<age>` is required, it should be first and, ideally, calculated as the number of days from `<dob>` to `<dos>`
# - **`variable`**: element of claim to evaluate
# - `value`: valid variable value, expected state
# - `operation`: could define different methods for a single variable
# - regex: matching patterns of lists of HCPCS, wildcards, etc.
# - calculation: days elapsed between date of birth and date of service
# - comparison: check if a value is greater than or less than another value
# - equality: check if a value is equal to another value
# - presence: check if a value is present, e.g. `!is.na(x)`
# - `condition`: functional code to call based on method selected
# - `alert`: message to display if final `terminator` condition is not met


re2r::show_regex("#?([a-f0-9]{6}|[a-f0-9]{3})")


# > last|> dplyr::count(group_id, a2)
# A tibble: 13 × 3
#   group_id a2        n
#   <chr>    <chr> <int>
#   1 2        0         1
#   2 2        2         1
#   3 2        7         1
#   4 3        0         1
#   5 3        1         1
#   6 3        2         1
#   7 3        4         1
#   8 3        5         1
#   9 3        6         1
#  10 9        2         1
#  11 9        5         1
#  12 C        7         1
#  13 C        9         1

# > last|> dplyr::count(group_id, name = "g")
# A tibble: 4 × 2
#   group_id     g
#   <chr>    <int>
#   1 2            3
#   2 3            6
#   3 9            2
#   4 C            2



c(
  # singles
  "^15777$",
  "^76514$",
  "^E0675$",
  "^S2342$",

  "^021[4-9]T$",
  "^022[0-2]T$",
  "^026[3569]T$",
  "^027[45]T$",


  "^0329T$",
  "^0330T$",
  "^0422T$",

  "^044[45]T$",
  "^050[67]T$",

  "^20939$",

  "^2251[0-5]$",
  "^2252[67]$",
  "^2719[78]$",

  "^3080[12]$",

  "^31231$",
  "^32673$", #36
  NULL
)

