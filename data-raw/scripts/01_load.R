library(tidyverse)
library(fuimus)
library(strex)
library(here)

source(here("data-raw", "scripts", "functions.R"))

cleaned_steps <- read_csv(
  here("data-raw/data/cleaned_steps.csv"),
  col_types = cols(
    number = col_integer(),
    order = col_integer(),
    variable = col_character(),
    action = col_character(),
    value = col_character(),
    condition = col_character())) |>
  mutate(condition = glue::as_glue(condition))


cleaned_definitions <- read_csv(
  here("data-raw/data/cleaned_definitions.csv"),
  col_types = cols(
    number = col_integer(),
    definition = col_character())) |>
  mutate(
    definition = if_else(
      grepl("[A-Za-z0-9,]%AND%[A-Za-z0-9,]", definition),
      gsub("[A-Za-z0-9,]%AND%[A-Za-z0-9,]", "AND", definition),
      definition),
    definition = if_else(
      grepl("@hcpcs UB04", definition, fixed = TRUE),
      gsub("@hcpcs UB04", "@ub04", definition, fixed = TRUE),
      definition),
    definition = if_else(
      grepl("@ub04\\sis\\s\\[TRUE\\]|@ub04\\sis\\sTRUE", definition, ignore.case = TRUE),
      gsub("@ub04\\sis\\s\\[TRUE\\]|@ub04\\sis\\sTRUE", "@claim is [UB04]", definition, ignore.case = TRUE),
      definition),
    definition = if_else(
      grepl("@ub04\\sis\\s\\[FALSE\\]|@ub04\\sis\\sFALSE|@ub04\\sis\\snot\\s\\[TRUE\\]", definition, ignore.case = TRUE),
      gsub("@ub04\\sis\\s\\[FALSE\\]|@ub04\\sis\\sFALSE|@ub04\\sis\\snot\\s\\[TRUE\\]", "@claim is not [UB04]", definition, ignore.case = TRUE),
      definition)
    ) |>
  pos_to_code(definition) |>
  # pos_name_to_code(definition) |>
  mutate(
    definition = str_remove_all(definition, regex("\\(|\\)")),
    definition = str_replace_all(definition, regex("\\[\\s"), "["),
    definition = if_else(number == 480, "@primary_name is one of [BLUE CROSS BLUE SHIELD OF NC, BCBS OF NC] %AND% @pos is not [02, 10] %AND% @hcpcs is one of 99421, 99422, 99423, 98970, 98971, 98972]", definition),
    definition = if_else(number == 488, "@hcpcs is one of [9942*, G2010, G2012] %AND% @pos is one of [02, 10]", definition),
    definition = if_else(number == 490, "@primary_class is [MEDICAID] %AND% @pos is one of [02, 10] %AND% <<@mod_1 is [GT, 95, 93, CR] %OR% @mod_2 is [GT, 95, 93, CR]>>", definition),
    definition = if_else(number == 493, "@primary_class is [MEDICARE] %AND% @pos is one of [02, 10] %AND% @hcpcs is one of [0362T, 0373T, 77427, 90785, 90791, 90792, 90832, 90833, 90834, 90836, 90837, 90838, 90839, 90840, 90845, 90846, 90847, 90853, 90901, 90951, 90952, 90953, 90954, 90955, 90956, 90957, 90958, 90959, 90960, 90961, 90962, 90963, 90964, 90965, 90966, 90967, 90968, 90969, 90970, 92002, 92004, 92012, 92014, 92507, 92508, 92521, 92522, 92523, 92524, 92526, 92550, 92552, 92553, 92555, 92556, 92557, 92563, 92565, 92567, 92568, 92570, 92587, 92588, 92601, 92602, 92603, 92604, 92607, 92608, 92609, 92610, 92625, 92626, 92627, 93750, 93797, 93798, 94002, 94003, 94004, 94005, 94625, 94626, 94664, 95970, 95971, 95972, 95983, 95984, 96105, 96112, 96113, 96116, 96121, 96125, 96127, 96130, 96131, 96132, 96133, 96136, 96137, 96138, 96139, 96156, 96158, 96159, 96160, 96161, 96164, 96165, 96167, 96168, 97110, 97112, 97116, 97129, 97130, 97150, 97151, 97152, 97153, 97154, 97155, 97156, 97157, 97158, 97161, 97162, 97163, 97164, 97165, 97166, 97167, 97168, 97530, 97535, 97537, 97542, 97750, 97755, 97760, 97761, 97763, 97802, 97803, 97804, 98960, 98961, 98962, 99202, 99203, 99204, 99205, 99211, 99212, 99213, 99214, 99215, 99221, 99222, 99223, 99231, 99232, 99233, 99234, 99235, 99236, 99238, 99239, 99281, 99282, 99283, 99284, 99285, 99291, 99292, 99304, 99305, 99306, 99307, 99308, 99309, 99310, 99315, 99316, 99341, 99342, 99344, 99345, 99347, 99348, 99349, 99350, 99406, 99407, 99441, 99442, 99443, 99468, 99469, 99471, 99472, 99473, 99475, 99476, 99477, 99478, 99479, 99480, 99483, 99495, 99496, 99497, 99498, G0108, G0109, G0270, G0296, G0316, G0317, G0318, G0396, G0397, G0406, G0407, G0408, G0420, G0421, G0422, G0423, G0425, G0426, G0427, G0438, G0439, G0442, G0443, G0444, G0445, G0446, G0447, G0459, G0506, G0508, G0509, G0513, G0514, G2086, G2087, G2088, G2211, G2212, G3002, G3003, G9685]", definition),
    definition = if_else(number == 494, "@primary_class is [MEDICARE] %AND% @hcpcs is one of [0362T, 0373T, 77427, 90785, 90791, 90792, 90832, 90833, 90834, 90836, 90837, 90838, 90839, 90840, 90845, 90846, 90847, 90853, 90901, 90951, 90952, 90953, 90954, 90955, 90956, 90957, 90958, 90959, 90960, 90961, 90962, 90963, 90964, 90965, 90966, 90967, 90968, 90969, 90970, 92002, 92004, 92012, 92014, 92507, 92508, 92521, 92522, 92523, 92524, 92526, 92550, 92552, 92553, 92555, 92556, 92557, 92563, 92565, 92567, 92568, 92570, 92587, 92588, 92601, 92602, 92603, 92604, 92607, 92608, 92609, 92610, 92625, 92626, 92627, 93750, 93797, 93798, 94002, 94003, 94004, 94005, 94625, 94626, 94664, 95970, 95971, 95972, 95983, 95984, 96105, 96112, 96113, 96116, 96121, 96125, 96127, 96130, 96131, 96132, 96133, 96136, 96137, 96138, 96139, 96156, 96158, 96159, 96160, 96161, 96164, 96165, 96167, 96168, 97110, 97112, 97116, 97129, 97130, 97150, 97151, 97152, 97153, 97154, 97155, 97156, 97157, 97158, 97161, 97162, 97163, 97164, 97165, 97166, 97167, 97168, 97530, 97535, 97537, 97542, 97750, 97755, 97760, 97761, 97763, 97802, 97803, 97804, 98960, 98961, 98962, 99202, 99203, 99204, 99205, 99211, 99212, 99213, 99214, 99215, 99221, 99222, 99223, 99231, 99232, 99233, 99234, 99235, 99236, 99238, 99239, 99281, 99282, 99283, 99284, 99285, 99291, 99292, 99304, 99305, 99306, 99307, 99308, 99309, 99310, 99315, 99316, 99341, 99342, 99344, 99345, 99347, 99348, 99349, 99350, 99406, 99407, 99441, 99442, 99443, 99468, 99469, 99471, 99472, 99473, 99475, 99476, 99477, 99478, 99479, 99480, 99483, 99495, 99496, 99497, 99498, G0108, G0109, G0270, G0296, G0316, G0317, G0318, G0396, G0397, G0406, G0407, G0408, G0420, G0421, G0422, G0423, G0425, G0426, G0427, G0438, G0439, G0442, G0443, G0444, G0445, G0446, G0447, G0459, G0506, G0508, G0509, G0513, G0514, G2086, G2087, G2088, G2211, G2212, G3002, G3003, G9685] %AND% @pos is one of [02, 10] %AND% @mod_1 is not [93, 95]", definition),
    definition = if_else(number == 495, "@primary_class is one of [BCBS AL] %AND% <<@pos is one of [02, 10] %OR% @mod_1 is [95, GT, GQ]>>", definition),
    definition = if_else(number == 496, "@primary_class is one of [BCBCS] %AND% @mod_1 is [95, GT, GQ] %AND% @pos is not [02, 10] %AND% @hcpcs is one of [G2010, G2012]", definition),
    definition = if_else(number == 498, "@primary_name is [BCBS REGENCE] %AND% @pos is one of [02, 10] %AND% @mod_1 is not [GT]", definition),
    definition = if_else(number == 503, "@primary_name is [COVID19 HRSA Uninsured Fund]", definition),
    definition = if_else(number == 510, "@mod_1 is [73, 74]", definition),
    definition = if_else(number == 519, "@hcpcs is one of [001*, 002*, 003*, 004*, 005*, 006*, 007*, 008*, 009*, 01*] %AND% @mod_1 is not [AA, AD, G8, G9, GC, GE, GF, QK, QS, QX, QY, QZ] %AND% @mod_2 is not [AA, AD, G8, G9, GC, GE, GF, QK, QS, QX, QY, QZ]", definition),
    definition = if_else(number == 522, "@primary_class is one of [BCBCS] %AND% @hcpcs is [J*] %AND% @ndc is not [PRESENT]", definition),
    definition = if_else(number == 530, "@hcpcs is [G2025] %AND% @mod_1 is [CG] %AND% @mod_2 is not [CS]", definition),
    definition = if_else(number == 534, "@hcpcs is one of [0214T, 0215T, 0217T, 0218T, 0219T, 0220T, 0221T, 0222T, 0263T, 0265T, 0266T, 0269T, 0274T, 0275T, 0329T, 0330T, 0422T, 0444T, 0445T, 0506T, 0507T, 15777, 20939, 22510, 22511, 22512, 22513, 22514, 22515, 22526, 22527, 27197, 27198, 30801, 30802, 31231, 32673, 34713, 34714, 34715, 34716, 34717, 36221, 34812, 34820, 34833, 34834, 35572, 50300, 50540, 54420, 54430, 55200, 55250, 55300, 58575, 58600, 58605, 58700, 58720, 58800, 58805, 58900, 58920, 58925, 58940, 58943, 61000, 61001, 61253, 63035, 63043, 63044, 63045, 63046, 63047, 63048, 64421, 64480, 64484, 64491, 64492, 64494, 64495, 64634, 64636, 76514, 92025, 92081, 92082, 92083, 92132, 92133, 92134, 92145, 92201, 92202, 92227, 92228, 92229, 92235, 92240, 92242, 95870, C7501, C7502, C7504, C7505, C9771, E0675, G0279, G0412, G0413, G0414, G0415, S2342] %AND% <<@mod_1 is [50, RT, LT] %OR% @mod_2 is [50, RT, LT]>>", definition),
    definition = if_else(number == 613, "@hcpcs is [91300] %AND% @hcpcs is not [0001A, 0002A, 0003A, 0004A] %OR% @age is less than [12 YRS]", definition),
    definition = if_else(number == 744, "@hcpcs is one of [9920*, 9921*] %AND% @pos is not [02, 03, 05, 06, 07, 08, 10, 11, 15, 16, 17, 19, 20, 22, 24, 25, 26, 49, 50, 52, 53, 57, 62, 65, 71, 72, 99]", definition)
  )

descriptors <- read_csv(
  here("data-raw/data/descriptors.csv"),
  col_types = cols(
    number = col_integer(),
    identifier = col_character(),
    category = col_character(),
    rationale = col_character()
  )
)
