#' Place of Service Name to Code
#'
#' @param x `<chr>` vector of `pos` names
#'
#' @returns `<chr>` vector of `pos` codes
#'
#' @examples
#'
#' pos_name_to_code(c(
#' "TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME",
#' "TELEHEALTH PROVIDED OTHER THAN IN PT HOME",
#' "TELEHEALTH PROVIDED OTHER THAN PT HOME"))
#'
#' @autoglobal
#'
#' @export
pos_name_to_code <- function(x) {

  data.table::fcase(
    sf_detect(x, "PHARMACY"), "01",
    sf_detect(x, "TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME|TELEHEALTH PROVIDED OTHER THAN IN PT HOME|TELEHEALTH PROVIDED OTHER THAN PT HOME"), "02",
    sf_detect(x, "SCHOOL"), "03",
    sf_detect(x, "HOMELESS SHELTER"), "04",
    sf_detect(x, "INDIAN HEALTH SERVICE FREE-STANDING FACILITY|INDIAN HEALTH SERVICE - FREE-SANDNG"), "05",
    sf_detect(x, "INDIAN HEALTH SERVICE PROVIDER-BASED FACILITY|INDIAN HEALTH SERVICE - PROVIDER-BASED"), "06",
    sf_detect(x, "TRIBAL 638 FREE-STANDING FACILITY|TRIBAL 638 FREE-SANDNG FACILITY"), "07",
    sf_detect(x, "TRIBAL 638 PROVIDER-BASED FACILITY|TRIBAL 638 PROVIDER-BASED"), "08",
    sf_detect(x, "PRISON/CORRECTIONAL FACILITY"), "09",
    sf_detect(x, "TELEHEALTH PROVIDED IN PATIENT HOME"), "10",
    sf_detect(x, "OFFICE"), "11",
    sf_detect(x, "HOME"), "12",
    sf_detect(x, "ASSISTED LIVING FACILITY"), "13",
    sf_detect(x, "GROUP HOME"), "14",
    sf_detect(x, "MOBILE UNIT"), "15",
    sf_detect(x, "TEMPORARY LODGING"), "16",
    sf_detect(x, "WALK-IN RETAIL HEALTH CLINIC"), "17",
    sf_detect(x, "PLACE OF EMPLOYMENT/WORKSITE"), "18",
    sf_detect(x, "OFF CAMPUS-OUTPATIENT HOSPITAL"), "19",
    sf_detect(x, "URGENT CARE FACILITY"), "20",
    sf_detect(x, "INPATIENT HOSPITAL"), "21",
    sf_detect(x, "ON CAMPUS-OUTPATIENT HOSPITAL"), "22",
    sf_detect(x, "EMERGENCY ROOM-HOSPITAL|EMERGENCY ROOM - HOSPITAL"), "23",
    sf_detect(x, "AMBULATORY SURGICAL CENTER"), "24",
    sf_detect(x, "BIRTHING CENTER"), "25",
    sf_detect(x, "MILITARY TREATMENT FACILITY"), "26",
    sf_detect(x, "OUTREACH SITE/STREET"), "27",
    sf_detect(x, "SKILLED NURSING FACILITY"), "31",
    sf_detect(x, "NURSING FACILITY"), "32",
    sf_detect(x, "CUSTODIAL CARE FACILITY"), "33",
    sf_detect(x, "HOSPICE"), "34",
    sf_detect(x, "AMBULANCE-LAND"), "41",
    sf_detect(x, "AMBULANCE-AIR OR WATER"), "42",
    sf_detect(x, "INDEPENDENT CLINIC"), "49",
    sf_detect(x, "FEDERALLY QUALIFIED HEALTH CENTER"), "50",
    sf_detect(x, "INPATIENT PSYCHIATRIC FACILITY"), "51",
    sf_detect(x, "PSYCHIATRIC FACILITY-PARTIAL HOSPITALIZATION|PSYCHIATRIC FACILITY - PARTIAL HOSPITALIZATION"), "52",
    sf_detect(x, "COMMUNITY MENTAL HEALTH CENTER"), "53",
    sf_detect(x, "INTERMEDIATE CARE FACILITY/INDIVIDUALS WITH INTELLECTUAL DISABILITIES"), "54",
    sf_detect(x, "RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY"), "55",
    sf_detect(x, "PSYCHIATRIC RESIDENTIAL TREATMENT CENTER"), "56",
    sf_detect(x, "NON-RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY|NON-RESIDENTIAL SUBSTANCE ABUSE FACILITY"), "57",
    sf_detect(x, "NON-RESIDENTIAL OPIOID TREATMENT FACILITY"), "58",
    sf_detect(x, "MASS IMMUNIZATION CENTER"), "60",
    sf_detect(x, "COMPREHENSIVE INPATIENT REHABILITATION FACILITY"), "61",
    sf_detect(x, "COMPREHENSIVE OUTPATIENT REHABILITATION FACILITY"), "62",
    sf_detect(x, "END-STAGE RENAL DISEASE TREATMENT FACILITY|END STAGE RENAL DISEASE TREATMENT FACILITY"), "65",
    sf_detect(x, "PROGRAMS OF ALL-INCLUSIVE CARE FOR THE ELDERLY (PACE) CENTER"), "66",
    sf_detect(x, "STATE OR LOCAL PUBLIC HEALTH CLINIC"), "71",
    sf_detect(x, "RURAL HEALTH CLINIC"), "72",
    sf_detect(x, "INDEPENDENT LABORATORY"), "81",
    sf_detect(x, "OTHER PLACE OF SERVICE"),  "99",
    default = NA_character_
  )
}
