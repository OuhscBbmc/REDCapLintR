stub <- function() {
  TRUE
}

convert_dictionary_to_metadata <- function(ds_dictionary) {
  checkmate::assert_data_frame(ds_dictionary)
  ds_dictionary |>
    dplyr::select(
      "field_name"                                  = `Variable / Field Name`,
      "form_name"                                   = `Form Name`,
      "section_header"                              = `Section Header`,
      "field_type"                                  = `Field Type`,
      "field_label"                                 = `Field Label`,
      "select_choices_or_calculations"              = `Choices, Calculations, OR Slider Labels`,
      "field_note"                                  = `Field Note`,
      "text_validation_type_or_show_slider_number"  = `Text Validation Type OR Show Slider Number`,
      "text_validation_min"                         = `Text Validation Min`,
      "text_validation_max"                         = `Text Validation Max`,
      "identifier"                                  = `Identifier?`,
      "branching_logic"                             = `Branching Logic (Show field only if...)`,
      "required_field"                              = `Required Field?`,
      "custom_alignment"                            = `Custom Alignment`,
      "question_number"                             = `Question Number (surveys only)`,
      "matrix_group_name"                           = `Matrix Group Name`,
      "matrix_ranking"                              = `Matrix Ranking?`,
      "field_annotation"                            = `Field Annotation`,
  )
}

read_metadata <- function() {
  path_dictionary <- system.file("test-data/project-simple/dictionary.csv", package = "REDCapLintR", mustWork = TRUE)
  col_types <- readr::cols(
    `Variable / Field Name`                         = readr::col_character(),
    `Form Name`                                     = readr::col_character(),
    `Section Header`                                = readr::col_character(),
    `Field Type`                                    = readr::col_character(),
    `Field Label`                                   = readr::col_character(),
    `Choices, Calculations, OR Slider Labels`       = readr::col_character(),
    `Field Note`                                    = readr::col_character(),
    `Text Validation Type OR Show Slider Number`    = readr::col_character(),
    `Text Validation Min`                           = readr::col_character(),
    `Text Validation Max`                           = readr::col_character(),
    `Identifier?`                                   = readr::col_character(),
    `Branching Logic (Show field only if...)`       = readr::col_character(),
    `Required Field?`                               = readr::col_character(),
    `Custom Alignment`                              = readr::col_character(),
    `Question Number (surveys only)`                = readr::col_character(),
    `Matrix Group Name`                             = readr::col_character(),
    `Matrix Ranking?`                               = readr::col_character(),
    `Field Annotation`                              = readr::col_character()
  )
  # col_types <- readr::cols(
  #   field_name                                   = readr::col_character(),
  #   form_name                                    = readr::col_character(),
  #   section_header                               = readr::col_character(),
  #   field_type                                   = readr::col_character(),
  #   field_label                                  = readr::col_character(),
  #   select_choices_or_calculations               = readr::col_character(),
  #   field_note                                   = readr::col_character(),
  #   text_validation_type_or_show_slider_number   = readr::col_character(),
  #   text_validation_min                          = readr::col_character(),
  #   text_validation_max                          = readr::col_character(),
  #   identifier                                   = readr::col_character(),
  #   branching_logic                              = readr::col_character(),
  #   required_field                               = readr::col_character(),
  #   custom_alignment                             = readr::col_character(),
  #   question_number                              = readr::col_character(),
  #   matrix_group_name                            = readr::col_character(),
  #   matrix_ranking                               = readr::col_character(),
  #   field_annotation                             = readr::col_character()
  # )

  ds_dictionary <-
    readr::read_csv(
      file = path_dictionary,
      col_types = col_types
    )
  ds_metadata <- convert_dictionary_to_metadata(ds_dictionary)
}

# degrade_v_name <- function(d_metadata) {
#   d_metadata$field_name[1] <- paste0(d_metadata$field_name[1], "_v1")
#
#   d_metadata
# }

convert_equation <- function(equation, check_name) {
  tryCatch({
    eval(parse(text = equation))
  }, error = function(e) {
    stop("Problem parsing the equation for smell `", check_name, "`.\n", e)
  })
}
load_checks <- function() {
  path_checks <- system.file("checks.yml", package = "REDCapLintR", mustWork = TRUE)

  checkmate::assert_file_exists(path_checks, extension = c("yml", "yaml"))
  checks <- yaml::read_yaml(path_checks)

}

fill_column <- function(.d, column_name) {
  if (column_name %in% colnames(.d)) {
    # Fill missing cells with TRUEs.
    .d[[column_name]]   <- dplyr::coalesce(.d[[column_name]], TRUE)
  } else {
    # If the column doesn't exist at all, create it and fill with TRUEs.
    .d[[column_name]]   <- TRUE
  }

  .d
}

load_rules <- function(checks) {
  checkmate::assert_list(checks, any.missing = FALSE, null.ok = FALSE)

   ds_rule_all <-
    checks$rules |>
    purrr::map_df(tibble::as_tibble) #|>
    # fill_column("active") |>
    # fill_column("debug")

  # structure(
  #   list(
  #     github_file_prefix  = misc$github_file_prefix,
  #     record_id_name      = misc$record_id_name,
  #     record_id_link      = misc$record_id_link,
  #     baseline_date_name  = misc$baseline_date_name,
  #     redcap_project_id   = misc$redcap_project_id,
  #     redcap_version      = misc$redcap_version,
  #     redcap_default_arm  = misc$redcap_default_arm,
  #     redcap_codebook     = misc$redcap_codebook,
  #     redcap_record_link  = misc$redcap_record_link,
  #
  #     smells            = smells_all$smells,
  #     smells_inactive   = smells_all$smells_inactive,
  #     smell_names_md    = smells_all$smell_names_md,
  #
  #     rules             = rules_all$rules,
  #     rules_inactive    = rules_all$rules_inactive
  #   ),
  #   class = "trawler_checks_definition"
  # )
}

check_v_name <- function (d) {
  d$field_name |>
    grepl(pattern = "^.+_v\\d$", x = _, perl = TRUE)
}

load_checks2 <- function() {
  d_metadata <-
    read_metadata() #|>
    # degrade_v_name()

  check_v_name(d_metadata)

  # rules_all   <- load_rules(  checks)
}
