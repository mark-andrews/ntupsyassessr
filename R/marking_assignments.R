#' Get a data-frame with details of all assessment Dropbox submissions
#'
#' Assuming that all the contents from NOW's dropbox have been saved in a
#' directory, then return a data frame with the submitting student's ID, the
#' filename and md5sum hash and filename extension of the most recent submission
#' by each student, or optionally of all submissions by each subject.
#'
#' @param dropbox_directory The file path to the directory with all the
#'   submissions to a assessment Dropbox
#' @param most_recent_submission Logical. Should we return only the most recent
#'   submission by each student (default) or all submissions?
#'
#' @return A data-frame with one row per each submission, and columns for
#'   student ID, filename, md5sum hash, and filename extension.
#' @export
get_dropbox_submissions <- function(dropbox_directory, most_recent_submission = TRUE){

  submitted_report_filename_pattern <- '([0-9]{5,6}-[0-9]{6}) - ([NT][01][0-9]{6}) - (.*[A-Za-z])- ([^-]*) - (.*)'

  # From the filename, return
  parse_dropbox_fname <- function(fname){
    the_match <- stringr::str_match(fname, submitted_report_filename_pattern)
    list(id = the_match[1, 3],
         student = the_match[1, 4],
         tstamp = the_match[1, 5]
    )
  }

  # Return timestamp as a lubridate object
  parse_tstamp <- function(tstamp){
    stringr::str_replace(tstamp,
                "(.*), ([0-9]{4}) ([12]*[0-9])([0-5][0-9]) (.*)",
                "\\1 \\2, \\3:\\4 \\5") %>%
      lubridate::parse_date_time("d m y, H:M %p")
  }

  tmp_df <- fs::dir_ls(path = dropbox_directory) %>%
    purrr::map_chr(tools::md5sum) %>%
    tibble::enframe(name = 'path', value = 'hash') %>%
    dplyr::mutate(fname = fs::path_file(path)) %>%
    dplyr::mutate(match = purrr::map(fname, parse_dropbox_fname)) %>%
    tidyr::unnest_wider(match) %>%
    dplyr::mutate(tstamp = parse_tstamp(tstamp))

  if (most_recent_submission){

    get_most_recent_submission <- function(submissions){
      dplyr::slice(dplyr::arrange(submissions, dply::desc(tstamp)), 1)
    }

    tmp_df <- dplyr::group_by(tmp_df, id) %>%
      tidyr::nest() %>%
      dplyr::mutate(newdata = purrr::map(data, get_most_recent_submission)) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(newdata) %>%
      dplyr::ungroup()
  }

  dplyr::mutate(tmp_df, ext = tools::file_ext(fname)) %>%
    dplyr::select(id, fname, hash, ext)

}

#' Get classlist of all students and their labs (seminars)
#'
#' @param all_seminars_fname A tsv file with seminar list of labs
#'
#' @return A data frame with id, student, seminar (1_SH_LAB, etc)
#' @export
get_seminar_list <- function(all_seminars_fname){
  readr::read_tsv(all_seminars_fname) %>%
    na.omit() %>%
    dplyr::rename(id = `Student ID`,
           seminar = `Group Code`) %>%
    dplyr::select(id, student = Name, seminar) %>%
    dplyr::filter(stringr::str_detect(seminar, '_LAB'))
}


#' For each RM2 seminar group, get the markers and their emails
#'
#' @param staff_grid_fname A tsv file with staff_1, staff_2, seminar name, HPL
#' @param staff_emails A tsv file with staff name and email
#'
#' @return A long data frame
#' @export
get_rm2_marking_staff <- function(staff_grid_fname, staff_emails){

  readr::read_tsv(staff_grid_fname) %>%
    dplyr::rename(seminar = Lab) %>%
    dplyr::relocate(seminar, .after = everything()) %>%
    tidyr::pivot_longer(cols = -seminar,
                 names_to = 'staff_type',
                 values_to = 'staff_name') %>%
    dplyr::mutate(is_hpl = staff_type == 'HPL') %>%
    dplyr::select(seminar, staff_name, is_hpl) %>%
    dplyr::left_join(
      readr::read_tsv(staff_emails, col_names = c('name','email')) %>%
        tidyr::separate(name, into = c('lastname', 'firstname'), sep = ', ', remove = F) %>%
        tidyr::unite('staff_name', firstname, lastname, sep = ' ', remove = F),
      by = 'staff_name'
    ) %>%
    dplyr::select(seminar, marker = name, email, is_hpl)

}


#' Marking assignments
#'
#' @param submissions Data frame of reports to be marked
#' @param markers Data frame of markers
#'
#' @return A data frame with marking assignments
#' @export
nonhpl_marking_assignments <- function(submissions, markers, use_hpls = FALSE){

  if (!use_hpls) {
    markers <- dplyr::filter(markers, !is_hpl)
  }

  submissions %>%
    dplyr::group_by(id) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = map(data,
        ~{
          dplyr::left_join(.,
                    markers %>%
                      dplyr::select(seminar, marker, marker_email = email),
                    by = 'seminar')
        })
    ) %>%
    tidyr::unnest(col = data) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(seminar) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = map(data, ~{
      dplyr::mutate(., is_marking = rep_len(c(T,F,F,T), n())) %>%
        dplyr::filter(is_marking) %>%
        dplyr::select(-is_marking)
    })) %>%
    dplyr::unnest(cols = data) %>%
    dplyr::ungroup()
}


#' Check marking assignments
#'
#' Some simple sanity checks of marking assignments. For example, each student
#' that submitted a report has had their report assigned to one marker, and only
#' one marker.
#'
#' @param submissions Data-frame of reports to be marked
#' @param marking_assignments Data-frame of marking assignments
#'
#' @return NULL If all checks pass. Stop with error otherwise.
#' @export
marking_assignments_check <- function(submissions, marking_assignments){

  # number of rows identical
  stopifnot(nrow(marking_assignments) == nrow(submissions))

  # no NAs introduced
  stopifnot(nrow(marking_assignments %>% na.omit()) == nrow(submissions))

  # number of observations per seminar are identical
  stopifnot(all_equal(marking_assignments %>% group_by(seminar) %>% summarise(n = n()),
                      submissions %>% group_by(seminar) %>% summarise(n = n())))

  # number of observations per student are identical
  stopifnot(all_equal(marking_assignments %>% group_by(id) %>% summarise(n = n()),
                      submissions %>% group_by(id) %>% summarise(n = n())))

  # students are the same in both cases
  stopifnot(all_equal(marking_assignments %>% select(id),
                      submissions %>% select(id)))

  # no duplicates of students in either one (make one unique, and should be same as other)
  stopifnot(all_equal(marking_assignments %>% select(id) %>% distinct(),
                      submissions %>% select(id)))

  NULL
}



#' Assign marking assignments to HPLs
#'
#' @param submissions A data-frame of submissions
#'
#' For each seminar
#'
#' @return
#' @export
hpl_assignments <- function(submissions, K = 15) {
  submissions %>%
    dplyr::group_by(seminar) %>%
    dplyr::nest() %>%
    dplyr::mutate(data = map(data, ~{
      dplyr::sample_frac(.) %>% # random permute
        dplyr::slice(1:K)
    })) %>%
    dplyr::unnest(cols = data) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      dplyr::markers %>%
        dplyr::filter(is_hpl) %>%
        dplyr::select(seminar, marker, marker_email = email),
      by = 'seminar'
    )
}


#' Anonymise all submissions to a NOW dropbox
#'
#' @param dropbox_directory A folder with all dropbox submissions
#' @param newdir
#'
#' @return
#' @export
anonymise_submissions <- function(dropbox_directory, newdir) {

  fff <- function(dropbox_directory){

    submitted_report_filename_pattern <- '([0-9]{5,6}-[0-9]{6}) - ([NT][01][0-9]{6}) - (.*[A-Za-z])- ([^-]*) - (.*)'

    # From the filename, return
    parse_dropbox_fname <- function(fname){
      the_match <- stringr::str_match(fname, submitted_report_filename_pattern)
      list(id = the_match[1, 3],
           student = the_match[1, 4],
           tstamp = the_match[1, 5]
      )
    }

    # Return timestamp as a lubridate object
    parse_tstamp <- function(tstamp){
      stringr::str_replace(tstamp,
                           "(.*), ([0-9]{4}) ([12]*[0-9])([0-5][0-9]) (.*)",
                           "\\1 \\2, \\3:\\4 \\5") %>%
        lubridate::parse_date_time("d m y, H:M %p")
    }

    fs::dir_ls(path = dropbox_directory) %>%
      purrr::map_chr(tools::md5sum) %>%
      tibble::enframe(name = 'path', value = 'hash') %>%
      dplyr::mutate(fname = fs::path_file(path)) %>%
      dplyr::mutate(match = purrr::map(fname, parse_dropbox_fname)) %>%
      tidyr::unnest_wider(match) %>%
      dplyr::mutate(ext = tools::file_ext(fname))
  }


  submissions_df <- fff(dropbox_directory)

  ggg <- function(path, hash, ext, ...){
    originals_newdir <- fs::path_join(c(newdir, 'originals'))
    crypto_dir <- fs::path_join(c(newdir, 'crypto'))

    if (!fs::dir_exists(originals_newdir)){
      fs::dir_create(originals_newdir)
    }

    if (!fs::dir_exists(crypto_dir)){
      fs::dir_create(crypto_dir)
    }

    original <- fs::file_copy(path, originals_newdir, overwrite = TRUE)
    crypto <- fs::file_copy(
      path, fs::path_join(
        c(crypto_dir,
          stringr::str_c(stringr::str_sub(hash, end = 7L), '.', ext)
        )
      ), overwrite = TRUE
    )

    c(original = original, anonymous = crypto)
  }

  pmap_dfr(submissions_df, ggg)

}


#' Read in Exported NOW MCQ Exam CSV files
#'
#' @param dat_dir Path name of a directory with NOW MCQ exported csv files
#'
#' @return
#' @export
read_exported_now_mcq <- function(data_dir){

  purrr::map(fs::dir_ls(path = data_dir,
                             glob = '*.csv'),
             ~readr::read_csv(., progress = FALSE, show_col_types = FALSE)) %>%
    dplyr::bind_rows()%>%
    dplyr::select(nid = Username,
                  firstname = FirstName,
                  lastname = LastName,
                  item = `Q #`,
                  ans = `Answer Match`,
                  score = Score,
                  weight = `Out Of`) %>%
    dplyr::mutate(correct = score > 0,
                  weight = as.integer(weight),
                  item = as.integer(item),
                  score = as.integer(score)) %>%
    dplyr::filter(ans == 'Checked') %>%
    dplyr::select(nid, firstname, lastname, item, score, correct, weight)

}
