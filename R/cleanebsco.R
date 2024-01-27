#' from ebsco psycinfo output cleaning
#'
#' This function turn the .xlsx file converted EBSCO xml into a cleaned xlsx that provides article ID, first author, year of publication, journal, and title.
#' @param path this is the path of the folder containing the .xlsx file converted from the xml EBSCO file.
#' @param name this is the name of the .xlsx file converted from the xml EBSCO file.
#' @examples cleanebsco(path = '/Users/phoebelam/Desktop/problemset', name = 'ebsco export').
#' @importFrom magrittr "%>%"
#' @export

cleanebsco<- function(path, name) {
  
  openxlsx::read.xlsx(paste(path, "/", name, ".xlsx", sep="")) %>%
    dplyr::group_by(X2) %>%
    tidyr::fill(X28, .direction = 'up') %>%
    dplyr::filter(is.na(X7)==F) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(article_ID = X2,
                  first_author = X7,
                  title = X15,
                  year = X34,
                  journal = X28) %>%
    dplyr::select(article_ID, first_author, year, journal, title) %>%
    dplyr::mutate(article_ID = as.numeric(article_ID)) %>%
    dplyr::filter(is.na(article_ID)==F)-> dat
  
  openxlsx::write.xlsx(dat, paste(path, "/", name, "_cleaned.xlsx", sep=""))
  
  print("cmumeta  |  clean EBSCO search file exported, please check your folder.")
  
}

