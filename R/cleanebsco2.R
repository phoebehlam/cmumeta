#' from ebsco psycinfo output to cleaned screening sheet
#'
#' This function turns the .xml file into a clean xlsx that provides article ID, first author, year of publication, journal, and title.
#' @param path this is the path of the folder containing the .xml
#' @param name this is the name of the .xml file converted from the xml EBSCO file.
#' @examples cleanebsco(path = '/Users/phoebelam/Desktop/problemset', name = 'ebsco export').
#' @importFrom magrittr "%>%"
#' @export
cleanebsco2<- function(path, name) {
  
  dat <- xml2::read_xml(paste(path, "/", name, ".xml", sep=""))
  
  xml2::xml_text(xml2::xml_find_all(dat, "./rec/@resultID")) -> articleid
  xml2::xml_text(xml2::xml_find_all(dat, ".//btl")) -> title
  xml2::xml_text(xml2::xml_find_all(dat, ".//jtl")) -> journal
  xml2::xml_text(xml2::xml_find_all(dat, ".//@year")) -> year
  xml2::xml_text(xml_find_all(dat, ".//aug")) -> authorlong

  data.frame(long = as.character(authorlong)) %>%
    dplyr::mutate_all(na_if,"") %>% 
    suppressWarnings(tidyr::separate(., long, c("first_author_lastname", "first_author_firstname"), sep=", ")) -> auth
  
  toDelete <- seq(0, nrow(auth), 2)
  auth[ toDelete ,] -> auth
  
  data.frame(article_id = articleid,
             title = title, 
             journal = journal,
             year = year) -> new
  
  cbind(new, auth) %>%
    select(., article_id, title, first_author_lastname, first_author_firstname, journal, year)-> final
  
  openxlsx::write.xlsx(final, paste(path, "/", name, "_cleaned.xlsx", sep=""))
  
  print("cmumeta  |  clean EBSCO search file exported, please check your folder.")
  
  
}
