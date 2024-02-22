#' from ebsco psycinfo output to cleaned screening sheet
#'
#' This function turns the .xlsx file (converted from the EBSCO xml file) into a clean xlsx that provides article ID, first author, year of publication, journal, and title.
#' @param path this is the path of the folder containing the .xlsx file converted from the xml EBSCO file.
#' @param name this is the name of the .xlsx file converted from the xml EBSCO file.
#' @examples cleanebsco(path = '/Users/phoebelam/Desktop/problemset', name = 'ebsco export').
#' @importFrom magrittr "%>%"
#' @export

cleanebsco2<- function(path, name) {
  
  dat <- xml2::read_xml(paste(path, "/", name, ".xml", sep=""))
  
  xml2::xml_text(xml2::xml_find_all(dat, "./rec/@resultID")) -> articleid
  xml2::xml_text(xml2::xml_find_all(dat, ".//btl")) -> title
  xml2::xml_text(xml2::xml_find_all(dat, ".//jtl")) -> journal
  xml2::xml_text(xml2::xml_find_all(dat, ".//@year")) -> year
  
  data.frame(article_id = articleid,
             title = title, 
             journal = journal,
             year = year) -> new
  
  
  openxlsx::write.xlsx(new, paste(path, "/", name, "_cleaned.xlsx", sep=""))
  
  print("cmumeta  |  clean EBSCO search file exported, please check your folder.")
  
  
}
