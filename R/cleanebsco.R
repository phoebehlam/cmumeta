#' from ebsco psycinfo output to cleaned screening sheet
#'
#' This function turns the .xlsx file (converted from the EBSCO xml file) into a clean xlsx that provides article ID, first author, year of publication, journal, and title.
#' @param path this is the path of the folder containing the .xlsx file converted from the xml EBSCO file.
#' @param name this is the name of the .xlsx file converted from the xml EBSCO file.
#' @examples cleanebsco(path = '/Users/phoebelam/Desktop/problemset', name = 'ebsco export').
#' @importFrom magrittr "%>%"
#' @export

cleanebsco<- function(path, name, windows=F) {
  
  if (windows == F) {
  
    openxlsx::read.xlsx(paste(path, "/", name, ".xlsx", sep=""), startRow = 2) %>% 
      dplyr::rename(article_ID = `/rec/@resultID`,
             author = `/rec/header/controlInfo/artinfo/aug/au`,
             title = `/rec/header/controlInfo/artinfo/tig/atl`,
             journal = `/rec/header/controlInfo/jinfo/jtl`,
             abstract = `/rec/header/controlInfo/artinfo/ab`,
             year = `/rec/header/controlInfo/pubinfo/dt/@year`) %>%
      dplyr::select(article_ID, author, title, journal, abstract, year)-> dat
    
    dat %>% 
      dplyr::group_by(article_ID) %>%
      tidyr::fill(title, .direction = 'down') %>% 
      tidyr::fill(journal, .direction = 'up') %>% 
      tidyr::fill(abstract, .direction = 'down') %>% 
      dplyr::filter(is.na(author)==F) %>% 
      dplyr::filter(dplyr::row_number()==1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(first_author = author) %>%
      dplyr::select(article_ID, first_author, year, journal, title, abstract) %>%
      dplyr::mutate(article_ID = suppressWarnings(as.numeric(article_ID)),
                    year = suppressWarnings(as.numeric(year))) %>%
      dplyr::filter(is.na(article_ID)==F)-> dat
    
  }else if(windows == T) {
    
    dat %>%
      dplyr::group_by(resultID) %>%
      tidyr::fill(atl, .direction = 'down') %>% 
      tidyr::fill(jtl, .direction = 'down') %>% 
      tidyr::fill(ab, .direction = 'up') %>% 
      dplyr::filter(is.na(au5)==F) %>% 
      dplyr::filter(dplyr::row_number()==1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(article_ID = resultID,
                    first_author = au5,
                    title = atl,
                    journal = jtl,
                    abstract = ab) %>%
      dplyr::select(article_ID, first_author, year, journal, title, abstract) %>%
      dplyr::mutate(article_ID = suppressWarnings(as.numeric(article_ID)),
                    year = suppressWarnings(as.numeric(year))) %>%
      dplyr::filter(is.na(article_ID)==F)-> dat
    
    
  }
  
  openxlsx::write.xlsx(dat, paste(path, "/", name, "_cleaned.xlsx", sep=""))
  
  print("cmumeta  |  clean EBSCO search file exported, please check your folder.")
  
}

