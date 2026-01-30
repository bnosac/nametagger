#' @title Read Europeana Newspaper data 
#' @description Read Europeana Newspaper data. Data is made available at \url{https://github.com/EuropeanaNewspapers/ner-corpora} under the CC0 license.
#' @param x path to the file
#' @export
#' @return a data.frame with columns doc_id, sentence_id, token, entity
#' @examples 
#' \dontshow{if(require(udpipe))\{}
#' library(udpipe)
#' r <- "https://raw.githubusercontent.com/EuropeanaNewspapers/ner-corpora/master"
#' \donttest{
#' x <- file.path(r, "enp_NL.kb.bio", "enp_NL.kb.bio")
#' x <- europeana_read(x)
#' x <- file.path(r, "enp_FR.bnf.bio", "enp_FR.bnf.bio")
#' x <- europeana_read(x)
#' x <- file.path(r, "enp_DE.sbb.bio", "enp_DE.sbb.bio")
#' x <- europeana_read(x)
#' x <- file.path(r, "enp_DE.onb.bio", "enp_DE.onb.bio")
#' x <- europeana_read(x)
#' x <- file.path(r, "enp_DE.lft.bio", "enp_DE.lft.bio")
#' x <- europeana_read(x)
#' }
#' \dontshow{\} # End of main if statement running only if the required packages are installed}
europeana_read <- function(x){
  label <- basename(x)
  x <- readLines(x, encoding = "UTF-8")
  x <- x[!startsWith(x, prefix = "#")]
  x <- strsplit(x, split = " ")
  x <- data.frame(doc_id = label, 
                  token = sapply(x, FUN=function(x) x[1]), 
                  entity = sapply(x, FUN=function(x) x[2]), 
                  stringsAsFactors = FALSE)
  x$token_next <- udpipe::txt_next(x$token)
  x$token_prev <- udpipe::txt_previous(x$token)
  x$new_sentence_id <- (endsWith(x$token_prev, ".") | endsWith(x$token_prev, "!") | 
                          endsWith(x$token_prev, "?") | x$token_prev %in% c(".", "?", "!")) & 
    substr(x$token, 1, 1) %in% LETTERS
  x$new_sentence_id[1] <- TRUE
  x$sentence_id <- cumsum(x$new_sentence_id)
  x <- x[, c("doc_id", "sentence_id", "token", "entity")]
  x
}

#' @name europeananews
#' @title Tagged news paper articles from Europeana
#' @description BIO-tagged news articles in different languages extracted from \url{https://github.com/EuropeanaNewspapers/ner-corpora} 
#' using \code{\link{europeana_read}}
#' \itemize{
#' \item{Dutch from the Koninklijke Bibliotheek}
#' \item{Austrian from the National Library of Austria}
#' \item{German from the Dr. Friedrich Teßmann Library}
#' \item{French from the National Library of France, in cooperation with LIP6-ACASA}
#' }
#'     
#' @references 
#'  Europeana Newspapers project, (2014), KB Europeana Newspapers NER Dataset. KB Lab: The Hague. \url{https://lab.kb.nl/dataset/europeana-newspapers-ner}
#' @docType data
#' @examples 
#' data(europeananews)
#' str(europeananews)
NULL


