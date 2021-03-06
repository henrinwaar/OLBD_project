utf8decode <- function(string){
  string = gsub("é", "�", string)
  string = gsub("ê", "�", string)
  string = gsub("è", "�", string)
  string = gsub("ö", "�", string)
  string = gsub("ë", "�", string)
  string = gsub("ç", "�", string)
  string = gsub("â", "�", string)
  string = gsub("ô", "�", string)
  string = gsub("î", "�", string)
  string = gsub("�", "�", string)
}

source.utf8 <- function(f) {
  l <- readLines(f, encoding="UTF-8")
  eval(parse(text=l),envir=.GlobalEnv)
}

giveHTMLContent <- function(name, surface, description, adress, image){
  content = paste(sep = "<br/><br/>",
                  paste("<h4> ", utf8decode(gsub("_", " ", name)), " </h4>"),
                  paste0("<img src =" , image, ' width="300" height="300">'),
                  utf8decode(paste("<b> Location: </b> &nbsp &nbsp ", adress)),
                  paste("<b> Surface: </b> &nbsp &nbsp", surface, "m2", sep = " "),
                  utf8decode(gsub("@en", "", gsub("@fr", "", description)))
  )
  return(content)
}
