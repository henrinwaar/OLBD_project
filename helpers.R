utf8decode <- function(string){
  string = gsub("Ã©", "é", string)
  string = gsub("Ãª", "ê", string)
  string = gsub("Ã¨", "è", string)
  string = gsub("Ã¶", "ö", string)
  string = gsub("Ã«", "ë", string)
  string = gsub("Ã§", "ç", string)
  string = gsub("Ã¢", "â", string)
  string = gsub("Ã´", "ô", string)
  string = gsub("Ã®", "î", string)
  string = gsub("Ã", "à", string)
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
