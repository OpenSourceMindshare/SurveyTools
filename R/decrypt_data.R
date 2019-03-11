#' Decrypt an encrypted .RData file
#'
#' A shiny gadget will pop up so the password can be provided
#'
#' @param fileLocation location of the file
decrypt_data <- function(fileLocation){
  loadIn <- new.env()
  load(fileLocation,envir=loadIn)
  readIn <- get(ls(loadIn),envir=loadIn)

  password <- get_password()
  key <- hash(charToRaw(password))
  return(unserialize(data_decrypt(readIn,key)))
}
