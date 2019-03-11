#' Encrypt and save and .RData file
#'
#' @param data data structure to encrypt
#' @param password password to encrypt/decrypt the file
#' @param saveLocation location to save the .RData file
#'
encrypt_data <- function(data,password,saveLocation){
  nonce <- random(24)
  key <- hash(charToRaw(password))

  msg <- serialize(data, NULL)
  cipher <- data_encrypt(msg, key, nonce)

  encrypted <- cipher
  save(encrypted,file=saveLocation)
}
