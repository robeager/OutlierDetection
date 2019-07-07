#' Scalar Encoding
#' @name ScalarEncoder
#' @description The Scalar Encoder takes an incoming number and encodes it for processing by the Spatial Pooler
#' @param n The number of total cells within the input matrix
#' @param w The size (width) of the encoded bitmask
#' @param resolution

ScalarEncoder <- function(
  TotalBits = 400,
  BitMask = 21,
  EncodeResolution = 0.56,
  EncodeBuckets = n-w+1
) {
  
  lowerBuffer = w
  upperBuffer = (n-w)
  minChange = 0
  maxChange = 232
  
}