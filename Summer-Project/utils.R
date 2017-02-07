utils

#count the number of times someone changes transaction type
transition <- function(char_vect){
  char_vect <- char_vect[!is.na(char_vect)]
  if(length(char_vect) == 0){
    return(NA)
    break}
  old_char <- char_vect[1]
  counter = 0
  for(i in char_vect){
    new_char = i
    if(new_char != old_char){
      counter = counter + 1
    }
    old_char <- new_char
  }
  return(counter)
}
