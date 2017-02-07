#text analysis


ingredients <- read.csv('all_ingredients.csv', stringsAsFactors = F)
recipes <- read.csv('all_recipes.csv', stringsAsFactors = F)

df <- merge(recipes, ingredients, by = 'recipe_id')
head(df)
str(df)

head(df$ingredients)

#The analysis done on the ingredients should be different than on the title,
# Website, score, etc.. 

ingred <- df$ingredients

#Clean ingredients
#remove numbers, single letters, special characters, 
lapply(ingred, function(x){
  temp <- gsub('\\d',"",x) #remove numbers
  temp <- gsub('[[:punct:]]', " ", temp) # remove special characters
  temp <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", temp) #remove single letters
  temp <- gsub('\\s+', " ", temp) #remove extra white space
  temp <- tolower(temp)
})




temp <- paste(temp, 'a a a a a a b b b b', sep = ' ')
