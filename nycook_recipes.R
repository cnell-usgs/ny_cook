library(stringr)
library(rvest)

nycook<-read_html("http://cooking.nytimes.com/68861692-nyt-cooking/3891782-our-50-most-popular-recipes-of-2016")
url<-"http://cooking.nytimes.com/68861692-nyt-cooking/3891782-our-50-most-popular-recipes-of-2016"
cook<-readLines(url)##read webpage

##pull url for weblinks
web_url<-'data-url=\"'
recipe_pages<-grep(web_url, cook[243:length(cook)], value=TRUE)##pull addys
recipe_id<-gsub(web_url, "", recipe_pages)%>% ##clean up link ids
  gsub(pattern='"',replacement= "")%>%
  str_trim(side="both")
recipe_id##looks good

##now write urls for each recipe
site_pre<-'http://cooking.nytimes.com'
site_post<-'?action=click&module=Collection+Page+Recipe+Card&region=Our+50+Most+Popular+Recipes+of+2016&pgType=collection&rank=1'
recipe_sites<-as.list(paste0(site_pre, recipe_id, site_post))

recipe_sites
cook_sites<-data.frame(site = recipe_sites,
                       id= recipe_id)
View(cook_sites)
##now pull recipe data from each site

recipe<-readLines(recipe_sites[1])#read page
grep('Ingredients', recipe)##find where ingredients start
recipe[364:450]###look for tags around each ingredient
ing_pattern<-'<span class=\"ingredient-name\">'
ing_list<-grep(ing_pattern, recipe[364:length(recipe)], value=TRUE)
ing_list
ingredients<-gsub(ing_pattern, "", ing_list)%>%
  gsub(pattern='</span>', replacement="")%>%
  gsub(pattern='<span>', replacement="")%>%
  str_trim(side="both")
ingredients

##needs to be cleaned up for simple ingredients
##remove everything after ',' because they are descrptive
ing<-gsub("\\,.*","", ingredients)
ing
##get rid of quantities
measures<-c("teaspoon","tablespoon","teaspoons","tablespoons","cup","pounds","pound","cups","pint","quart","gallon","ounce","ounces","gallons","quarts","pints","kg","mg")
#ing<-str_replace_all(string=ingredients, pattern=measure, replacement=" ")

ing_only <- as.character(sapply(ing, function(x) 
  gsub(paste(measures, collapse = '|'), '', x)))

ing_only<-tolower(ing_only)
ingreds<-as.data.frame(ing_only)
colnames(ingreds)<-recipe_id[1]
View(ingreds)

##function that takes a nytimes recipe list and extracts all the ingredients for each recipe
##returns a df with the name of the recipe and ingredients found in each
##does some cleaning of ingredient names to match (imperfect)
##written for the top recipes of 2016, working on making universal
#params
#url = the url of the nytimes cooking website list

get_recipe<-function(url){
 ##use url to find the key of each recipe and make list called recipe_id
 ##find in url where the key is and how to split
  #
  nycook<-read_html("http://cooking.nytimes.com/68861692-nyt-cooking/3891782-our-50-most-popular-recipes-of-2016")
  url<-"http://cooking.nytimes.com/68861692-nyt-cooking/3891782-our-50-most-popular-recipes-of-2016"
  cook<-readLines(url)##read webpage
  
  ##pull url for weblinks
  web_url<-'data-url=\"'
  recipe_pages<-grep(web_url, cook[243:length(cook)], value=TRUE)##pull addys
  recipe_id<-gsub(web_url, "", recipe_pages)%>% ##clean up link ids
    gsub(pattern='"',replacement= "")%>%
    str_trim(side="both")
  
  recipe_ingredients=NULL #empty df for ingredient list
  ##then integrate in the for loop
}


####################################
recipe_ingredients=NULL
##loop to pull ingredients from all recipes in list from nytimes
for (i in recipe_id){
  ##extract keys for each recipe and extract web text
  site_pre<-'http://cooking.nytimes.com'
  site_post<-'?action=click&module=Collection+Page+Recipe+Card&region=Our+50+Most+Popular+Recipes+of+2016&pgType=collection&rank=1'
  recipe_site<-paste0(site_pre, i, site_post)
  recipe<-readLines(recipe_site)
  #extract ingredients
  ing_pattern<-'<span class=\"ingredient-name\">'
  ing_list<-grep(ing_pattern, recipe[300:length(recipe)], value=TRUE)
  #clean ingredient
  ingredients<-gsub(ing_pattern, "", ing_list)%>%
    tolower()%>%
    gsub(pattern='</span>', replacement="")%>%
    gsub(pattern='<span>', replacement="")%>%
    gsub(pattern="\\,.*", replacement="")%>%
    gsub(pattern='[0-9]+', replacement='')%>%
    str_replace_all(pattern="[^[:alnum:]]", replacement=" ")
  rec_name<-gsub(i, pattern="^.*?\\-", replacement="") ##convert url to recipe name
  ##remove extra wordage
  measures<-c("teaspoon","tablespoon","teaspoons","ears ","stems of","sleeve","s", "sprig","to taste","tablespoons"," can ","cup","pounds","pound","cups","stick","sticks","pint","quart","pinch","gallon","ounce","grams","ounces","gallons","quarts","pints","kg","mg","milliliters")
  adjectives<-c("boneless","squeezed","stalks","rich","prepared","about"," of "," or ","mixed","slivered"," day "," old ","fine","rolled","raw ","wild"," and ","unsalted","ripe","bunch","firm","size","plus","head","neutral","cleaned","extra","very","sifted","branches","kosher","yellow","juice","zest","paste","clove","cloves","tender","leaves","large","fresh","sprigs","skinless","medium","small","diced","chopped","thick","slices","sliced","minced","packed","loosely","flaky","handful","crushed","roughly","pitted","finely","low sodium","whole","cold","hot","warm","toasted","roasted","grated","shredded","peeled","skinned","freshly","unsweetened","canned","dried","ground")
  ing_o <- as.character(sapply(ingredients, function(x) 
    gsub(paste(measures, collapse = '|'), '', x, fixed=TRUE)))
  ing_only <- as.character(sapply(ing_o, function(x) 
    gsub(paste(adjectives, collapse = '|'), '', x, fixed=TRUE)))
  ing_only<-ing_only%>%str_trim(side="both")#clean whitespace
  ingreds<-data.frame(recipe = rep(rec_name,length(ing_only)), ingredient = ing_only)
  recipe_ingredients=rbind(recipes,ingreds)#return df
}
length(unique(recipe_ingredients$recipe))
length(unique(recipe_ingredients$ingredient))
length(recipe_ingredients$ingredient)
View(recipe_ingredients)

##how many ingredients are in each recipe?
ing_count<-recipe_ingredients%>%
  group_by(recipe)%>%
  summarize(n_ingredients = length(unique(ingredient)), 
            ingredient_list= paste(ingredient, collapse = ", "))
View(ing_count)

##look at results
df<-recipe_ingredients%>%
  group_by(ingredient)%>%
  summarize(recipes_in = length(recipe))
View(df)

fd<-recipe_ingredients[-1,]
fd.melt<-melt(fd, id.vars=c("recipe"),na.rm=FALSE)
trec_cast<-dcast(fd.melt, recipe~value,fill=0, drop=FALSE)
rec_wide<-trec_cast%>%select(-contains('href'), -Var.2)
View(rec_wide)
###wide df- recipe x ingredients
recs<-rec_wide[,-1]
rownames(recs)<-rec_wide$recipe
View(recs)
rec<-as.matrix(recs)
###network data
##adjacency matrix
#by ingredients
ing_adj<-crossprod(rec)


##recipes similar to one another
rec_adj<-crossprod(t(rec))



