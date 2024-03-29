#Current line : 247

#Loading required packages
used_libraries <- c("dplyr","lubridate")
lapply(used_libraries, require, character.only = TRUE)

getwd()

# Creating a Vector : elemenrs are of same data type
v1 <- c(1:12)

# Creating a matrix
m1 <- matrix(v1,nrow = 4,byrow = FALSE)

# Creating a dataframe
df1 <- data.frame(
    id = c('1','2','3','4','5'),
    name = c('Sam','John','Mary','Rahul','Paul'),
    job = c('Devops','Support','Admin','Helpdesk','Analyst'),
    DOB = dmy(c('10-1-1990','23-7-1993','2-1-1985','13-12-1988','10-9-1997')),
    stringsAsFactors = FALSE
)

# Creating a list : elements can be of different data types
a1 <- "John"    #Character
a2 <- 3.14      #Numeric
a3 <- FALSE     #Logical
a4 <- c('1','2','3','4','5')                        #Numeric Vector
a5 <- c('Alpha','Beta','Charlie','Delta','Echo')    #Character Vector
a6 <- c('TRUE','FALSE','TRUE','FALSE','TRUE')       #Logical Vector
a7 <- matrix(10:21,nrow = 4,ncol = 3, byrow = TRUE) #Matrix
a8 <- data.frame(a1,a2,a3)                          #Data Frame

l1 <- list(a1,a2,a3,a4,a5,a6,a7,a8)
l1[[4]]
l1[[4]][2]
l2 <- list(Element1=a1,Element2=a2,Element3=a3,Element4=a4,Element5=a5,Element6=a6,Element7=a7,Element8=a8)                  #List
l2$Element5
l2$Element5[2]
#Unlisting
print(length(unlist(l1)))
df2 <- matrix(unlist(l1), ncol = 11, byrow = TRUE)
df3 <- data.frame(matrix(unlist(l1), ncol = ceiling(length(unlist(l1))/3), byrow=TRUE))


#Reading country Data except first row
country_data <- (read.csv("Country.csv", sep=";", header=TRUE))[-1,]
#Creating a list which stores column numbers whose data type needs to be changed
col_to_be_changed <- c(2:ncol(country_data)) 
#Changing the data type of the columns
country_data[,col_to_be_changed] <- as.numeric(as.character(unlist(country_data[col_to_be_changed])))
#Removes scientific notation in R
options(scipen = 999)
#Storing names of dataframe in a list to modify it
country_data_col_names <- names(country_data)
#Substituting 3 successive dots with a space
country_data_col_names <- gsub(pattern="\\.{3}", replacement=" ", ignore.case=TRUE, x=country_data_col_names)
#Substituting a single dot with a space
country_data_col_names <- gsub(pattern="\\.", replacement=" ", ignore.case=TRUE, x=country_data_col_names)
#Substituting spaces with an underscore 
country_data_col_names <- gsub(pattern="[[:space:]]", replacement="_", ignore.case=TRUE, x=country_data_col_names)
#Substituting underscore at the end with a blank
country_data_col_names <- gsub(pattern="_$", replacement="", ignore.case=TRUE, x=country_data_col_names)
#Replacing the column names of the data
names(country_data) <- country_data_col_names


#Reading inflation data
inflation_data <- read.csv("IncomeGroup.csv",header = TRUE)
#Removing 3rd column from the data
inflation_data <- inflation_data[,-3]
#Storing names of dataframe in a list to modify it
inflation_data_col <- names(inflation_data)
#Modifying the names of columns
inflation_data_col[3:ncol(inflation_data)] <- gsub(pattern="[^0-9]",replacement="",ignore.case=TRUE, x=inflation_data_col[3:ncol(inflation_data)])
#Replacing column names with modified names
names(inflation_data)<- inflation_data_col

#Reading R&D data
rnd_data <-  read.csv("rnd.csv", header = TRUE)
rnd_data$Value <- as.numeric(as.character(rnd_data$Value))
rnd_data[is.na(rnd_data)] <- 0

#METADATA
#Get the structure, summary, field names, column names, row names of the data
str(country_data)
summary(country_data)
names(country_data)
colnames(country_data)
rownames(country_data)

#Get value stored at first row and second column of the data
country_data[1,2]

#COLUMN MANIPULATION
#Get first column : output is a dataframe
country_data[1]
#Get first column : output is a list
country_data[,1]
#Get sku column : output is a list
country_data$Country
#Removing a column by its index
remove_first_column <- country_data[-1]

#ROW MANIPULATION
#Get first row  : output is a dataframe
country_data[1,]
#Get specific rows : output is a dataframe
country_data[c(1,3),]
#Get values from specific rows and specific columns : output is a dataframe
country_data[c(1,3),c(1,2)]
#Removing specific rows and columns from the data
remove_row_column <- country_data[-1,-c(2,3)]

#SUBSETTING DATA
#Subsetting data(rows) based on a condition and without condition
subset1 <- country_data[which(country_data$Area_sq_km>1000000),]
subset1a <- country_data[which(country_data[,2]>1000000),]
#Subsetting data(rows and columns) based on a condition and without condition
subset2 <- country_data[which(country_data$Area_sq_km>1000000),c(1,2,3,4)]
subset2a <- country_data[which(country_data[,2]>1000000),c(1,2,3,4)]
subset2b <- country_data[c(1:100),c("Country","Area_sq_km")]
#Subsetting data(columns) based on a condition and without condition
subset3 <- country_data[1:5]
subset3a <- country_data[c("Country","Area_sq_km","Exports","GDP")]

#MISSING VALUE TREATMENT
#Output is a logical table which returns TRUE if value is NA else returns FALSE
is.na(country_data)
#Substituting NA's with 0 in the data
temp1 <- country_data
temp1[is.na(temp1)] <- 0
#Omit those rows which has NA in them
View(na.omit(country_data))
#Omit rows with NA but only in certain columns
View(na.omit(country_data[,c(1,2,3,4,5)]))


#FLOW CONTROL STATEMENTS IN R =====================================================================
# IF Statement
value_to_be_matched <- "Delhi" #Change the content of the string to see different results
if(!is.na(match(value_to_be_matched,country_data[,1]))){
    print("Country is in the list !")
}

# IF-ELSE Control Statement
if(!is.na(match(value_to_be_matched,country_data[,1]))){
    print("Country is in the list !")
}else{
    print("Country is not in the list !")
}

# IFELSE Control Statement
# runif(n) generates n uniform random numbers between 0 and 1.
# runif(n, a, b) generates n uniform random numbers between a and b.
list1 <- floor(100*runif(12))
matrix1 = matrix(list1,4,3)
temp2 <- ifelse(matrix1 %% 2 == 0, "Even", "Odd")
View(temp2)

# REPEAT Control Statement
i=1
N = nrow(country_data)
subset3 <- country_data[,1:2]
subset3$Type <- "-"
repeat{
    subset3[i,3] <- ifelse(subset3[i,2]>=1000000,"Densely Populated",(ifelse(subset3[i,2]>500000 && subset3[i,2]<1000000,"Moderately Populated","Sparsely Populated")))
    if(i>=N) break
    i<-i+1
}

# FOR Control Statement
#Iteration is done on a numeric vector
vector1 <- c('1','2','3','4','5','6')
for(i in vector1){
    print("Hello there !")
}
#Iteration is done on a character vector
vector2 <- c("Alpha","Beta","Charlie","Delta","Echo","Foxtrot","Gamma","Holland","India","Juliet")
for(i in vector2){
    print(paste0("Iteration ",match(i,vector2)," : ",i))
}
#Iteration is done on a list
subset4 <- country_data[1]
list2 <- c("1","One","2","Two","3","Three",vector1,vector2,subset4)
for(i in list1){
    print(paste0("Iteration ",match(i,list1)," : ",i))
}

# WHILE Control Statement
i <- 1
temp3 <- matrix(runif(nrow(country_data)),nrow(country_data),1) #Created a dummy matrix having same no of rows as country_data
while(i<=nrow(country_data)){
    temp3[i,1] <- paste0(country_data[i,1]," has an area of ",country_data[i,2]," sq kms.")
    i <- i+1
}

# BREAK Control Statement
for (i in c(1:10)) {
    if (i==3) break
    print(i)
}

# NEXT Control Statement
for (i in c(1:10)) {
    if (i %in% vector1) next
    print(i)
}


# APPLY FAMILY OF FUNCTIONS========================================================================
#apply : Is performed on an array or a matrix
temp4 <- matrix(c(1:10, 11:20, 21:30), nrow = 10, ncol = 3)
#For row operation : Margin = 1
apply(temp4, 1, sum)
#For column operation : Margin = 2
apply(temp4, 2, sum)
#Giving the whole user defined function in the argument itself
apply(temp4, 1, function(x) x*2)
apply(temp4, 2, function(x) x*2)
#For each cell operation : Margin = 1:2
apply(temp4,1:2, function(x) x+5)
#Specifying the function separately and just passing the name of it
custom_function <- function(x){
    sd(x)/sqrt(length(x))
}
apply(temp4, 2, custom_function)
apply(temp4, 1, custom_function)


#lapply, sapply, vapply : Is performed on a vactor or a list
#lapply returns a list as output but sapply give a more simplified output i.e. returns a vector wherever possible
#vapply requires you to specify the output data type (FUN.VALUE = data_type)
View(l1)
View(list1)
View(list2)
View(lapply(l1, length))
View(sapply(l1, length))
View(vapply(l1, length, numeric(1))) #Need to work on more examples of vapply

#tapply
#second arguement is the column on which indexing needs to be done
#first argument is the column on which the function is applied
View(rnd_data)
# tapply(rnd_data$Value, rnd_data$Year, sum)
# temp5 <- as.matrix(unlist(tapply(rnd_data$Value, rnd_data$Year, function(x) c(sum(x),mean(x)))),nrow=5,col=2)
temp5 <- matrix(unlist(tapply(rnd_data$Value, rnd_data$Year, function(x) c(sum(x),mean(x))),use.names = FALSE),nrow=length(unique(rnd_data$Year)),byrow = TRUE)

#mapply : It applies the specified functions to the arguments one by one
v2 <- vector(mode = "numeric", length = 900)
v2 <- mapply(function(x,y){paste0(x," ",y)}, rnd_data$Value, rnd_data$Unit)
mapply(function(x,y){x^y},c(2,3),c(3,4))
mapply(function(x,y,z,k){(x+k)^(y+z)},c(a=2,b=3),c(A=3,B=4),MoreArgs=list(1,2))
mapply(function(x,y,z,k){(x+k)^(y+z)},c(a=2,b=3),c(A=3,B=4),MoreArgs=list(1,2), USE.NAMES = FALSE)

#by : is similar to tapply function but is used over data frame or matrix
View(iris)
by(iris$Sepal.Length,list(iris$Species),mean)
View(mtcars)
by(mtcars$hp,list(mtcars$gear,mtcars$cyl),mean)




install.packages("Hmisc")
library(Hmisc)

describe(data_set)
glimpse(data_set)


#Factors   
V_conversion <- c('Alpha','Beta','Charlie','Delta','Echo')    
class(V_conversion)    
V_conversion <- factor(V_conversion)    
class(V_conversion)

height <- c(132,151,162,139,166,147,122)
weight <- c(48,49,66,53,67,52,40)
gender <- c("male","male","female","female","male","female","male")
df_hwg <- data.frame(height,weight,gender)
is.factor(df_hwg$gender)
df_hwg$gender <- factor(df_hwg$gender)
print(df_hwg$gender)    
df_hwg$gender <- factor(df_hwg$gender,levels = c("male","female"))    
print(df_hwg$gender)    


generate_factor_col <- gl(4, 2, labels = c("Alpha", "Beta","Charlie","Delta"))    
View(generate_factor_col)


#---------------------------------------------------------------------------------------------------------------------------------------

#Playing with strings
a9 <- "This is a sample string"
nchar(a9)
str_length(a9)
tolower(a9)
str_to_lower(a9)
toupper(a9)
str_to_upper(a9)

x <- c("Alpha", "Beta", "Charlie","AlphaDelta")
abbreviate(x, 1)

#SplitString function
x <- "Split the words in a sentence."
strsplit(x," ")

x <- "Split at every character."
strsplit(x,"")

x <- " Split at each space with a preceding character."
strsplit(x, ". ")

x <- "Do you wish you were Mr. Jones?"
strsplit(x, ". ")
strsplit(x, ". ", fixed=TRUE)


dates <- c("1999-05-23", "2001-12-30", "2004-12-17")
temp  <- strsplit(dates, "-")
temp
matrix(unlist(temp), ncol=3, byrow=TRUE)


Names <- c("Brin, Sergey", "Page, Larry",
                 +            "Dorsey, Jack", "Glass, Noah",
                 +            "Williams, Evan", "Stone, Biz")
Cofounded <- rep(c("Google", "Twitter"), c(2,4))
temp <- strsplit(Names, ", ")
temp
mat  <- matrix(unlist(temp), ncol=2, byrow=TRUE)
df   <- as.data.frame(mat)
df   <- cbind(df, Cofounded)
colnames(df) <- c("Last", "First", "Cofounded")
df


#Sub() function
x <- "Betty had a bit of butter !"
#ignores the case of replacement
y <- sub("Butter","pie", x)
y <- sub("Butter","pie", x, ignore.case = TRUE)
#Searches for the fixed string
y <- sub("h.*d","Replacement", x)
y <- sub("had","Replacement", x, fixed=TRUE)

#replace single, all digits 
x <- c("15 line 435", "good weather", "89 pigs")
y <- sub("[[:digit:]]","",x)
y <- sub("[[:digit:]]+","",x)

library(stringr)
string <- "Los Angeles, officially the City of Los Angeles and often known by its initials L.A., 
            is the second-most populous city in the United States (after New York City), the most populous city in 
            California and the county seat of Los Angeles County. Situated in Southern California, Los Angeles is 
            known for its Mediterranean climate, ethnic diversity, sprawling metropolis, and as a major center of the 
            American entertainment industry."

strwrap(string) 
nchar(string) 
str_length(string) 
tolower(string) 
str_to_lower(string) 
toupper(string) 
str_to_upper(string) 
chartr("and","for",x = string) #letters a,n,d get replaced by f,o,r 
str_replace_all(string = string, pattern = c("City"),replacement = "state") #this is case sentitive 

#extract parts of string 
substr(x = string,start = 5,stop = 11)
#extract angeles str_sub(string = string, start = 5, end = 11) 

#get difference between two vectors 
setdiff(c("monday","tuesday","wednesday"),c("monday","thursday","friday")) 

#check if strings are equal 
setequal(c("monday","tuesday","wednesday"),c("monday","tuesday","wednesday")) 
setequal(c("monday","tuesday","thursday"),c("monday","tuesday","wednesday")) 

#abbreviate strings 
abbreviate(c("monday","tuesday","wednesday"),minlength = 3) 

#split strings 
strsplit(x = c("ID-101","ID-102","ID-103","ID-104"),split = "-") 
str_split(string = c("ID-101","ID-102","ID-103","ID-104"),pattern = "-",simplify = T)
#find and replace first match 
sub(pattern = "L",replacement = "B",x = string,ignore.case = T) 

#find and replace all matches 
gsub(pattern = "Los",replacement = "Bos",x = string,ignore.case = T)


#=================================================================================================================
# grep	    :returns the index or value of the matched string
# grepl	    :returns the Boolean value (True or False) of the matched string
# regexpr	:return the index of the first match
# gregexpr	:returns the index of all matches
# regexec	:is a hybrid of regexpr and gregexpr
# regmatches:returns the matched string at a specified index. It is used in conjunction with regexpr and gregexpr

#Regular Expressions - METACHARACTERS . \ | ( ) [ ] { } $ * + ?
#Speical operators which regex doesn't capture.
#If any of these characters are available in a string, regex won't detect them unless they are prefixed with double backslash (\)
dt <- c("percent%","percent","percent%%","percent12345%") 
grep(pattern = "percent\\%", x = dt, value = T)

dt <- c("may?","money$","and&") 
grep(pattern = "[a-z][\\?-\\$-\\&]", x = dt, value = T)

gsub(pattern = "[\\?-\\$-\\&]", x = dt, replacement = "")
gsub(pattern = "\\\\", x = "Barcelona\\Spain", replacement = "-",) 

#Regular Expressions - Quantifiers
#Are mainly used to determine the length of the resulting match.
#Quantifiers exercise their power on items to the immediate left of it.
# .	It matches everything except a newline.
# ?	The item to its left is optional and is matched at most once.
# *	The item to its left will be matched zero or more times.
# +	The item to its left is matched one or more times.
# {n}	The item to its left is matched exactly n times. The item must have a consecutive repetition at place. e.g. Anna
# {n, }	The item to its left is matched n or more times.
# {n,m}	The item to its left is matched at least n times but not more than m times.
# Greedy Quantifiers     : The symbol .* is known as a greedy quantifier. 
#                          It says that for a particular pattern to be matched, 
#                          it will try to match the pattern as many times as its repetition are available.
# Non-Greedy Quantifiers : The symbol .? is known as a non-greedy quantifier. 
#                          Being non-greedy, for a particular pattern to be matched, it will stop at the first match.

number <- "101000000000100" 
#greedy 
regmatches(number, gregexpr(pattern = "1.*1",text = number)) 
#non greedy 
regmatches(number, gregexpr(pattern = "1.?1",text = number)) 

names <- c("anna","crissy","puerto","cristian","garcia","steven","alex","rudy") 
#doesn't matter if e is a match 
grep(pattern = "e*",x = names,value = T)
#must match t one or more times 
grep(pattern = "t+",x = names,value = T)
#must match n two times
grep(pattern = "n{2}",x = names,value = T)


#Regular Expressions - Sequences
#These are used to describe a pattern in a given string
# \d	matches a digit character
# \D	matches a non-digit character
# \s	matches a space character
# \S	matches a non-space character
# \w	matches a word character
# \W	matches a non-word character
# \b	matches a word boundary
# \B	matches a non-word boundary

string <- "I have been to Paris 20 times" 
#match a digit 
gsub(pattern = "\\d+",replacement = "_",x = string) 
regmatches(string,regexpr(pattern = "\\d+",text = string)) 
#match a non-digit 
gsub(pattern = "\\D+",replacement = "_",x = string) 
regmatches(string,regexpr(pattern = "\\D+",text = string)) 
#match a space - returns positions 
gregexpr(pattern = "\\s+",text = string) 
#match a non space 
gsub(pattern = "\\S+",replacement = "NonSpace",x = string) 
#match a word character 
gsub(pattern = "\\w",replacement = "Char",x = string) 
#match a non-word character 
gsub(pattern = "\\W",replacement = " NonWordChar ",x = string)


#Regular Expressions - Character classes
# Character classes refer to a set of characters enclosed in a square bracket [ ]. 
# These classes match only the characters enclosed in the bracket. 
# These classes can also be used in conjunction with quantifiers. 
# The use of the caret (^) symbol in character classes is interesting. 
# It negates the expression and searches for everything except the specified pattern.
# [aeiou]	    : matches lower case vowels
# [AEIOU]	    : matches upper case vowels
# [0123456789]: matches any digit
# [0-9]       : same as the previous class
# [a-z]       : match any lower case letter
# [A-Z]       : match any upper case letter
# [a-zA-Z0-9] : match any of the above classes
# [^aeiou]    : matches everything except letters
# [^0-9]      : matches everything except digits

string <- "20 people got killed in the mob attack. 14 got severely injured" 
#extract numbers 
regmatches(x = string,gregexpr("[0-9]+",text = string)) 
#extract without digits 
regmatches(x = string,gregexpr("[^0-9]+",text = string))


#Regular Expressions - POSIX Character Classes
# [[:lower:]]	: matches lower case letter
# [[:upper:]]	: matches upper case letter
# [[:alpha:]]	: matches letters
# [[:digit:]]	: matches digits
# [[:space:]]	: matches space characters eg. tab, newline, vertical tab, space, etc
# [[:blank:]]	: matches blank characters (same as previous) such as space, tab
# [[:alnum:]]	: matches alphanumeric characters, e.g. AB12, ID101, etc
# [[:cntrl:]]	: matches control characters. Control characters are non-printable characters such as \t (tab), \n (new line), \e (escape), \f (form feed), etc
# [[:punct:]]	: matches punctuation characters
# [[:xdigit:]]: matches hexadecimal digits (0 - 9 A - E)
# [[:print:]]	: matches printable characters ([[:alpha:]] [[:punct:]] and space)
# [[:graph:]]	: matches graphical characters. Graphical characters comprise [[:alpha:]] and [[:punct:]]
string <- c("I sleep 16 hours\n, a day","I sleep 8 hours\n a day.","You sleep how many\t hours ?")
#get digits 
unlist(regmatches(string,gregexpr("[[:digit:]]+",text = string))) 
#remove punctuations 
gsub(pattern = "[[:punct:]]+",replacement = "",x = string) 
#remove spaces 
gsub(pattern = "[[:blank:]]",replacement = "-",x = string) 
#remove control characters 
gsub(pattern = "[[:cntrl:]]+",replacement = " ",x = string) 
#remove non graphical characters 
gsub(pattern = "[^[:graph:]]+",replacement = "",x = string) 

#Examples-----------------
#1) Extract digits from a string of characters 
string <- "My roll number is 1006781" 
gsub(pattern = "[^0-9]",replacement = "",x = string) 
stringi::stri_extract_all_regex(str = string,pattern = "\\d+") #list
regmatches(string, regexpr("[0-9]+",string)) 
regmatches(string, regexpr("[[:digit:]]+",string))

#2) Remove spaces from a line of strings
gsub(pattern = "[[:space:]]",replacement = "",x = "and going there today tomorrow") 
gsub(pattern = "[[:blank:]]",replacement = "",x = "and going there today tomorrow") 
gsub(pattern = "\\s",replacement = "",x = "and going there today tomorrow")

#3) Return if a value is present in a vector
det <- c("A1","A2","A3","A4","A5","A6","A7") 
grep(pattern = "A1|A4",x = det,value =T) 

#4) Extract strings which are available in key value pairs
d <- c("(monday :: 0.1231313213)","tomorrow","(tuesday :: 0.1434343412)") 
grep(pattern = "\\([a-z]+ :: (0\\.[0-9]+)\\)",x = d,value = T) 
regmatches(d,regexpr(pattern = "\\((.*) :: (0\\.[0-9]+)\\)",text = d))

#5) In a key value pair, extract the values
string = c("G1:E001", "G2:E002", "G3:E003") 
gsub(pattern = ".*:",replacement = "",x = string)

#6) Remove punctuation from a line of text
going <- "a1~!@#$%^&*bcd(){}_+:efg\"<>?,./;'[]-=" 
gsub(pattern = "[[:punct:]]+",replacement = "",x = going)

#7) Remove digits from a string which contains alphanumeric characters
c2 <- "day of 2nd ID5 Conference 19 12 2005"
gsub(pattern = "\\b\\d+\\b",replacement = "",x = c2)

#8) Find the location of digits in a string
string <- "there were 2 players each in 8 teams" 
gregexpr(pattern = '\\d',text = string) 
unlist(gregexpr(pattern = '\\d',text = "there were 2 players each in 8 teams"))

#9) Extract information available inside parentheses (brackets) in a string
string <- "What are we doing tomorrow ? (laugh) Play soccer (groans) (cries)" 
gsub("[\\(\\)]","",regmatches(string, gregexpr("\\(.*?\\)", string))[[1]]) 

#10) Extract only the first digit in a range
x <- c("75 to 79", "80 to 84", "85 to 89") 
gsub(" .*\\d+", "", x)  

#11) Extract email addresses from a given string
string <- c("My email address is abc@boeing.com","my email address is def@jobs.com","aescher koeif","paul renne") 
unlist(regmatches(x = string, gregexpr(pattern = "[[:alnum:]]+\\@[[:alpha:]]+\\.com",text = string))) 



#Data Reshaping===================================================================================================
#Transpose, cbind, rbind
first <- matrix(c(1:12), nrow=4, byrow=TRUE) 
first <- t(first) 
first <- as.data.frame(first)
names(first) <- c('a','b','c','d')
second <- c('m','n','o')
first <- cbind(second,first)
third <- c('p','1','1','1','1')
first <- rbind(first,third)

#Gather : wide-format to long-format
#Spread : long-format to wide-format
library(tidyr)
month <- month.abb[1:3]
delhi <- sample(seq(-5,47,by=0.01),3,rep=TRUE)
mumbai <-sample(seq(-5,47,by=0.01),3,rep=TRUE)
chennai <-sample(seq(-5,47,by=0.01),3,rep=TRUE)
bangalore <- sample(seq(-5,47,by=0.01),3,rep=TRUE)
kolkata <- sample(seq(-5,47,by=0.01),3,rep=TRUE)
data <- data.frame(month,delhi,mumbai,bangalore,chennai,kolkata)
data
gathered_data <- gather(data,key="city",value="avg.temp",-month)
gathered_data
spread_data <- spread(gathered_data,key="city",value="avg.temp")

#Unite : combine two or more columns.
#Separate : separate a joined column in two or more columns.
months <- c("jan","feb","jan","feb")
year <- c("2018","2018","2019","2019")
temp <- c(4.64,19.68,2.56,36.74)
delhi_temp <- data.frame(months,year,temp)
delhi_temp
united_delhi_temp <- unite(delhi_temp,"interval",months,year)
united_delhi_temp
sep_delhi_temp <- separate(united_delhi_temp,interval,c("month","year"))
sep_delhi_temp

#Melt : wide-format to long-format
#Cast : long-format to wide-format
#dcast: output is a dataframe
#acast: output is a vector, matrix or an array
library(reshape2)
mdata <- melt(data,id.vars=c("month"), measure.vars=c("delhi", "mumbai","bangalore","chennai","kolkata"),
              variable.name="city", value.name="AvgTemp")
cdata <- dcast(mdata,month~city , value.var="AvgTemp")



#Visualizations===================================================================================================

# A statistical graphic is a mapping from data to aesthetic attributes (colour, shape, size) of geometric objects (points, lines, bars).
# The plot may also contain statistical transformations of the data and is drawn on a specific coordinate system.
# Faceting can be used to generate the same plot for different subsets of the dataset.
# It is the combination of these independent components that make up a graphic.
# Geometric objects, --geoms-- for short, represent what you actually see on the plot (points, lines, polygons, etc.)
# Statistical transformations, --stats-- for short, summarise data in many useful ways.
# The scales map values in the data space to values in an aesthetic space, whether it be colour, or size, or shape.
# Scales draw a legend or axes, which provide an inverse mapping to make it possible to read the original data values from the graph.
# A coordinate system, coord for short, describes how data coordinates are mapped to the plane of the graphic. 
# It also provides axes and gridlines to make it possible to read the graph.
# Types : Cartesian coordinates, Polar coordinates, Map projections
# A faceting specification describes how to break up the data into subsets and how to display those subsets as small multiples. 
# This is also known as conditioning or latticing/trellising.

# Components of Plots:
#     1)data
#     2)Aesthetics
#     3)Geometric objects
#     4)Statistical Transformations
#     5)Position Adjustments
#     6)Facets
#     7)scales
#     8)Coordinates

# FORMAT : ggplot() + layers()

install.packages("ggExtra")
library(ggplot2)
library(ggthemes)
options(scipen=999)  # turn-off scientific notation like 1e+48

# SCATTERPLOT
scatterplot <- ggplot(midwest, aes(x=area, y=poptotal)) + 
    geom_point(aes(col=state, size=popdensity)) + 
    geom_smooth(method="loess", se=F) + 
    xlim(c(0, 0.1)) + 
    ylim(c(0, 500000)) + 
    labs(subtitle="Area Vs Population", y="Population", x="Area", title="Scatterplot", caption = "Source: midwest")
plot(scatterplot)

# JITTERPLOT
jitterplot <- ggplot(mpg, aes(cty, hwy))+ 
    geom_jitter(width = .5, size=1) +
    labs(subtitle="mpg: city vs highway mileage", y="hwy", x="cty", title="Jittered Points")
plot(jitterplot)

# COUNTPLOT
countplot <- ggplot(mpg, aes(cty, hwy))+ 
    geom_count(col="tomato3", show.legend=F) +
    labs(subtitle="mpg: city vs highway mileage", y="hwy", x="cty", title="Counts Plot")
plot(countplot)

# BUBBLEPLOT
bubbleplot <- ggplot(mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ], aes(displ, cty)) + 
    geom_jitter(aes(col=manufacturer, size=hwy)) + 
    geom_smooth(aes(col=manufacturer), method="lm", se=F) +
    labs(subtitle="mpg: Displacement vs City Mileage", title="Bubble chart")
plot(bubbleplot)

# HISTOGRAM ON CONTINUOUS VARIABLE
histogram_continuous <- ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral")+ 
    geom_histogram(aes(fill=class), binwidth = .1, col="black", size=.1) +
    # geom_histogram(aes(fill=class), bins=5, col="black", size=.1)
    labs(title="Histogram with Auto Binning", subtitle="Engine Displacement across Vehicle Classes")
    # labs(title="Histogram with Fixed Bins", subtitle="Engine Displacement across Vehicle Classes")
plot(histogram_continuous)

# HISTOGRAM ON CATEGORICAL VARIABLES
histogram_categorical <- ggplot(mpg, aes(manufacturer))+ 
    geom_bar(aes(fill=class), width = 0.5) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable", subtitle="Manufacturer across Vehicle Classes")
plot(histogram_categorical)

# DENSITY PLOT
densityplot <- ggplot(mpg, aes(cty))+ 
    geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
    labs(title="Density plot", subtitle="City Mileage Grouped by Number of cylinders",caption="Source: mpg",x="City Mileage",fill="# Cylinders")
plot(densityplot)

# BOXPLOT
boxplot <- ggplot(mpg, aes(class, cty))+ 
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", subtitle="City Mileage grouped by Class of vehicle",caption="Source: mpg",x="Class of Vehicle",y="City Mileage")
plot(boxplot)

# DOTPLOT
dotplot <- ggplot(mpg, aes(manufacturer, cty)) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = .5, fill="red") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Dot plot", subtitle="City Mileage vs Class: Each dot represents 1 row in source data",caption="Source: mpg",x="Class of Vehicle",y="City Mileage")
plot(dotplot)

# TUFTEPLOT
tufteplot <- ggplot(mpg, aes(manufacturer, cty)) + 
    geom_tufteboxplot() + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Tufte Styled Boxplot", subtitle="City Mileage grouped by Class of vehicle",caption="Source: mpg",x="Class of Vehicle",y="City Mileage")
plot(tufteplot)

# VIOLINPLOT
violinplot <- ggplot(mpg, aes(class, cty))+ 
    geom_violin() + 
    labs(title="Violin plot", subtitle="City Mileage vs Class of vehicle",caption="Source: mpg",x="Class of Vehicle",y="City Mileage")
plot(violinplot)

# POPULATION PYRAMID
email_campaign_funnel <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/email_campaign_funnel.csv")
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

populationpyramid <- ggplot(email_campaign_funnel, aes(x = Stage, y = Users, fill = Gender)) + 
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks, labels = lbls) + # Breaks & Labels
    coord_flip() +  # Flip axes
    labs(title="Email Campaign Funnel") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5), axis.ticks = element_blank()) +   # Centre plot title
    scale_fill_brewer(palette = "Dark2")  # Color palette
plot(populationpyramid)








