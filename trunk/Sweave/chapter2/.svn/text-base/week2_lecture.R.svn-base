###################################################
### chunk number 1: aa_SweaveListingsPreparations
###################################################
source('listingPreps.R');
options(prompt='>', continue=' ', width=40)
#.Rout$escapeinside="{(*@}{@*)}"
#.myRset$escapeinside="{(*@}{@*)}"
#SweaveListingoptions(Rset=.myRset, Rout=.Rout, intermediate = FALSE)
#SweaveListingPreparations()


###################################################
### chunk number 2: v1
###################################################
c('hello', TRUE); c(1, TRUE)
class(c('hello', TRUE)); class(c(1, TRUE));
c('Hello', 14);
class(c(TRUE, TRUE));


###################################################
### chunk number 3: v11
###################################################
my_name <- c('D', 'a', 'v', 'i',
             'd', ' ', 'R', '.');
length(my_name);
names(my_name);
names(my_name) <- 
  c(  paste('first', as.character(1:5),
          sep="_"), 
      'space',
      paste('last', as.character(1:2),
            sep="_") 
   );
my_name;


###################################################
### chunk number 4: v2
###################################################
temp_vec <- 1:5;
temp_vec
temp_vec <- c(temp_vec, 6);
temp_vec;
temp_vec2 <- c(temp_vec, 'Cat');
temp_vec2;
temp_vec[-2];
temp_vec[3] <- 100;
temp_vec;
temp_vec1 <- 1:5; temp_vec2 <- 7:10;
c(temp_vec1, 6, temp_vec2);


###################################################
### chunk number 5: index1
###################################################
my_name[1:3];
my_name[(1:3) * 2];
my_name[c(1, 5, length(my_name))];
my_name['first_3'];
my_name[c('first_1', 'last_1')]


###################################################
### chunk number 6: index2
###################################################
temp_vec <- 1:10;
temp_vec <= 5;
temp_vec[temp_vec <= 5];
my_name == 'D'
my_name[my_name == 'D'];
my_name[my_name == ' ' | my_name == '.'];
my_name[!(my_name %in% c(' ', '.'))];


###################################################
### chunk number 7: lists1
###################################################
list_ex <- list(my_name, 1:5, function(x) { return(1/x) });
list_ex;
list_ex2 <- list(name_ta=my_name, one_to_five=1:5, myfun = function(x) { return(1/x) });


###################################################
### chunk number 8: lists2
###################################################
list_ex2;
class(list_ex);
class(list_ex2[[1]]);


###################################################
### chunk number 9: dfoverview
###################################################
cost_per=c(0.10, 0.25, 0.50);
items=c('spam', 'egg', 'foobar');
on_hand=c(123, 153, 55);
df <- data.frame(items=items, on_hand=on_hand, cost_per=cost_per);
df
total_value <- df$on_hand * df$cost_per


###################################################
### chunk number 10: dfoverview1
###################################################
df <- cbind(df, total_value=total_value)
df
df2 <- cbind(df, weight_per=c(0.5, 0.1, 3))
df2


###################################################
### chunk number 11: dfoverview2
###################################################
df2 <- cbind(
      df2, 
      total_weight=df2$on_hand * df2$weight_per,
      value_density=df2$total_value/(df2$on_hand * df2$weight_per)
            )
df2
class(df2);
is.list(df2);


###################################################
### chunk number 12: complextypessyntax
###################################################
df <- data.frame(items=items, on_hand=on_hand, cost_per=cost_per);
list1 <- as.list(df);
list1;
names(df);
names(df)[1] <- c('My items');
df


###################################################
### chunk number 13: widen
###################################################
op <- options(); options(width=60)


###################################################
### chunk number 14: funcdoc01 eval=FALSE
###################################################
## help(ls);
## ?ls
## ?`+`  # use backquotes when R doesn't
##       #  like the question marks


###################################################
### chunk number 15: funcdoc02
###################################################
# help(package=RInterval); 
# truncated for space
cat(paste(help(package=RInterval)$info[[1]], collapse="\n"), '\n');


###################################################
### chunk number 16: funcdoc03
###################################################
help.search('digest');


###################################################
### chunk number 17: funcdoc04
###################################################
## help.start();


###################################################
### chunk number 18: funcdoc1
###################################################
example(is.integer);


###################################################
### chunk number 19: funcdoc2
###################################################
isTRUE;
lsf.str;


###################################################
### chunk number 20: unwiden
###################################################
options(op);


