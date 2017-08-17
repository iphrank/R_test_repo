
library(pryr)
library(tidyverse)
library(lazyeval)
library(rlang)
library(magrittr) # needed to include the pipe operators
library(lubridate)

data("mtcars")
load(mtcars)

p <- c("mpg", "cyl", "disp")
p <- c(mtcars$gear, mtcars$cyl, mtcars$carb)

p <- mtcars$cyl
p2 <- p
prop.table(table(p2))


substitute() # promise captures the expression needed to compute the value and the environment in which to compute it.
# doesnot work the same in function env and global env
subs() # pryr
class(quote(mpg)) # "name"
parse(text = ) #
class(parse(text = "5+5")) # "expression"
deparse() # nse to string
eval() # evaluate nse

deparse(substitute())
eval(substitute())
eval(quote())
evalq()

#
select <- function(df, vars) {
  vars <- substitute(vars)
  var_pos <- setNames(as.list(seq_along(df)), names(df))
  pos <- eval(vars, var_pos)
  df[, pos, drop = FALSE]
}
select(mtcars, -cyl)

#
subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r, ]
}
x <- 4
subset2(sample_df, a == x)

#
f <- function(x) substitute(x)
g <- function(x) deparse(f(x))
g(1:10) ## x
g(x) ## x
g(x + y ^ 2 / z + exp(a * sin(b))) ## x

#
g <- function(x) deparse(substitute(x))
g(1:10) ## [1] "1:10"
g(x) ## [1] "x"
g(x + y^2) ## [1] "x + y^2"

# first capture vars <- substitute(vars)
f <- function(x) {
    x <- substitute(x)
    print(x)
    #prop.table(table(x))
}

p <- c(mtcars$gear, mtcars$cyl, mtcars$carb)
p <- c(quote(mtcars$gear), quote(mtcars$cyl), quote(mtcars$carb))
p <- substitute(p); p

eval(subs(lapply(p, f(xx))))

eval(subs(lapply(p, function(xx) prop.table(table(xx)))))
lapply(p, function(x) {x <- subs(x); prop.table(table(x))}))


# subs {pryr}
a <- 1; b <- 2
substitute(a + b + z) #> a + b + z
subs(a + b + z) ## 1 + 2 + z

f <- function() {
  a <- 1; b <- 2
  substitute(a + b + z)
}
f() ## 1 + 2 + z

subs(a + b, list(a = "y", b = quote(x))) ## "y" + x
subs(a + b, list(a = quote(y()))) ## y() + b
subs(y <- y + 1, list(y = 1)) ## 1 <- 1 + 1

subs(a + b, list("+" = quote(f))) ## f(a, b)
subs(a + b, list("+" = quote(`*`))) ## a * b


x <- quote(mpg)
y <- quote(disp)
d <- quote(mtcars)
f_interp(subs(xyplot(x ~ y, data = d))) ## xyplot(mpg ~ disp, data = mtcars))

xyplot2 <- function(x, y, data = data) {
  substitute(xyplot(x ~ y, data = data))
}
xyplot2(mpg, disp, data = mtcars) ## xyplot(mpg ~ disp, data = mtcars)
xyplot2(x, y, data = mtcars)

xyplot3 <- function(x, y, ...) {
  substitute(xyplot(x ~ y, ...))
}
xyplot3(mpg, disp, data = mtcars, col = "red", aspect = "xy")
## xyplot(mpg ~ disp, data = mtcars, col = "red", aspect = "xy")



######
# library(lazyeval) # interp
f0 <- function(df, grpvar, uniqvar) {
    df %>%
        group_by_(grpvar) %>%
        summarise_(n_uniq = interp(~n_distinct(v), v = as.name(uniqvar)) ) %>%
        filter(n_uniq > 1)
}

# dplyr
f1 <- function(df, grpvar, uniqvar) {
   grpvar <- enquo(grpvar)
   uniqvar <- enquo(uniqvar)

   df %>%
       group_by(!!grpvar) %>%
       summarise(n_uniq = n_distinct(!!uniqvar)) %>%
       filter(n_uniq >1)
}

res2 <- f1(iris, Sepal.Length, Sepal.Width)
res1 <- f0(iris, "Sepal.Length", "Sepal.Width")
identical(res1, res2) ##[1] TRUE


# library(lazyeval)
df <- data.frame(v1 = 1:5, v2 = 6:10, v3 = c(rep("A", 3), rep("B", 2)))

key <- "v3"; val <- "v2"; drp <- "v1"
df2 <- tbl_df(df) %>%
  select(-matches(drp)) %>%
  group_by_(key) %>%
  summarise_(sum_val = interp(~sum(var, na.rm = TRUE), var = as.name(val)))

# library(rlang)
key <- quote(v3); val <- quo(v2)
df %>%
    select(-matches("v1")) %>%
    group_by(!!key) %>%
    summarise(sum(!!val, na.rm = TRUE))

quote(3+3)
!!quote(3+3)
key <- quote(v3)
!!key
key <- quo(v3)
paste(!!key, "!")

# quo (or quos for multiple) is for unquoted variable names, not strings.
# To convert strings to quosures use sym (or syms),
# and use !! or !!! as appropriate to unquote or unquote-splice:
# library(dplyr)
#
# quo() results in ~<expression>, quos() for multiples
# quote() results in <expression>
# sym(), syms()

my_data  <- data_frame(foo = 0:10, bar = 10:20, meh = 20:30)
my_nse_quo <- quos(c(newbar = bar, newfoo = foo))
my_nse_quote <- quo(c(newbar = bar, newfoo = foo))

# For strings to symbols
subset_se <- function(df, ...){
     df %>% select(!!!rlang::syms(...))
}
my_newnames  <-  c("newbar" = "bar", "newfoo" = "foo") # rename colnames
my_newnames2  <-  c("bar", "foo")
subset_se(my_data, my_newnames)

# For unquoted variable names,
subset_nse <- function(df, ...){
    df %>% select(!!!quos(...))
}
subset_nse_ <- . %>% {
    df %>% rlang::quos() %>% select()
}
subset_nse_(bar)
subset_nse(my_data, newbar = bar, newfoo = foo)
move_stuff_nse(my_data,my_nse_quo)

my_nse_quo
my_nse_quote
eval(my_nse_quote)

cname <- NA
test <- function(...) {
  quosures <- rlang::quos(...)
  print(quosures)
  cname <<- lapply(names(eval(quosures)), function(x) {
    print(x)
  })
}

test <- function(...) {
  cname <<- lapply(names(rlang::quos(...)), function(x) {
    print(x)
  })
}

test <- function(...) {
  cname <<- lapply(select(!!!quos(...)), function(x) {
    print(x)
  })
}

test(df)
cname

subset_nse <- function(df, ...){
    df %>% select(!!!quos(...))
}
subset_nse(my_data, newbar = bar, newfoo = foo)

sst <- function(...) {
    lit <- substitute(lapply(c(...), function(x) prop.table(table(x))))
    eval(lit)
}
sst(train$Survived, titanic.train$Survived, titanic.test$Survived)

sst <- function(...) {
    for(i in seq_along(...)) {
        #substitute(lapply(c(...), function(x) prop.table(table(x))))
        f <- substitute(i %>% table() %>% prop.table())
        t <- paste(f, t, sep = "\n")
    }
    t
    #eval(lit)
}
sstt <- sst(train$Survived, titanic.train$Survived, titanic.test$Survived)
eval(sstt)

# purrr::map())
calc_sum <- function(vars) {
    sum(vars, na.rm = TRUE)
}

mapWithRename <- function(...) {
  x <- map(...)
  names(x) <- paste0(names(x), "Rename")
  x
}

dat <- tibble(
  variable_1 = c(1:5, NA, NA, NA, NA, NA),
  variable_2 = c(NA, NA, NA, NA, NA, 11:15)
)
dat
dat %>% mapWithRename(calc_sum)


# Split into pieces, fit model to each piece, then predict
by_cyl <- mtcars %>% split(.$cyl) # split to list of df by ...
mods <- by_cyl %>% map(~ lm(mpg ~ wt, data = .)) # lapply
mods2 <- by_cyl %>% map(~ table() %>% prop.table())
map2(mods, by_cyl, predict)

# Split into pieces, fit model to each piece, then predict
by_cyl <- mtcars %>% split(.$train) # split to list of df by ...
mods <- by_cyl %>% map(~ lm(mpg ~ wt, data = .)) # lapply
mods2 <- by_cyl %>% map(~ table() %>% prop.table())
map2(mods, by_cyl, predict)

# dot/pipe functions
# . %>% RHS
read_year <- . %>% as.character %>% as.Date %>% year

# Creating a dataset
df <- data.frame(now = "2015-11-11", before = "2012-01-01")
df2 <- data.frame(now = "2014-11-11", before = "2013-01-01")
dfl <- list(df, df2)

# Example 1: applying `read_year` to a single character-vector
df$now %>% read_year # [1] 2015

# Example 2: applying `read_year` to all columns of `df`
dfl %>% lapply(read_year) %>% as.data.frame  # implicit `lapply(df, read_year)
df %>% mutate_all(funs(read_year)) # same as above using `mutate_all`
#    now before
# 1 2015   2012
dfl %>% mutate_all(funs(read_year)) # same as above using `mutate_all`

# We can review the composition of the function by typing its name or using functions:
read_year
read_year[[2]]


# extract feature
iris %>% subset(select = "Species") %>% head() # return data.frame
iris %>% `[[`("Species") %>% head() # returns vector
iris %$% Species %>% head() # returns vector

# dot placeholder, when not used as first parameter
iris %>% {
    if (nrow(.) > 0)
    rbind(head(., 1), tail(., 1))
    else .}

# “exposition” pipe operator %$%
# exposes the names within the LHS object to RHS expression
df %$% now %>% read_year # %$% extract feature
dfl %$% .[[2]]$before
iris %$% ts.plot(Sepal.Length)
iris %$% list(quantile(Sepal.Length), quantile(Sepal.Width))# apply function

# !!!!!!!!
iris %$% list(Sepal.Length, Sepal.Width) %>% map(quantile) # apply function


# !!!!!!!!!
#
#
iris %$% names(iris)[-5] %>% subset(iris, select = .) %>% map(quantile)
qtl <- . %$% names(iris)[-5] %>% subset(iris, select = .) %>% map(quantile)

map(ll, qtl)


# !!!!!!!!!
ll <- list(iris, iris)
tt <- list(train, train)
#
#
toDF <- . %>% as.data.frame
qtl2 <- . %>% toDF %$% .[,sapply(., is.numeric)] %>% map(quantile)
prop <- . %>% toDF %$% .[,sapply(., is.factor)] %>% print()

# find NA's from list of tibbles/data.frames
toDF <- . %>% as.data.frame
isNA <- . %>% map(is.na) %>% map(as.integer) %>% map(mean) %>% unlist %>% toDF
isGT <- . %>% rownames_to_column(var = "feature") %>% filter(.[] > 0)
isNUM <- . %$% .[,sapply(., is.numeric)]
isFCT <- . %$% .[,sapply(., is.factor)]
isLGL <- . %$% .[,sapply(., is.logical)]

numNAs <- . %>% toDF %>% isNUM %>% isNA %>% isGT
factNAs <- . %>% toDF %>% isFCT %>% isNA %>% isGT
logNAs <- . %>% toDF %>% isLGL %>% isNA %>% isGT
map(tt, numNAs); map(tt, factNAs); map(tt, logNAs)

######################################

# find proportions per feature
toDF <- . %>% as.data.frame
isTBL <- . %>% map(addmargins(., FUN = sum))
toTIB <- . %>% map(as.tibble) # %>% index_split(.)
toTIB2 <- . %>% toTIB #%>% cbind(.[1], .[2]) # map(names(.), FUN) %>%  #
BIND <- . %>% cbind(.[1]) %>% head
prop <- . %>% toDF %$% .[,sapply(., is.factor)] %>% toDF %>% map(table) %>% map(prop.table) %>% toTIB2 %>% BIND
map(tt, prop)
# train vs test; stratification test
# plot, table list[[1]], list[[2]]
# combine each item
map(ll, prop)

# https://www.rdocumentation.org/packages/rlang/versions/0.0.0.9000/topics/as_name
map(names(iris), as_name) %>% as.vector()
reduce(cbind)
merge(tt[[1]], tt[[2]])

# get indices
which(tt %in% 1:100)
list_along(tt) %>% unlist
str(tt)
attr(tt, which(names))
names(tt)
index2vec(tt)

# combine first[1] and second[2]
ll %>% cbind(.[[1]][1], .[[2]][1]) %>% head
table(cbind(tt[[1]][1], tt[[2]][1]))

split(df, interaction(df$a, df$b))

a <- list(1:3, 3:5, 3:7); b <- c(1:10)
g <- rep(seq_along(a), sapply(a, length)); g
sapply(b, function(x) g[which(unlist(a) %in% x)])

#
toDF <- . %>% as.data.frame
qtl2 <- . %>% toDF %$% .[,sapply(., is.numeric)] %>% map(quantile)
map(ll, qtl2)
map(tt, qtl2) # Error in eval(substitute(expr), data, enclos = parent.frame()) :
              # invalid 'envir' argument of type 'closure'
#
#
#

tt <- map(tt, toDF)
str(ll)
qtl2
prop
nas
tdfTodf

library(lazyeval)
interp(~x+y, x = 2)

# plyr - works
ddply(iris, names(iris)[5], summarize, value = mean(Sepal.Length))




# get vector of features with NA
tblNA <- function(df) {
    isNA <- lapply(df, is.na)
    sumNA <- lapply(isNA, sum)
    unlist(sumNA[sumNA > 0])
}
tblNA(trainRaw)

# split NSE df$name in "df", "name"
splitFeature <- function(q) {
    r <- as.character(match.call())[2]
    df <- str_extract(as.name(r), "\\w+")
    df_ <- str_extract(as.name(r), "\\w+\\$")
    name <- str_extract(as.name(r), "\\w+$")
    list(df = df, df_ = df_, name = name)
}

NAfact <- function(response) {
    df <- str_extract(as.name(response), "\\w+")
    r <- response
    function(feature) {
        nse <- parse(text = feature)
        tbl <- prop.table(table(r, is.na(eval(nse))))
        name <- str_extract(feature, "\\w+$")
        rownames(tbl) <- c("Died", "Survived")
        colnames(tbl) <- c(name, "NA")
        sqsm <- function(x) sum(x)^2
        marg2 <- round(ftable(sweep(addmargins(tbl, 1,
                        list(list(Sum = sum, Prop = sqsm))), 2,
                        apply(tbl, 2, sum)/100, "/")), 1)
    }
}

# list NA matrices and proportions
NAlist <- splitFeature(trainRaw$Survived)$df_ %>%
     paste0(names(tblNA(trainRaw))) %>%
     lapply(NAfact(trainRaw$Survived)) %>%
     print()

NAfeatures <- names(tblNA(trainRaw))

table(train$Survived, train$Pclass)

length(tt)
lapply(tt, head)
map(tt, head)


https://www.rdocumentation.org/packages/mlr/versions/2.10
#
p <- 0.5
log2(1)
entropy <- sum(p*log2(p)); entropy

fv = generateFilterValuesData(iris.task, method = "information.gain")
fv2 = generateFilterValuesData(iris.task, method = c("information.gain", "chi.squared"))
fv2$data



