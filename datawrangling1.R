#philips #akzo #van houte #unilever
refineOriginal3 <- refine %>% mutate_each(funs(replace(., grepl('ill', .) | grepl('hlip', .) , 'philips')), company)

#
refineOriginal3 <- refine %>% mutate_each(funs(replace(., grepl('AKZO', .) | grepl('ackz', .) | grepl('z0', .) | grepl('ak ', .) | grepl('Akzo', .), 'akzo')), company)

#
refineOriginal3 <- refine %>% mutate_each(funs(replace(., grepl('Van', .) | grepl('Houten', .) | grepl(' Houten', .) | grepl('Van Houten', .) | grepl('V', .) | grepl('H', .), 'van houten')), company)
refineOriginal3 <- refine %>% mutate_each(funs(replace(., grepl('vanhouten', .), 'van houten')), company)

#
refineOriginal3 <- refine %>% mutate_each(funs(replace(., grepl('U', .) | grepl('uni', .) | grepl('Unilever', .) | grepl('lve', .), 'unilever')), company)

#Unilever
refineOriginal3$company <- gsub('U', 'u', refineOriginal3$company)

refineOriginal3$company <- gsub('A', 'a', refineOriginal3$company)

refineOriginal3$company <- gsub(' ', '', refineOriginal3$company)

refineOriginal3$company <- gsub('V', 'v', refineOriginal3$company)

refineOriginal3$company <- gsub('H', 'h', refineOriginal3$company)

#split columns
refineoriginal6a <- within(df, Product.code...number<-data.frame(do.call('rbind', strsplit(as.character(Product.code...number), '-', fixed=TRUE))))
refineOriginal4b <- transform(refineOriginal4, Product.Code...number=do.call(rbind, strsplit(refineOriginal4$Product.code...number, '-', fixed=TRUE)), stringsAsFactors=F)

#rename columns
#product_code and product_number
names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
colnames(trSamp)[2] <- "newname2"

names(refineOriginal6a)[names(refineOriginal6a) == 'Product.code...number.a'] <- 'product_code'

colnames(refineOriginal6a)[3] <- "product_code"

#Change Factor to Character
bob <- data.frame(lapply(bob, as.character), stringsAsFactors=FALSE)

#Map Category Values - Add to New Column
refineOriginal4b$productcategory <- mapvalues(refineOriginal4b$test, from = c("p", "x", "q", "v"), to = c("Smartphone", "Laptop", "Tablet", "TV"))

#Create Dummy Variables (Binary)
for(level in unique(refineOriginal6a$company)){
  refineOriginal6a[paste("company", level, sep = "_")] <- ifelse(refineOriginal6a$company == level, 1, 0)
  }

#Flatten Data Frame
refineOriginal6aflatten <- flatten(refineOriginal6aflatten, recursive = TRUE)
