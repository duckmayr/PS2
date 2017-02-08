
## PS2 test of all functions
## While not proper automated testing, running these commands will
## demonstrate whether the functions operate as desired.

# First read in the functions:
rm(list=ls())
source('benfords.R')
source('print.benfords.R')
source('write.csv.benfords.R')

# Then create some test data:

testVector <- rep(9, 100)
testMatrix <- matrix(rep(9, 100), nrow=10)

# Then see if the functions run without complaint

(vecTest1 <- benfords(testVector)) # See if a vector works with default type
(vecTest2 <- benfords(testVector, type='m')) # see if it works with 'm' type
(vecTest3 <- benfords(testVector, type='d')) # see if it works with 'd' type
(matTest1 <- benfords(testMatrix)) # See if a matrix works with default type
(matTest2 <- benfords(testMatrix, type='m')) # see if it works with 'm' type
(matTest3 <- benfords(testMatrix, type='d')) # see if it works with 'm' type

# If any of the following statements returns FALSE something is broken:

round(vecTest1$M, 3) == 0.954
round(vecTest1$D, 3) == 1.036
'digit.distribution' %in% names(vecTest1)
'digit.distribution' %in% names(vecTest2)
'digit.distribution' %in% names(vecTest3)
!('D' %in% names(vecTest2))
!('M' %in% names(vecTest3))
round(matTest1$M, 3) == 0.954
round(matTest1$D, 3) == 1.036
'digit.distribution' %in% names(matTest1)

# See if print.benfords() works with output from benfords()
vecTest_table1 <- print.benfords(vecTest1) # befords() output with both tests
vecTest_table2 <- print.benfords(vecTest2) # befords() output with m test
vecTest_table3 <- print.benfords(vecTest3) # befords() output with d test
# See if print.benfords() works with raw data:
vecTest_table4 <- print.benfords(testVector)
# Note I'm no longer testing the matrices; if they worked in benfords(),
# it will work here too since it just calls benfords()

# If any of the following statements returns FALSE something is broken:

all(c("note", "Leemis' m", "Cho-Gains' d") == names(vecTest_table1))
all(c("note", "Leemis' m") == names(vecTest_table2))
all(c("note", "Cho-Gains' d") == names(vecTest_table3))
all(c("note", "Leemis' m", "Cho-Gains' d") == names(vecTest_table4))
vecTest_table1$`Leemis' m` == "0.954242509439325*"
vecTest_table2$`Leemis' m` == "0.954242509439325*"
vecTest_table3$`Cho-Gains' d` == "1.03630998450624"
vecTest_table4$`Cho-Gains' d` == "1.03630998450624"

# Finally we see if it writes CSVs correctly

write.csv.benfords(vecTest1, 'vecTest1.csv')
write.csv.benfords(vecTest2, 'vecTest2.csv')
write.csv.benfords(vecTest3, 'vecTest3.csv')
write.csv.benfords(vecTest_table1, 'vecTest_table1.csv')
write.csv.benfords(vecTest_table2, 'vecTest_table2.csv')
write.csv.benfords(vecTest_table3, 'vecTest_table3.csv')
write.csv.benfords(testVector, 'testVector.csv')

vecTest1CSV <- read.csv('vecTest1.csv')
vecTest2CSV <- read.csv('vecTest2.csv')
vecTest3CSV <- read.csv('vecTest3.csv')
vecTest_table1CSV <- read.csv('vecTest_table1.csv')
vecTest_table2CSV <- read.csv('vecTest_table2.csv')
vecTest_table3CSV <- read.csv('vecTest_table3.csv')
testVectorCSV <- read.csv('testVector.csv')

View(vecTest1CSV)
View(vecTest2CSV)
View(vecTest3CSV)
View(vecTest_table1CSV)
View(vecTest_table2CSV)
View(vecTest_table3CSV)
View(testVectorCSV)
