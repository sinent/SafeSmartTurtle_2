print ("Testing is starting...")
firstIndex <-1
lastIndex <-100

conditions = c('count', 'intersection')
grid_sizes = c('grid 10', 'grid 20')
DSLs = c('new', 'old')
file_name = 'fault_detection_test_nums.txt'
file_name_2 = 'gen_input_times.txt'

for (c in conditions){
    for (g in grid_sizes){
        test_num_path_old = sprintf("%s//%s//%s//%s", c, DSLs[2], g, file_name_2 )
        test_num_path_new = sprintf("%s//%s//%s//%s", c, DSLs[1], g, file_name_2 )
        print(test_num_path_old)
        print(test_num_path_new)
        data_old <- read.table(test_num_path_old, sep = "" , header = F )[(firstIndex:lastIndex),1]
        data_new <- read.table(test_num_path_new, sep = "" , header = F )[(firstIndex:lastIndex),1]
        
        # both_data = c(data_new, data_old)
        # levels = c( rep("new", length), rep("old", length) )
        # my_data <- data.frame(both_data, levels)
        # print(kruskal.test(both_data ~ levels, data = my_data))

        print(wilcox.test(data_new, data_old,  alternative="greater")) # means alternative hypothesis := the first is greater than the second

    }   
}

##*************************************************************************************************************##
# print("Testing improvements in the number of executed tests:")

# testNum_NoFilter <- read.table("NoFilter//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# testNum_F1 <- read.table("F1//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# testNum_F2 <- read.table("F2//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# testNum_F3 <- read.table("F3//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]

# wilcox.test(testNum_NoFilter,testNum_F1, alternative="greater")  # Alternative: first data is significantly greater than the second
# wilcox.test(testNum_NoFilter,testNum_F2, alternative="greater")
# wilcox.test(testNum_NoFilter,testNum_F3, alternative="greater")

# wilcox.test(testNum_F1, testNum_NoFilter, alternative="greater")
# wilcox.test(testNum_F2, testNum_NoFilter, alternative="greater")
# wilcox.test(testNum_F3, testNum_NoFilter, alternative="greater")

##*************************************************************************************************************##

##*************************************************************************************************************##
# print("Testing improvements in the number failed shrink steps:")

# FailedShrinkSteps_NoFilter <- read.table("NoFilter//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# FailedShrinkSteps_F1 <- read.table("F1//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# FailedShrinkSteps_F2 <- read.table("F2//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# FailedShrinkSteps_F3 <- read.table("F3//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]

# wilcox.test(FailedShrinkSteps_NoFilter,FailedShrinkSteps_F1, alternative="greater") # means alternative hypothesis is that the first is greater than the second
# wilcox.test(FailedShrinkSteps_NoFilter,FailedShrinkSteps_F2, alternative="greater")
# wilcox.test(FailedShrinkSteps_NoFilter,FailedShrinkSteps_F3, alternative="greater")

# wilcox.test(FailedShrinkSteps_F1, FailedShrinkSteps_NoFilter, alternative="greater")
# wilcox.test(FailedShrinkSteps_F2, FailedShrinkSteps_NoFilter, alternative="greater")
# wilcox.test(FailedShrinkSteps_F3, FailedShrinkSteps_NoFilter, alternative="greater")

##*************************************************************************************************************##
# print("Testing improvements in the number of shrink steps:")

# ShrinkSteps_NoFilter <- read.table("NoFilter//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# ShrinkSteps_F1 <- read.table("F1//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# ShrinkSteps_F2 <- read.table("F2//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# ShrinkSteps_F3 <- read.table("F3//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]

# t.test(ShrinkSteps_NoFilter,ShrinkSteps_F1, alternative="greater")
# t.test(ShrinkSteps_NoFilter,ShrinkSteps_F2, alternative="greater")
# t.test(ShrinkSteps_NoFilter,ShrinkSteps_F3, alternative="greater")

# t.test(ShrinkSteps_F1,ShrinkSteps_NoFilter, alternative="greater")
# t.test(ShrinkSteps_F2,ShrinkSteps_NoFilter, alternative="greater")
# t.test(ShrinkSteps_F3,ShrinkSteps_NoFilter, alternative="greater")

##*************************************************************************************************************##