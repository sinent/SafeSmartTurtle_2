
print ("Testing normality is starting...")

#********************************************* Reading the inputs from file ***********************************************
firstIndex <-1
lastIndex <-100
length = lastIndex - firstIndex +1

firstIndex <-1
lastIndex <-100
length = lastIndex - firstIndex +1

conditions = c('count', 'intersection')
grid_sizes = c('grid 10', 'grid 20')
DSLs = c('new', 'old')
file_name = 'fault_detection_test_nums.txt'
file_name_2 = 'gen_input_times.txt'

for (c in conditions){
    for (d in DSLs){
        for (g in grid_sizes){
            test_num_path = sprintf("%s//%s//%s//%s", c, d, g, file_name_2 )
            data <- read.table(test_num_path, sep = "" , header = F )[(firstIndex:lastIndex),1]
            print(test_num_path)
            print(shapiro.test(data))
            print("Done!")
        }   
    }
}

# for (c in conditions){
#     for (g in grid_sizes){
#         test_num_path_new = sprintf("%s//%s//%s//%s", c, DSLs[1], g, file_name )
#         test_num_path_old = sprintf("%s//%s//%s//%s", c, DSLs[2], g, file_name )
#         data_new <- read.table(test_num_path_new, sep = "" , header = F )[(firstIndex:lastIndex),1]
#         data_old <- read.table(test_num_path_old, sep = "" , header = F )[(firstIndex:lastIndex),1]

#         both_data = c(data_new, data_old)
#         levels = c( rep("new", length), rep("old", length) )
#         my_data <- data.frame(both_data, levels)
#         print(kruskal.test(both_data ~ levels, data = my_data))
#     }   
# }

# testNum_NoFilter <- read.table("NoFilter//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# testNum_F1 <- read.table("F1//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# testNum_F2 <- read.table("F2//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# testNum_F3 <- read.table("F3//Raw//TestsNum.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]

# FailedShrinkSteps_NoFilter <- read.table("NoFilter//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# FailedShrinkSteps_F1 <- read.table("F1//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# FailedShrinkSteps_F2 <- read.table("F2//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# FailedShrinkSteps_F3 <- read.table("F3//Raw//FailedShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]

# ShrinkSteps_NoFilter <- read.table("NoFilter//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# ShrinkSteps_F1 <- read.table("F1//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# ShrinkSteps_F2 <- read.table("F2//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
# ShrinkSteps_F3 <- read.table("F3//Raw//ShrinkSteps.txt", sep = "" , header = F )[(firstIndex:lastIndex),2]
#****************************************************************************************************************************

#************************************************* Testing the normality of the data ****************************************

# shapiro.test(testNum_NoFilter)
# shapiro.test(testNum_F1)
# shapiro.test(testNum_F2)
# shapiro.test(testNum_F3)

# shapiro.test(ShrinkSteps_NoFilter)
# shapiro.test(ShrinkSteps_F1)
# shapiro.test(ShrinkSteps_F2)
# shapiro.test(ShrinkSteps_F3)

# shapiro.test(FailedShrinkSteps_NoFilter)
# shapiro.test(FailedShrinkSteps_F1)
# shapiro.test(FailedShrinkSteps_F2)
# shapiro.test(FailedShrinkSteps_F3)

# print ("Testing normality is done.")
#****************************************************************************************************************************


# # multi-group significance testing - Kruskal wallis
# levels = c(rep("NoFilter", length), rep("F1", length), rep("F2", length), rep("F3", length) )
# testNums   = c(testNum_NoFilter, testNum_F1, testNum_F2, testNum_F3)
# shrinkSucc = c(ShrinkSteps_NoFilter, ShrinkSteps_F1, ShrinkSteps_F2, ShrinkSteps_F3)
# shrinkFail = c(FailedShrinkSteps_NoFilter, FailedShrinkSteps_F1, FailedShrinkSteps_F2, FailedShrinkSteps_F3)

# my_data <- data.frame(testNums, levels)
# kruskal.test(testNums ~ levels, data = my_data)

# my_data <- data.frame(shrinkSucc, levels)
# kruskal.test(shrinkSucc ~ levels, data = my_data)

# my_data <- data.frame(shrinkFail, levels)
# kruskal.test(shrinkFail ~ levels, data = my_data)
