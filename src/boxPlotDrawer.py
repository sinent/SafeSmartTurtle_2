import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import os
import random
import statistics

#drawing box plots of different grid sizes in one figure 
# run this with python3

def get_data_paper2(grid, filter, data):
    filePath = "exp1-SUT3/G-%d-A-5-W-5-D-5-R-100-GenD--SUT-randFilterOrCorrect/%s/Raw/%s.txt" %(grid, filter, data)
    f = open(filePath, "r")
    ans = []
    if data == "Fault Detection Steps":
        line = f.readline()
        while len(line.split())>0:
            ans.append(int(line.split()[0]))
            line = f.readline()
    else:  
        line = f.readline() # skipping the first line: '#turtle=X'
        line = f.readline()
        while len(line.split())>1:
            ans.append(int(line.split()[1]))
            line = f.readline()
    return ans

def drawPlot_paper2(targetData, ylabelText):
    # targetData is the foldername containing the data
    # it belongs to [TestsNum, ShrinkSteps, FailedShrinkSteps, Discards]

    def set_box_color(bp, color):
        plt.setp(bp['boxes'], color=color)
        plt.setp(bp['whiskers'], color=color)
        plt.setp(bp['caps'], color=color)
        plt.setp(bp['medians'], color=color)

    data_NoF =[]
    data_F1  =[]
    data_F2  =[]
    data_F3  =[]

    grids = [10,15,20,50]
    for g in grids:
        data_NoF.append( get_data_paper2(g, "NoFilter", targetData) )
        data_F1.append(  get_data_paper2(g, "F1", targetData) )
        data_F2.append(  get_data_paper2(g, "F2", targetData) )
        data_F3.append(  get_data_paper2(g, "F3", targetData) )

    ticks = ['10*10', '15*15', '20*20', '50*50']

    plt.figure()

    bpA = plt.boxplot(data_NoF, positions=np.array(range(len(data_NoF)))*3-0.75, sym='', widths=0.4)
    bpB = plt.boxplot(data_F1,  positions=np.array(range(len(data_F1)))*3-0.25, sym='', widths=0.4)
    bpC = plt.boxplot(data_F2,  positions=np.array(range(len(data_F2)))*3+0.25, sym='', widths=0.4)
    bpD = plt.boxplot(data_F3,  positions=np.array(range(len(data_F3)))*3+0.75, sym='', widths=0.4)

    set_box_color(bpA, '#0579D5') # colors are from http://colorbrewer2.org/
    set_box_color(bpB, '#ff6600')
    set_box_color(bpC, '#a6a6a6')
    set_box_color(bpD, '#ffc61a')

    # draw temporary red and blue lines and use them to create a legend
    plt.plot([], c='#0579D5', label='No Filter')
    plt.plot([], c='#ff6600', label='F1 Filter')
    plt.plot([], c='#a6a6a6', label='F2 Filter')
    plt.plot([], c='#ffc61a', label='F3 Filter')

    plt.xlabel('Grid Size', fontsize=14, fontweight='bold')
    plt.ylabel(ylabelText, fontsize=14, fontweight='bold')
    legend_properties = {'weight':'bold'} 
    plt.legend(prop=legend_properties) 

    # plt.legend()

    plt.xticks(range(0, len(ticks) * 3, 3), ticks)
    plt.tick_params(axis='x', labelsize=14)
    plt.xticks(weight = 'bold')
    plt.tick_params(axis='y', labelsize=14)
    plt.yticks(weight = 'bold')
    # plt.xlim(-2, len(ticks)*2)
    # plt.ylim(0, 8)

    plt.legend(bbox_to_anchor=(0,1.02,1,0.2), loc="lower left", prop = {'weight':'bold', 'size':11.5},
                mode="expand", borderaxespad=0, ncol=4)
            
    # plt.legend(prop={'size': 14})
    # legend_properties = {'weight':'bold'} 
    # plt.legend(prop=legend_properties) 

    plt.tight_layout()

    if(not os.path.isdir("exp1-SUT3/boxplots/")):
        os.mkdir("exp1-SUT3/boxplots/")
    plt.savefig("exp1-SUT3/boxplots/"+targetData + ".pdf")



def paper_2_box_plots():
    drawPlot_paper2("TestsNum", "Number of test cases")
    drawPlot_paper2("Discards", "Number of test cases")
    drawPlot_paper2("ShrinkSteps"      , "Number of steps" )
    drawPlot_paper2("FailedShrinkSteps", "Number of steps" )
    drawPlot_paper2("Fault Detection Steps", "Number of executed steps" )

def get_data_paper3(cs_condition, new_or_old, grid_size: int, test_num_or_time):
    """
    Parrameters:
    ------------
    cs_condition: 'count' or 'intersection'
    new_or_old: 'new' or 'old'
    grid_size: (int)
    test_num_or_time: 'test_num' or 'time'
    """

    if test_num_or_time == 'time':
        file_name = "gen_input_times.txt"
    else: # we have only time or test_num files
        file_name = "fault_detection_test_nums.txt"
    filePath = f"../exp-paper3/{cs_condition}/{new_or_old}/grid {grid_size}/{file_name}"
    # filePath = f"../exp-paper3"
    # if os.path.exists(filePath):
    #     print("yes")
    # else:
    #     print("no")
    f = open(filePath, "r")
    ans = []
    line = f.readline()
    while len(line.split())>0:
        ans.append(int(line.split()[0]))
        line = f.readline()
    return ans

def drawPlot_paper3(cs_condition, targetData, ylabelText):

    def set_box_color(bp, color):
        plt.setp(bp['boxes'], color=color)
        plt.setp(bp['whiskers'], color=color)
        plt.setp(bp['caps'], color=color)
        plt.setp(bp['medians'], color=color)

    data_old_dsl =[]
    data_new_dsl  =[]

    grids = [10,20]
    for g in grids:
        data_old_dsl.append( get_data_paper3(cs_condition, "old", g, targetData) )
        data_new_dsl.append( get_data_paper3(cs_condition, "new", g, targetData) )

    ticks = ['10*10', '20*20']

    plt.figure()

    bpB = plt.boxplot(data_old_dsl,  positions=np.array(range(len(data_old_dsl)))*2-0.25, sym='', widths=0.4) #?????
    bpC = plt.boxplot(data_new_dsl,  positions=np.array(range(len(data_new_dsl)))*2+0.25, sym='', widths=0.4)

    set_box_color(bpB, '#0579D5')   # colors are from http://colorbrewer2.org/ 
    set_box_color(bpC, '#ff6600')

    # draw temporary red and blue lines and use them to create a legend
    plt.plot([], c='#0579D5', label='Initial DSL')
    plt.plot([], c='#ff6600', label='Extended DSL')

    plt.xlabel('Grid Size', fontsize=14, fontweight='bold')
    plt.ylabel(ylabelText, fontsize=14, fontweight='bold')
    legend_properties = {'weight':'bold'} 
    plt.legend(prop=legend_properties) 

    # plt.legend()

    plt.xticks(range(0, len(ticks) * 2, 2), ticks)
    plt.tick_params(axis='x', labelsize=14)
    plt.xticks(weight = 'bold')
    plt.tick_params(axis='y', labelsize=14)
    plt.yticks(weight = 'bold')
    # plt.xlim(-2, len(ticks)*2)
    # plt.ylim(0, 8)

    plt.legend(bbox_to_anchor=(0,1.02,1,0.2), loc="lower left", prop = {'weight':'bold', 'size':11.5},
                mode="expand", borderaxespad=0, ncol=2)
            
    # plt.legend(prop={'size': 14})
    # legend_properties = {'weight':'bold'} 
    # plt.legend(prop=legend_properties) 

    plt.tight_layout()

    if(not os.path.isdir("../exp-paper3/boxplots/")):
        os.mkdir("../exp-paper3/boxplots/")
    plt.savefig(f"../exp-paper3/boxplots/{cs_condition}_{targetData}.pdf")


def drawPlot_paper3_2(targetData, ylabelText):

    def set_box_color(bp, color):
        plt.setp(bp['boxes'], color=color)
        plt.setp(bp['whiskers'], color=color)
        plt.setp(bp['caps'], color=color)
        plt.setp(bp['medians'], color=color)

    data_old_dsl =[]
    data_new_dsl  =[]

    grids = [10]
    for g in grids:
        data_old_dsl.append( get_data_paper3("count", "old", g, targetData) )
        data_new_dsl.append( get_data_paper3("count", "new", g, targetData) )
        data_old_dsl.append( get_data_paper3("intersection", "old", g, targetData) )
        data_new_dsl.append( get_data_paper3("intersection", "new", g, targetData) )

    ticks = ["A", "A'", "B", "B'"]

    plt.figure()


    bpB = plt.boxplot(data_old_dsl,  positions=np.array(range(len(data_old_dsl)))*2-0.25, sym='', widths=0.4)
    bpC = plt.boxplot(data_new_dsl,  positions=np.array(range(len(data_new_dsl)))*2+0.25, sym='', widths=0.4)

    set_box_color(bpB, '#0579D5')   # colors are from http://colorbrewer2.org/ 
    set_box_color(bpC, '#ff6600')

    # draw temporary red and blue lines and use them to create a legend
    plt.plot([], c='#0579D5', label='Initial DSL constraint')
    plt.plot([], c='#ff6600', label='Extended DSL constraint')

    plt.xlabel("Test scenario constraint", fontsize=14, fontweight='bold')
    plt.ylabel(ylabelText, fontsize=14, fontweight='bold')
    legend_properties = {'weight':'bold'} 
    plt.legend(prop=legend_properties) 

    # plt.legend()

    plt.xticks([-0.25, 0.25, 2-0.25, 2+0.25], ticks)  # todo : change this part for your figure
    # plt.xticks(range(0, len(ticks) * 2, 2), ticks)
    plt.tick_params(axis='x', labelsize=14)
    plt.xticks(weight = 'bold')
    plt.tick_params(axis='y', labelsize=14)
    plt.yticks(weight = 'bold')
    # plt.xlim(-2, len(ticks)*2)
    # plt.ylim(0, 8)

    plt.legend(bbox_to_anchor=(0,1.02,1,0.2), loc="lower left", prop = {'weight':'bold', 'size':11.5},
                mode="expand", borderaxespad=0, ncol=2)
            
    # plt.legend(prop={'size': 14})
    # legend_properties = {'weight':'bold'} 
    # plt.legend(prop=legend_properties) 

    plt.tight_layout()

    if(not os.path.isdir("../exp-paper3/boxplots_2/")):
        os.mkdir("../exp-paper3/boxplots_2/")
    plt.savefig(f"../exp-paper3/boxplots_2/{targetData}.pdf")


def drawPlot_questionnaire():

    ylabelText = "Usefulness prioritization"

    def set_box_color(bp, color):
        plt.setp(bp['boxes'], color=color)
        plt.setp(bp['whiskers'], color=color)
        plt.setp(bp['caps'], color=color)
        plt.setp(bp['medians'], color=color)

    ticks = ["C1", "C2", "C3", "C4", "C5", "C6", "C7"]

    repondents = 10 
    random_data = sum([[*range(1,8)] for i in range(repondents)],[])
    random.shuffle(random_data)

    # q1 = [random_data.pop() for _ in range(repondents)]
    # q2 = [random_data.pop() for _ in range(repondents)]
    # q3 = [random_data.pop() for _ in range(repondents)]
    # q4 = [random_data.pop() for _ in range(repondents)]
    # q5 = [random_data.pop() for _ in range(repondents)]
    # q6 = [random_data.pop() for _ in range(repondents)]
    # q7 = [random_data.pop() for _ in range(repondents)]

    """
    ---old DSL
    1-Pick two robots randomly and guide them to a random square area of 3*3 at the same random time.
    2-Pick two robots and three arena positions randomly and guide the robots to cross those positions (not necessarily at the same time).
    ---new DSL
    3-Pick two particular robots and guide them to a random square area of 3*3 at the same particular time.
    4-Pick two particular robots and three particular arena positions and guide the robots to cross those positions (not necessarily at the same times).
    5-Take all robots and start them with maximum speeds in random paths.
    6-Pick two robots randomly and make them enter a 5*5 area of the arena at the same random time which one position in that area has the minimum observability of the adjacent points.
    7-Pick three robots randomly and put them in sequence at the three initial positions of a random path and make them to follow that path. 
    """

    q1 = [1,1,1,2,2,2,3,3,4,6]
    q2 = [3,3,3,4,4,4,4,4,5,5]
    q3 = [1,1,1,1,1,2,2,2,6,6]
    q4 = [1,2,2,3,3,3,4,6,6,7]
    q5 = [4,5,6,7,7,7,7,7,7,7]
    q6 = [2,2,3,4,5,5,5,5,5,5]
    q7 = [1,3,4,5,6,6,6,6,7,7]

    all_data = [q1,q2,q3,q4,q5,q6,q7]
    for i in range(7):
        # print(f"avg[{i+1}]={sum(all_data[i])/10}")
        print(f"median[{i+1}]={statistics.median(all_data[i])}")

    plt.figure()

    # bp1 = plt.boxplot(q1, positions=[1], sym='', widths=0.5, showmeans=True, whis=(0, 100), mprops={"marker":"o","markerfacecolor":"blue", "markeredgecolor":"blue"})
    # bp2 = plt.boxplot(q2, positions=[2], sym='', widths=0.5, showmeans=True, whis=(0, 100), meanprops={"marker":"o","markerfacecolor":"blue", "markeredgecolor":"blue"})
    # bp3 = plt.boxplot(q3, positions=[3], sym='', widths=0.5, showmeans=True, whis=(0, 100), meanprops={"marker":"o","markerfacecolor":"orange", "markeredgecolor":"orange"})
    # bp4 = plt.boxplot(q4, positions=[4], sym='', widths=0.5, showmeans=True, whis=(0, 100), meanprops={"marker":"o","markerfacecolor":"orange", "markeredgecolor":"orange"})
    # bp5 = plt.boxplot(q5, positions=[5], sym='', widths=0.5, showmeans=True, whis=(0, 100), meanprops={"marker":"o","markerfacecolor":"orange", "markeredgecolor":"orange"})
    # bp6 = plt.boxplot(q6, positions=[6], sym='', widths=0.5, showmeans=True, whis=(0, 100), meanprops={"marker":"o","markerfacecolor":"orange", "markeredgecolor":"orange"})
    # bp7 = plt.boxplot(q7, positions=[7], sym='', widths=0.5, showmeans=True, whis=(0, 100), meanprops={"marker":"o","markerfacecolor":"orange", "markeredgecolor":"orange"})

    bp1 = plt.boxplot(q1, positions=[1], sym='', widths=0.5,  whis=(0, 100), medianprops={"marker":"*","markerfacecolor":"blue", "markeredgecolor":"blue"})
    bp2 = plt.boxplot(q2, positions=[2], sym='', widths=0.5,  whis=(0, 100), medianprops={"marker":"*","markerfacecolor":"blue", "markeredgecolor":"blue"})
    bp3 = plt.boxplot(q3, positions=[3], sym='', widths=0.5,  whis=(0, 100), medianprops={"marker":"*","markerfacecolor":"orange", "markeredgecolor":"orange"})
    bp4 = plt.boxplot(q4, positions=[4], sym='', widths=0.5,  whis=(0, 100), medianprops={"marker":"*","markerfacecolor":"orange", "markeredgecolor":"orange"})
    bp5 = plt.boxplot(q5, positions=[5], sym='', widths=0.5,  whis=(0, 100), medianprops={"marker":"*","markerfacecolor":"orange", "markeredgecolor":"orange"})
    bp6 = plt.boxplot(q6, positions=[6], sym='', widths=0.5,  whis=(0, 100), medianprops={"marker":"*","markerfacecolor":"orange", "markeredgecolor":"orange"})
    bp7 = plt.boxplot(q7, positions=[7], sym='', widths=0.5,  whis=(0, 100), medianprops={"marker":"*","markerfacecolor":"orange", "markeredgecolor":"orange"})


    # bp1 = plt.boxplot(q1, positions=[1], sym='', widths=0.5, whis=(0, 100))
    # bp2 = plt.boxplot(q2, positions=[2], sym='', widths=0.5, whis=(0, 100))
    # bp3 = plt.boxplot(q3, positions=[3], sym='', widths=0.5, whis=(0, 100))
    # bp4 = plt.boxplot(q4, positions=[4], sym='', widths=0.5, whis=(0, 100))
    # bp5 = plt.boxplot(q5, positions=[5], sym='', widths=0.5, whis=(0, 100))
    # bp6 = plt.boxplot(q6, positions=[6], sym='', widths=0.5, whis=(0, 100))
    # bp7 = plt.boxplot(q7, positions=[7], sym='', widths=0.5, whis=(0, 100))

    set_box_color(bp1, '#0579D5')   # colors are from http://colorbrewer2.org/ 
    set_box_color(bp2, '#0579D5')
    set_box_color(bp3, '#ff6600')
    set_box_color(bp4, '#ff6600')
    set_box_color(bp5, '#ff6600')
    set_box_color(bp6, '#ff6600')
    set_box_color(bp7, '#ff6600')

    # set_box_color(bp1, '#e41a1c') 
    # set_box_color(bp2, '#377eb8')
    # set_box_color(bp3, '#4daf4a')
    # set_box_color(bp4, '#984ea3')
    # set_box_color(bp5, '#ff7f00')
    # set_box_color(bp6, '#e6ab02')
    # set_box_color(bp7, '#a65628')

    # draw temporary red and blue lines and use them to create a legend
    plt.plot([], c='#0579D5', label='Initial DSL constraint')
    plt.plot([], c='#ff6600', label='Extended DSL constraint')

    plt.xlabel("Test scenario constraint description", fontsize=14, fontweight='bold')
    plt.ylabel(ylabelText, fontsize=14, fontweight='bold')
    # legend_properties = {'weight':'bold'} 
    # plt.legend(prop=legend_properties) 

    # plt.legend()

    plt.xticks(range(1,8), ticks)  # todo : change this part for your figure
    # plt.xticks(range(0, len(ticks) * 2, 2), ticks)
    plt.tick_params(axis='x', labelsize=14)
    plt.xticks(weight = 'bold')
    plt.tick_params(axis='y', labelsize=14)
    plt.yticks(weight = 'bold')
    # plt.xlim(-2, len(ticks)*2)
    # plt.ylim(0, 8)

    plt.legend(bbox_to_anchor=(0,1.02,1,0.2), loc="lower left", prop = {'weight':'bold', 'size':11.5},
                mode="expand", borderaxespad=0, ncol=2)
            
    # plt.legend(prop={'size': 14})
    # legend_properties = {'weight':'bold'} 
    # plt.legend(prop=legend_properties) 

    plt.tight_layout()

    if(not os.path.isdir("../exp-paper3/boxplots_2/")):
        os.mkdir("../exp-paper3/boxplots_2/")
    plt.savefig(f"../exp-paper3/boxplots_2/questionnaire_result.pdf")


#******* notice **********
# run this program in terminal (OF THE SAME VS - not in terminal of the mac)
# running that in vs leads to an error

if __name__ == "__main__":
    drawPlot_questionnaire()

    # drawPlot_paper3_2("test_num", "Number of test cases")
    # drawPlot_paper3_2("time", "Time (ms)")

    # drawPlot_paper3_2("count", "test_num", "Number of test cases", ["A", "A'"])
    # drawPlot_paper3_2("intersection", "test_num", "Number of test cases", ["B", "B'"])
    # drawPlot_paper3_2("count", "time", "Time (ms)", ["A", "A'"])
    # drawPlot_paper3_2("intersection", "time", "Time (ms)", ["B", "B'"])

    # drawPlot_paper3("count", "test_num", "Number of test cases")
    # drawPlot_paper3("intersection", "test_num", "Number of test cases")
    # drawPlot_paper3("count", "time", "Time (ms)")
    # drawPlot_paper3("intersection", "time", "Time (ms)")

    print("Done!")


