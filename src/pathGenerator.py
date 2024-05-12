import sys
from z3 import *
import time
import psutil

from random import randrange
import socket
import datetime

#------------------------------------
# these functions are mostly used when we call this python module from an erlang module

# from erlport.erlterms import Atom                         # it is needed when calling this python module from an erlang module
# from erlport.erlang import set_message_handler, cast      # it is needed when calling this python module from an erlang module
def register_handler(ErlangProcess):
    def handler(message):
        # ToDo [error root]: it is required to get the time-out from erlang code too, and construct the pathGenerator using that parameter
        gridX = message[0]
        gridY = message[1]
        agentsNum = message[2]
        waitSteps = message[3]
        dispSteps = message[4]
        cs = message[5]
        genORFilterFlag = message[6]
        timeout = message[8]  # this parameter should have been the 7th parameter, but since I implemented that at last, I did not change the previous code
        obj = pathGenerator(gridX,gridY,agentsNum,waitSteps,dispSteps,cs, timeout, genORFilterFlag)

        if(asciiListToString(genORFilterFlag)=="Filter"):
            receivedPath =message[7]
            # obj.printTestInputs([receivedPath])
            # print(obj.solver)
            isSatisfied = obj.isValidInput(receivedPath)
            cast(ErlangProcess, isSatisfied)
        elif(asciiListToString(genORFilterFlag)=="Gen"):
            inputNum = message[7]
            inputs = obj.genTestInputs(inputNum)
            paths = [pathsPointsToActions(x) for x in inputs]
            cast (ErlangProcess, paths)

    set_message_handler(handler)

def pathsPointsToActions(paths):
    """
    converting paths from the format of a list of list of points, i.e., (x,y), to the expected format of our SUT
    which is a list of (agentID, startX, startY, [actions]). 
    Each action is an erlang atom from this list {right, left, up, down, nop}
    """

    ans = []
    for agentNum in range(len(paths)):
        actions = []
        for time in range(1, len(paths[agentNum])):
            if( paths[agentNum][time][0]>paths[agentNum][time-1][0]):       #right
                actions.append(Atom(bytes("right", 'utf-8')))
            elif( paths[agentNum][time][0]< paths[agentNum][time-1][0]):    #left
                actions.append( Atom(bytes("left", 'utf-8')) )
            elif( paths[agentNum][time][1]>paths[agentNum][time-1][1]):     #up
                actions.append( Atom(bytes("up", 'utf-8')) )
            elif( paths[agentNum][time][1]< paths[agentNum][time-1][1]):    #left
                actions.append( Atom(bytes("down", 'utf-8')) )
            else:
                actions.append( Atom(bytes("nop", 'utf-8')) )               # waiting (no operation)      
        ans.append( (agentNum+1,paths[agentNum][0][0], paths[agentNum][0][1], actions) ) 

    return ans

def asciiListToString(input):   # converting from a string, in the format of a list of ascii integers, to python string format
    return ''.join(map(chr, input))

def stringToAscii(input):       # converting from python string format to the string format of a list of ascii integers
    return [ord(x) for x in list(input)]
#------------------------------------

class pathGenerator:
    '''
    todo: 
        further improvementing points:
        error handing 
        changing classs modifiers
        setter/getter fo solver
        reset function for the solver
    '''

    # New consructor: if fixing the seed of z3 for random results is also needed
    # def __init__(self, gridX, gridY, agentsNum, waitSteps, dispSteps, constraints, z3RandomSeed,timeoutSeconds = 60, genORFilterFlag = stringToAscii("Gen")):
    # if we do not set the z3 random seed, each time that we try to solve a set of constraints, we will have the same solution
    # Note that in each execution, we may try to solve a set of contraints a couple of times. Even if we do not change the seed each time
    # we will see a different solutions for the sequeence of calls to solve a question. However, if we do not control the seed, we will have
    # the same 'sequence' of solutions each time that we execute the program.

    def __init__(self, gridX, gridY, agentsNum, waitSteps, dispSteps, constraints,timeoutSeconds = 60, genORFilterFlag = stringToAscii("Gen")):
        self.gridX = gridX
        self.gridY = gridY
        self.agentsNum  = agentsNum
        self.waitSteps  = waitSteps
        self.dispSteps  = dispSteps
        self.constraits = constraints

        self.totalSteps = self.waitSteps + self.dispSteps
        self.solver = Solver()
        # self.solver.set('timeout', timeoutSeconds*1000)
        # set_option('timeout', timeoutSeconds*1000)      # I think both of the z3 time-out setting do not work well
        
        # set_option('smt.random_seed', 1)   # setting the random seed of z3
        # varSeed =  get_param('smt.random_seed')       # getting the random seed of z3
        # print("smt.random_seed is: %s" %(varSeed))

        self.X = None # self.X[i][t] is the x position of agent i in time t, # 0<= i < agentsNum, 0<= t <= totalSteps
        self.Y = None # self.X[i][t] is the y position of agent i in time t ,# 0<= i < agentsNum, 0<= t <= totalSteps
        # removing unnecessary waiting constraints (when waitSteps=0) to improve z3 performance
        # self.W = None # self.W[i][t] is the remaining waiting steps of agent i in time t, # 0<= i < agentsNum, 0<= t <= totalSteps        
        self.D = None # self.D[i][t] is the remaining displacement steps of agent i in time t, # 0<= i < agentsNum, 0<= t <= totalSteps

        self.csNameIndex = 0    # this is used for naming different variables and having distinct ones

        self.X = [ [ Int("x_%s_%s" % (i+1, t)) for t in range(self.totalSteps+1) ] for i in range(self.agentsNum) ] 
        self.Y = [ [ Int("y_%s_%s" % (i+1, t)) for t in range(self.totalSteps+1) ] for i in range(self.agentsNum) ] 

        # self.W = [ [ Int("w_%s_%s" % (i+1, t)) for t in range(self.totalSteps+1) ] for i in range(self.agentsNum) ] 
        self.D = [ [ Int("d_%s_%s" % (i+1, t)) for t in range(self.totalSteps+1) ] for i in range(self.agentsNum) ] 

        self.solver.add(self.findCS(self.constraits))
        if asciiListToString(genORFilterFlag) == "Gen":  # there is no need to have Path generation constraints for filtering paths - just skip them
            self.__initPathsConstraints()
        
    def __initPathsConstraints(self):
        # CSs for having distinct initial positions (time t=0) for the agents
        distinctStartingPosition_CSs = []
        for i in range(self.agentsNum):
            for j in range(i+1,self.agentsNum):
                distinctStartingPosition_CSs.append( Or( self.X[i][0] != self.X[j][0], self.Y[i][0] != self.Y[j][0]))

        # the remaining waiting and dsplacement steps are zero at the beginning and fullfiled at the end of the path
        start_end_WD_CSs = [ And (
                                # removing waiting constraints to improve z3 performance
                                # self.W[i][0]==0, self.W[i][self.totalSteps]==self.waitSteps,
                                self.D[i][0]==0, self.D[i][self.totalSteps]==self.dispSteps
                            ) 
                            for i in range(self.agentsNum) ]

        boundaries_CSs = [  And(
                                [And(
                                    self.X[i][t]>=1, self.X[i][t]<=self.gridX,
                                    self.Y[i][t]>=1, self.Y[i][t]<=self.gridY,
                                    self.D[i][t]>=0, self.D[i][t]<=self.dispSteps

                                    # self.X[i][t]>=0, self.X[i][t]<=self.gridX,
                                    # self.Y[i][t]>=0, self.Y[i][t]<=self.gridY,
                                    # self.D[i][t]>=0, self.D[i][t]<=self.dispSteps

                                    # removing waiting constraints to improve z3 performance
                                    # self.W[i][t]>=0, self.W[i][t]<=self.waitSteps,
                                )
                                for t in range(self.totalSteps+1)]) 
                            for i in range(self.agentsNum)]

        stepping_WD_CS1 = [ And(
                                # removing waiting constraints to improve z3 performance
                                # [Xor(
                                #     And(self.D[i][t+1]==self.D[i][t]+1 , self.W[i][t+1]==self.W[i][t]  ), # displacement move
                                #     And(self.W[i][t+1]==self.W[i][t]+1 , self.D[i][t+1]==self.D[i][t]  )  # waiting move
                                # )
                                [self.D[i][t+1]==self.D[i][t]+1 for t in range(self.totalSteps)])
                            for i in range(self.agentsNum)]

        stepping_XY_CS2 =   [And( 
                                [Xor( 
                                        And( self.X[i][t+1] == self.X[i][t], Xor(self.Y[i][t+1]==self.Y[i][t]+1, self.Y[i][t+1]==self.Y[i][t]-1) ), # up/down move
                                        And( self.Y[i][t+1] == self.Y[i][t], Xor(self.X[i][t+1]==self.X[i][t]+1, self.X[i][t+1]==self.X[i][t]-1) )  # left/right move
                                    )
                                # removing waiting constraints to improve z3 performance
                                # If(  self.D[i][t+1]==self.D[i][t]+1,
                                #     # displacement move
                                #     Xor( 
                                #         And( self.X[i][t+1] == self.X[i][t], Xor(self.Y[i][t+1]==self.Y[i][t]+1, self.Y[i][t+1]==self.Y[i][t]-1) ), # up/down move
                                #         And( self.Y[i][t+1] == self.Y[i][t], Xor(self.X[i][t+1]==self.X[i][t]+1, self.X[i][t+1]==self.X[i][t]-1) )  # left/right move
                                #     ),
                                    
                                #     # waiting move
                                #     And(self.X[i][t+1] == self.X[i][t], self.Y[i][t+1] == self.Y[i][t])
                                # )
                                for t in range(self.totalSteps)])
                            for i in range(self.agentsNum)]
        
        minX =  1 
        maxX = self.gridX
        minY =  1 
        maxY = self.gridY

        # extended DSL experiment - the code is modified to run the experiment
        # it will assign a random value to the starting point of the first agent in the faulty square (the constraint defined by new DSL)
        # ----the added code---
        new_approach = False    #  set it to True for experimenting with new DSL, False otherwise (old DSL)
        if new_approach:
            minX = (self.gridX)//2 
            minY = (self.gridY)//2
        # -------------------------

        randomness_boost = [self.X[0][0]==randrange(minX,maxX+1), self.Y[0][0]==randrange(minY,maxY+1),]
        # since we do not have distinct starting point constraint in QC erlang version, we do not add it to the constraints
        # self.solver.add(distinctStartingPosition_CSs + start_end_WD_CSs + boundaries_CSs + stepping_WD_CS1 + stepping_XY_CS2)
        self.solver.add( start_end_WD_CSs + boundaries_CSs + stepping_WD_CS1 + stepping_XY_CS2+ randomness_boost)
    
    def isValidInputs(self, testInputs):
        """
        Checks if the test inputs are satisfied by the class constraints (for filtering issues)
        Returns: a list of boolean values, each corresponding to the given test input        
        """
        return [self.isValidInput(testInput) for testInput in testInputs]

    def isValidInput(self, testInput):
        """
        returns True if the given input is satisfied by the class constraints (for filtering issues)
        """

        self.solver.push()
        for i in range(len(testInput)):
            for t in range (len(testInput[i])):
                cs = And(self.X[i][t]==testInput[i][t][0],self.Y[i][t]==testInput[i][t][1])
                self.solver.add(cs)
        # print("Solver:")
        # print(self.solver)

        if self.solver.check()==sat:
            # print( self.solver.model())
            self.solver.pop()
            return True
        
        self.solver.pop()
        return False

    def genTestInputs(self, n):
        """
        This function returns 'n' test inputs by solving the z3 Solver field of this object (if the constraints are consistent).
        If the constraints are not consistent or 'n' solutions does not exit, it will just exit the program.
        """
        
        print("Generating %d test inputs.." %n)
        test_inputs = []
        for i in range(n):
            t1 = time.time()
            paths = self.__genOneTestInput()
            t2 = time.time()
            generation_time = round((t2-t1)*1000)
            print("Test input %d is generated in %d ms." %(i+1, round((t2-t1)*1000)))
            test_inputs.append(paths)
        print("Test generation is finished.\n")

        return test_inputs

    def __genOneTestInput(self):
        """
        This function returns one test input by solving the z3 Solver field of this object (if the constraints are consistent).
        If the constraints are not consistent it will exit the program.
        """

        # self.solver.push()

        # startX =7#self.gridX // 2 # random.randrange(0, self.gridX+1)
        # startY =7#self.gridY // 2 # random.randrange(0, self.gridY+1)
        # print ("Random starting point of first path: (%d,%d)" %(startX, startY))
        # self.solver.add( And(self.X[0][0] == startX, self.Y[0][0]==startY))

        # randomAgent =0 # random.randrange(0,self.agentsNum)
        # ramdomTime  =1 # random.randrange(0,self.totalSteps+1)
        # positionX = 1
        # positionY = 1
        # print ("Random constraint: Agent:%d at time:%d is at position (%d,%d)." %(randomAgent, ramdomTime,positionX,positionY))
        # self.solver.add( And(self.X[randomAgent][ramdomTime]== positionX, self.Y[randomAgent][ramdomTime]==positionY))
        # print(self.solver)
        if (self.solver.check() == sat):
            ans = []
            model = self.solver.model()
            for i in range(self.agentsNum):
                agentAns = []
                for t in range (self.totalSteps+1):
                    pointX = int(str( model[self.X[i][t]] ))
                    pointY = int(str( model[self.Y[i][t]] ))  # pointY = int(str( model.evaluate(self.Y[i][t]) ))
                    agentAns.append((pointX,pointY))
                ans.append(agentAns)
            # self.solver.pop()
            return ans
        
        elif (self.solver.check() == unsat):
            print("Constraints are inconsistent.")
            print(self.solver)
            sys.exit()
        else:
            randSeedZ3 =  get_param('smt.random_seed')
            print("Consistency of constraints is unknown for z3 random seed %s." %(randSeedZ3))
            # if int(randSeedZ3)<100 :  # changing the z3 seed of randomness
            #     set_option('smt.random_seed', int(randSeedZ3)+1)
            #     self.__genOneTestInput()
            sys.exit()
    
    def printTestInputs(self, testInputs):
        """
        This is a pretty printer for the list of test inputs.
        Parameter:
        -----------
        testInputs: Each test input is a set of paths (for different agents).
                    Each path is a list of tuples, where each tuple has two members (X,Y).
        """

        for i in range(len(testInputs)):
            print ("Test Input " + str(i+1) + ":")
            for j in range(len(testInputs[i])):
                print ("Path "+str(j+1)+ " :", end='')
                for t in range (len(testInputs[i][j])):
                    print ("("+str(testInputs[i][j][t][0])+","+str(testInputs[i][j][t][1])+"),", end='')
                print ("")
            print ("")
        print ("")

    ##*****************************************************************************************************************************
    ##******************************************************* CONSTRAINTS *********************************************************
    ##*****************************************************************************************************************************

    def findCS(self, cs):
        """
        cs: The constraint that follow our DSL semantics in the syntax of a tuple.
            This parameter is (designed to be) received from an Erlang code, and as a result, Strings are in the form of a list of ascii integers.
        --------
        Returns: Converted form of the given constraint, in z3 constraints format
        """

        if (len(cs)==2):    # ToDo: changing and using switch/case (of python 3)
            keyword = asciiListToString(cs[0])
            L1 = cs[1]
            if (keyword=="NOT"):
                return Not(self.findCS(L1))
            else:
                print ("Parsing Error - (%s,-) is invalid for Constraint type.\n" %(keyword,) )
                sys.exit()
        
        elif (len(cs)==3):
            keyword = asciiListToString(cs[0] )
            L1 = cs[1]
            L2 = cs[2]
            if(keyword=="OR"):
                return Or (self.findCS(L1), self.findCS(L2))
            elif(keyword=="AND"):
                return And (self.findCS(L1), self.findCS(L2))
            elif(keyword=="IN"):
                return self.findCSCondition(L1, L2)
            else:
                print ("Parsing Error - (%s,_,_) is invalid for Constraint type.\n" %(keyword,))
                sys.exit()
        else:
            print ("Parsing Error - %s is an invalid Constraint.\n" %(cs,))
            sys.exit()

    def findCSCondition(self, area, condition):
        """
        Finds the z3 equivalent constraint of the constraint "IN area condition"

        Parameters:
        -----------
        area:       The area that the condition is supposed to be satisfied on that
        condition:  The expected condition for the paths that is supposed to be satisfied.

        Returns:
        --------
        Converted form of the given constraint, in z3 constraints format
        """

        if (len(area)==2):
            areaType = asciiListToString(area[0])
            areaSize = area[1]

            self.csNameIndex +=1
            centerX = Int ("Cx_%s" %(self.csNameIndex))    # 'Int' does not support squares with 'odd' side length
            centerY = Int ("Cy_%s" %(self.csNameIndex))
            # centerX = Real ("Cx_%s" %(self.csNameIndex))
            # centerY = Real ("Cy_%s" %(self.csNameIndex))

            inGridCenter = And (centerX <= self.gridX, centerX>=0, centerY>=0, centerY<=self.gridY)

            if (areaType == "Circle"):
                return Exists([ centerX, centerY], 
                                And([centerX>=1, centerX<=self.gridX,  # exp3 - change the min value of varibales in grid from 0 to 1
                                     centerY>=1, centerY<=self.gridY,  # exp3 - change the min value of varibales in grid from 0 to 1
                                     self.findCSsCircle(centerX, centerY, areaSize, condition)]
                                )
                        )
                # return And( inGridCenter, self.findCSsCircle(centerX, centerY, areaSize, condition) )
            elif (areaType == "Square"):
                return Exists([ centerX, centerY], 
                                And([centerX>=1, centerX<=self.gridX,   # exp3 - change the min value of varibales in grid from 0 to 1
                                     centerY>=1, centerY<=self.gridY,   # exp3 - change the min value of varibales in grid from 0 to 1
                                     self.findCSsSqaure(centerX, centerY, areaSize, condition)]
                                )
                        )                
                # return And (inGridCenter, self.findCSsSqaure(centerX, centerY, areaSize, condition) )
            else:
                print ("Parsing Error - %s is an invalid area type.\n" %(areaType,))

        else:
            print ("Parsing Error - %s is an invalid condition area.\n" %(area,))
            sys.exit()
    ##*****************************************************************************************************************************

    ##*****************************************************************************************************************************
    ##******************************************************* CIRCLE AREA *********************************************************
    ##*****************************************************************************************************************************

    def findCSsCircle(self, centerX, centerY, radius, condition):
        """
        Finds the z3 equivalent constraint of the constraint "IN (AreaInstance (Circle radius) (centerX centerY)) condition"

        Parameters:
        -----------
        centerX:    The x coordinate of the area center 
        centerY:    The y coordinate of the area center
        radius:     The circle radius
        condition:  The expected condition for the paths that is supposed to be satisfied.

        Returns:
        --------
        Converted form of the given constraint, in z3 constraints format
        """

        if (len(condition)==2):
            keyword = asciiListToString( condition[0] )
            L1 = condition[1]
            if (keyword == "Not"):
                return Not (self.findCSsCircle(centerX, centerY, radius, L1))
            elif (keyword == "Count"):
                return self.findCSsCircleCount(centerX, centerY, radius, L1)                       
            else:
                print ("Parsing Error - (%s,_) is invalid for condition type.\n" %(keyword,))
                sys.exit()
        
        elif(len(condition)==3):
            keyword = asciiListToString (condition[0])
            L1 = condition[1]
            L2 = condition[2]
            if(keyword=="Intersection"):
                return self.findCSsCircleIntersection(centerX, centerY, radius, L1, L2)
            elif( keyword == "And"):
                return And (self.findCSsCircle(centerX, centerY, radius, L1), self.findCSsCircle(centerX, centerY, radius, L2))
            elif( keyword == "Or"):
                return Or  (self.findCSsCircle(centerX, centerY, radius, L1), self.findCSsCircle(centerX, centerY, radius, L2))
            else:
                print ("Parsing Error - (%s,_,_) is invalid for condition type.\n" %(keyword,))
                sys.exit()
        else:
            print ("Parsing Error - %s is an invalid condition.\n" %(condition,))
            sys.exit()

    def findCSsCircleCount(self, centerX, centerY, radius, count):
        """
        Finds the z3 equivalent constraint of the constraint "IN (AreaInstance (Circle radius) (centerX centerY)) (Count count)"
        
        Parameters:
        -----------
        centerX:    The x coordinate of the circle center 
        centerY:    The y coordinate of the circle center
        radius:     The radius of the circle
        count:      The least number of agents that are supposed to be within the circle

        Returns:
        --------
        Converted form of the given constraint, in z3 constraints format
        """



        # extended DSL experiment - the code is modified to run the experiment
        #
        # ----the modified code---
        # optimized when there are two agents, we want both to be in the area, and this is the only constraint to solve (no combination with and/or/not)
        new_approach = False    #  set it to True for experimenting with new DSL, False otherwise (old DSL)

        time  = randrange(self.totalSteps+1)
        cs_1 = ( (self.X[0][time]-centerX)**2  + (self.Y[0][time]-centerY)**2 <= radius**2)
        cs_2 = ( (self.X[1][time]-centerX)**2  + (self.Y[1][time]-centerY)**2 <= radius**2)
        if new_approach:
            center_in_faulty_area = And(centerX>=self.gridX//2, centerY>=self.gridY//2) # for the new DSL - define the center in the faulty area
            return And(cs_1,cs_2,center_in_faulty_area)  # new DSL exp - fast approach
        else:
            return And(cs_1, cs_2) # old DSL exp - fast approach
        # ---------------------------

        # ---- the original code ----
        # return Or(  # converting list L to a sequence of function arguments by: '*L'
        #             [ AtLeast( *[( (self.X[i][t]-centerX)**2 + (self.Y[i][t]-centerY)**2 <= radius**2) for i in range(self.agentsNum)], count) 
        #             for t in range(self.totalSteps+1)]
        #         )
        # ---------------------------

    def findCSsCircleIntersection(self, centerX, centerY, radius, intersectionsNum, degree):
        """
        Finds the z3 equivalent constraint of the constraint "IN (AreaInstance (Circle radius) (centerX centerY)) (Intersection intersectionNum degree) "
        
        Parameters:
        -----------
        centerX:            The x coordinate of the circle center 
        centerY:            The y coordinate of the circle center
        radius:             The radius of the circle
        intersectionNum:    The least number of intersection points
        degree:             The least degree of the intersection points

        Returns:
        --------
        Converted form of the given constraint, in z3 constraints format
        """
        # todo: Think: can I define a map here instead of z3 integer vairbales?

        #Intersection Points
        self.csNameIndex +=1
        IPXs = [Int("Ix_%s_%s" %(i, self.csNameIndex) ) for i in range(intersectionsNum)]   
        IPYs = [Int("Iy_%s_%s" %(i, self.csNameIndex) ) for i in range(intersectionsNum)]   

        
        ip_cs = Exists( [*IPXs,*IPYs],
                        And( 
                            And([( ( (IPXs[i]-centerX)**2 + (IPYs[i]-centerY)**2 ) <= radius**2 ) for i in range(intersectionsNum)]),
                            And(
                                [AtLeast(
                                    *[Or ([And(self.X[i][t]==IPXs[j], self.Y[i][t]==IPYs[j]) for t in range(self.totalSteps+1)]) 
                                    for i in range(self.agentsNum)],
                                    degree
                                ) for j in range(intersectionsNum)]
                            )
                        )
                )
        
        # extended DSL experiment - the code is modified to run the experiment
        #
        # ----the modified code---
        new_approach = True    #  set it to True for experimenting with new DSL, False otherwise (old DSL)

        if new_approach:
            center_in_faulty_area = And(centerX>self.gridX/2, centerY>self.gridY/2)
            return And(ip_cs, center_in_faulty_area)
        else:
            return ip_cs
        # ---------------------------

        # ---- the original code ----
        # return ip_cs
        # ---------------------------
    
        # no need to define new constraint for checking that intersection points are inside the grid
        # this constraint is already implied by the following constraint [all (X,Y)s are inside the grid]
        
        # intersection Points in area
        # IPsInArea = And ([( ( (IPXs[i]-centerX)**2 + (IPYs[i]-centerY)**2 ) <= radius**2 ) for i in range(intersectionsNum)])
        
        # Question: is this code a correct way of implementing this functionality?
        # the paths that cross the intersection points with the given degree
        # IP_CSs= And(
        #             [AtLeast(
        #                 *[Or ([And(self.X[i][t]==IPXs[j], self.Y[i][t]==IPYs[j]) for t in range(self.totalSteps+1)]) 
        #                  for i in range(self.agentsNum)],
        #                 degree
        #             ) for j in range(intersectionsNum)]
        #         )
        # return And(IPsInArea,IP_CSs)
             
    
    ##*****************************************************************************************************************************

    ##*****************************************************************************************************************************
    ##******************************************************* SQUARE AREA *********************************************************
    ##*****************************************************************************************************************************

    def findCSsSqaure(self, centerX, centerY, sideLength, condition):
        """
        Finds the z3 equivalent constraint of the constraint "IN (AreaInstance (Square sideLength) (centerX centerY)) condition"

        Parameters:
        -----------
        centerX:    The x coordinate of the square center 
        centerY:    The y coordinate of the square center
        sideLength: The square sideLength
        condition:  The expected condition for the paths that is supposed to be satisfied.

        Returns:
        --------
        Converted form of the given constraint, in z3 constraints format
        """

        if (len(condition)==2):
            keyword = asciiListToString(condition[0])
            L1 = condition[1]
            if (keyword == "Not"):
                return Not (self.findCSsSqaure(centerX, centerY, sideLength, L1))
            elif (keyword == "Count"):
                return self.findCSsSquareCount(centerX, centerY, sideLength, L1)
            else:
                print ("Parsing Error - (%s,_) is invalid for condition type.\n" %(keyword,))
                sys.exit()
        
        elif(len(condition)==3):
            keyword = asciiListToString(condition[0])
            L1 = condition[1]
            L2 = condition[2]
            if(keyword=="Intersection"):
                return self.findCSsSquareIntersection(centerX, centerY, sideLength, L1, L2)
            elif( keyword == "And"):
                return And (self.findCSsSqaure(centerX, centerY, sideLength, L1), self.findCSsSqaure(centerX, centerY, sideLength, L2))
            elif( keyword == "Or"):
                return Or  (self.findCSsSqaure(centerX, centerY, sideLength, L1), self.findCSsSqaure(centerX, centerY, sideLength, L2))
            else:
                print ("Parsing Error - (%s,_,_) is invalid for condition type.\n" %(keyword,))
                sys.exit()
        else:
            print ("Parsing Error - %s is an invalid condition.\n" %(condition,))
            sys.exit()

    def findCSsSquareCount(self, centerX, centerY, sideLength, count):
        """
        Finds the z3 equivalent constraint of the constraint "IN (AreaInstance (Square sideLength) (centerX centerY)) (Count count)"
        
        Parameters:
        -----------
        centerX:    The x coordinate of the square center 
        centerY:    The y coordinate of the square center
        sideLength: The sideLength of the square
        count:      The least number of agents that are supposed to be within the square

        Returns:
        --------
        Converted form of the given constraint, in z3 constraints format
        """

        return Or(  # converting list L to a sequence of function arguments by: '*L'
                    # since I could not use abs() function, I squared the numbers before comparing them
                    [ AtLeast( *[And( (self.X[i][t]-centerX)**2<=(sideLength/2)**2, (self.Y[i][t]-centerY)**2<=(sideLength/2)**2) for i in range(self.agentsNum)], count) 
                    for t in range(self.totalSteps+1)])

    def findCSsSquareIntersection(self, centerX, centerY, sideLength, intersectionsNum, degree):
        """
        Finds the z3 equivalent constraint of the constraint "IN (AreaInstance (Square sideLength) (centerX centerY)) (Intersection intersectionNum degree) "
        
        Parameters:
        -----------
        centerX:            The x coordinate of the square center 
        centerY:            The y coordinate of the square center
        sideLength:         The sideLength of the square
        intersectionNum:    The least number of intersection points
        degree:             The least degree of the intersection points

        Returns:
        --------
        Converted form of the given constraint, in z3 constraints format
        """
        #Intersection Points
        self.csNameIndex +=1
        IPXs = [Int("Ix_%s_%s" %(i, self.csNameIndex) ) for i in range(intersectionsNum)]  
        IPYs = [Int("Iy_%s_%s" %(i, self.csNameIndex) ) for i in range(intersectionsNum)]   
        
        # since I could not use abs() function, I squared the numbers before comparing them
        IPsInArea = And( [And((IPXs[i]-centerX)**2<= (sideLength/2)**2, (IPYs[i]-centerY)**2 <= (sideLength/2)**2 ) for i in range(intersectionsNum)])
        
        # the following constraint also implied that the inresection points are inside the grid [already defined that (X,Y)s are inside the grid]
        
        # the paths that cross the intersection points with the given degree
        IP_CSs= And(
                    [AtLeast(
                        *[Or ([And(self.X[i][t]==IPXs[j], self.Y[i][t]==IPYs[j]) for t in range(self.totalSteps+1)]) 
                         for i in range(self.agentsNum)],
                        degree
                    ) for j in range(intersectionsNum)]
                )
        # return And(IPsInArea,IP_CSs)
        return Exists(  [*IPXs,*IPYs],
                        And( 
                            And ([And((IPXs[i]-centerX)**2<= (sideLength/2)**2, (IPYs[i]-centerY)**2 <= (sideLength/2)**2 ) for i in range(intersectionsNum)]),
                            And(
                                [AtLeast(
                                    *[Or ([And(self.X[i][t]==IPXs[j], self.Y[i][t]==IPYs[j]) for t in range(self.totalSteps+1)]) 
                                    for i in range(self.agentsNum)],
                                    degree
                                ) for j in range(intersectionsNum)]
                            )
                        )
        )        

    ##*****************************************************************************************************************************

#------------------------------------
# monitoring the python processes
# I made this function since I realized when I expect some python processes to be killed, they are still alive
def procStatus(message):
    print("proc status:" + message)
    allProc = [x.name() for x in psutil.process_iter()]
    procCount = len(allProc)
    print("proc count: %d" %(procCount))
    pyProc = [pyProc for pyProc in allProc if "python" in pyProc]
    pyProcCount = len(pyProc)
    print("python proc count: %d" %(pyProcCount))
    time.sleep(3)
    return [procCount,pyProcCount]
#------------------------------------

##------------------------------------practicing and mini test code---------------------------------    
def test_num1():

    # cs = (stringToAscii("NOT"),(stringToAscii("IN"), (stringToAscii("Circle"), 1), (stringToAscii("Intersection"),1,3) ))
    cs = (stringToAscii("IN"), (stringToAscii("Circle"), 1), (stringToAscii("Intersection"),1,3) )
    gridX = 5
    gridY = 5
    agentsNum = 3
    waitSteps = 0
    dispSteps = 2
    inputNum = 1
    obj = pathGenerator(gridX,gridY,agentsNum,waitSteps,dispSteps,cs)
    print("generating..")
    t1= time.time()
    inputs = obj.genTestInputs(inputNum)
    t2 = time.time()
    # ans = obj.isValidInputs(inputs)
    # t3 = time.time()
    print ("Data generation time: %d" %(int(t2-t1)))
    # print ("Data filtering time : %d" %(int(t3-t2)))

    obj.printTestInputs(inputs)
    # mmm =  get_param('smt.random_seed')
    # print("smt.random_seed is: %s" %(mmm))
    # print(ans)
   
    # print("constraints:\n")
    # for s in obj.solver.assertions():
    #     print (s)

def generate_and_save_test_input(file_name_to_save_results):
    areaType =  "Circle" #  "Square" #
    areaSize = 1
    conditionType = "Count" # "Intersection" #
    # numIntersection = 1
    densityDegree = 2

    cs = (stringToAscii("IN"), (stringToAscii(areaType), areaSize), (stringToAscii(conditionType), densityDegree)) # count condition
    # cs = (stringToAscii("IN"), (stringToAscii(areaType), areaSize), (stringToAscii(conditionType), numIntersection, densityDegree)) # intersection condition

    gridSize  = 5
    agentsNum = 2
    dispSteps = 10
    waitSteps = 0
    inputsNum = 1
    obj = pathGenerator(gridSize,gridSize,agentsNum,waitSteps,dispSteps,cs)

    testInputs = obj.genTestInputs(inputsNum)
    faults = []
    for turtle in range(0,agentsNum):
        one_turtle_fault = []
        for step in range(0,waitSteps+dispSteps):
            one_turtle_fault.append(randrange(4))
        faults.append(one_turtle_fault)

    # obj.printTestInputs(testInputs)
    save_inputs(testInputs, faults, gridSize, file_name_to_save_results)

def save_inputs(testInputs, faults, gridSize, file_name_to_save_results):
    # for item in testInputs[0]:
    #     print(item)

    file_name_to_save_results = "inputs_z3.txt"
    inputs_file = open(file_name_to_save_results, 'w')

    for i in range(len(testInputs)):
        str = f"Grid Size: ({gridSize},{gridSize})"
        inputs_file.write(str+"\n")
        print (str)

        for j in range(len(testInputs[i])):
            
            str = f"Turtle ID:{j+1} - Start: ({testInputs[i][j][0][0]},{testInputs[i][j][0][1]}) - Moves:"
            inputs_file.write(str)
            print (str, end='')

            for t in range (1, len(testInputs[i][j])):
                if testInputs[i][j][t][0]>testInputs[i][j][t-1][0]:
                    move="right"
                elif testInputs[i][j][t][0]<testInputs[i][j][t-1][0]:
                    move="left"
                elif testInputs[i][j][t][1]>testInputs[i][j][t-1][1]:
                    move="up"
                elif testInputs[i][j][t][1]<testInputs[i][j][t-1][1]:
                    move="down"
                else:
                    move="nop"

                str = f"{move},"
                print (str, end='')
                inputs_file.write(str)
            
            str = f" - Faults:"
            print (str, end='')
            inputs_file.write(str)

            for t in range (len(testInputs[i][j])-1):
                str = f"{faults[j][t]},"
                print (str, end='')
                inputs_file.write(str)

            print ("")
            inputs_file.write("\n")
        print ("")
        inputs_file.write("\n")
        print("done!")
        inputs_file.close()


if __name__ == "__main__":

    '''
        todo: while running different experiment:
        stop/start the SUT
        set the grid size 
        set the cs (count/intersection - with its parameters)
        set the constraint of the starting point of the first turtle, to one quarter of the grid
        set the constraint of the center of the area, to one quarter of the grid
        run this file - wait till the execution is finished.

        after execution:
        put the SUT files to the folder of paper 3 exp
        put the fault detection results to the folder of paper 3 exp
    '''

    fault_detection_num =1
    file_name_inputs = "inputs_z3.txt"

    my_local_host=socket.gethostbyname(socket.gethostname())
    portSend =1234
    sendServerAddressPort = (my_local_host, portSend)
    sendUDPClientSocket = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)

    portReceive = 6789
    receiveUDPClientSocket = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM)
    bufferSize = 1024
    receiveServerAddressPort = (my_local_host, portReceive)
    receiveUDPClientSocket.bind(receiveServerAddressPort)

    fault_scenarios_num = []
    generation_times = []
    total_scenarios = 0
    num_of_run = 10
    for repeat in range(1,num_of_run+1): 
        one_fault_scenarios =0
        while True:
            total_scenarios +=1
            one_fault_scenarios+=1
            print(f"Trying to find the fault with index: {repeat}")
            print(f"Total Scenarios: {total_scenarios}")

            t1 = datetime.datetime.now()
            generate_and_save_test_input(file_name_inputs)
            t2 = datetime.datetime.now()
            generation_times.append( (t2-t1).microseconds//1000 ) # saving the milliseconds

            bytesToSend = str.encode(file_name_inputs) # sending the file name to the socket
            sendUDPClientSocket.sendto(bytesToSend, sendServerAddressPort)
            msgFromServer_binary = receiveUDPClientSocket.recvfrom(bufferSize)
            msgFromServer_string = msgFromServer_binary[0].decode('ascii')

            if msgFromServer_string=="Fail":    #Pass
                break 
            break
        
        fault_scenarios_num.append(one_fault_scenarios)

    print("Experiment is done.")

    faults_file_name = "fault_detection_test_nums.txt"
    f = open(faults_file_name,'w')
    for item in fault_scenarios_num:
        f.write(f"{item}\n")
    
    gen_times_file_name = "gen_input_times.txt"
    f = open(gen_times_file_name, 'w')
    for item in generation_times:
        f.write(f"{item}\n")
    
    print(f"Average: {sum(fault_scenarios_num)/num_of_run}")
    receiveUDPClientSocket.close()
    sendUDPClientSocket.close()

    # set_option('smt.random_seed', randrange(1000))   # setting the random seed of z3
    # # varSeed =  get_param('smt.random_seed')        # getting the random seed of z3
    # # print("smt.random_seed is: %s" %(varSeed))

    # for i in range(1, 10):
    #     print(f"try:{i}")
    #     rand = randrange(1000)
    #     print(f"random:{rand}")
    #     set_option('smt.random_seed', rand)   # setting the random seed of z3
    #     generate_and_save_test_input("x.txt")
