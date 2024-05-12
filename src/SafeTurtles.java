import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.io.File;  // Import the File class
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;

public class SafeTurtles {
	
	public ArrayList<Turtle> turtles ;
	public Position gridSize;  // indexes of grid: 0,1,2,...,(gridSize-1)
	
	enum movementAction {up, down, right, left, nop}
	
	// constructor to create SafeTurtles 
	public SafeTurtles(ArrayList<Turtle>  turtles, Position gridSize) {
		this.turtles = turtles;
		this.gridSize = gridSize;
	}
	
	// Constructor to read the SUT test inputs from file
	public SafeTurtles(String fileName) {
		SafeTurtles st = readSUTInput(fileName);
		this.turtles = st.turtles;
		this.gridSize = st.gridSize;
	}
	
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof SafeTurtles) {
			SafeTurtles safeTurtles = (SafeTurtles) obj;
			if(!this.gridSize.equals(safeTurtles.gridSize))
				return false;
			if(this.turtles.size()!=safeTurtles.turtles.size())
				return false;
			for(int tIndex=0;tIndex<this.turtles.size();tIndex++) {
				if(!this.turtles.get(tIndex).equals(safeTurtles.turtles.get(tIndex)))
					return false;
			}
			return true;
		}
		return false;
	}
	
	// get the action direction, which is among {U,D,L,R}, by moving from the current position to the next position
	public static movementAction getNextMove(Position current, Position next) {
		if(next.x == current.x && next.y ==current.y)
			return movementAction.nop;
		if(next.x == current.x && next.y ==current.y +1)
			return movementAction.up;
		if(next.x == current.x && next.y ==current.y -1)
			return movementAction.down;
		if(next.x == current.x-1 && next.y ==current.y)
			return movementAction.left;
		if(next.x == current.x+1 && next.y ==current.y)
			return movementAction.right;
		System.out.println("Error: impossible turtle move!");
		return null;
	}
	
	// get the position in the grid by moving from a point to a direction
	public static Position getNextPosition(Position current, movementAction move ) {
		Position nextPosition = switch (move) {
			case nop: 	yield new Position(current.x, current.y);
			case up: 	yield new Position(current.x, current.y+1);
			case down: 	yield new Position(current.x, current.y-1);
			case right: yield new Position(current.x+1, current.y);
			case left: 	yield new Position(current.x-1, current.y);	
			default:	throw new IllegalArgumentException("Unexpected value: " + move);
		};
		return nextPosition;
	}
	
	// it will execute the SUT with the given SUT at the time of constructing the object
	// it will inject the fault based on the stepFault of each turtle. 
	// If there are not enough number of defined faults and the turtle has not reached its goal, it will add random faults to the turtles.
	// it will return all executed moves of all turtles at the end
	public ArrayList<ArrayList<Position>> runTurtles(){

		while( !completeMission()) { // check there is still any turtle not finished its journey
			// Getting the current positions
			ArrayList<Position> currentPositions = new ArrayList<Position>(
					turtles.stream()
	 					.map(turtle-> turtle.currentPosition)
	 					.toList()
	 				);
			
			// There are turtles with different priorities. The one with smaller ID(number) has higher priority.
			// A turtle with a higher priority has priority to (plan and) move to a free position
			ArrayList<Position> higherPriorityNextPlans = new ArrayList<Position>();
			
			for(int i=0;i<turtles.size();i++) {
				
				if(turtles.get(i).completedMission())
					continue;
				
				boolean injectedFault1 = false; // self-localization fault
				boolean injectedFault2 = false; // deviation to unintended position or unable to brake on time
				boolean injectedFault3 = false; // communication problem, do not see the agents nearby
				int faultNum ; //new Random().nextInt(4);	
				
				if(turtles.get(i).stepsFault.size()!=0) {
					int nextFault = turtles.get(i).stepsFault.remove(0);
					faultNum = nextFault;
					turtles.get(i).stepsFault.add(faultNum);			
				}				
				else
					// if all of the faults in SUT test inputs are used and there is no random fault anymore in stepsFault
					// we generate the faults with the same possibility of 25% for each fault and correct action
					faultNum = new Random().nextInt(4);	
				
				// Injecting the faults to the top-right quarter of the grid
				if((turtles.get(i).currentPosition.x > turtles.get(i).gridSize.x/2) && 
				   (turtles.get(i).currentPosition.y > turtles.get(i).gridSize.y/2)) {
					switch(faultNum) {
					case 0: break; // no fault in this case
					case 1: injectedFault1=true;break;
					case 2: injectedFault2=true;break;
					case 3: injectedFault3=true;break;
					default: System.out.println("Wrong fault number!");	
					}
				}

				ArrayList<Position> otherCurrentPositions = new ArrayList<Position>(currentPositions);
				otherCurrentPositions.remove(i);
				
				Position currentPos = turtles.get(i).currentPosition;
				Position nextPos = turtles.get(i).getNext(); 
				
				// the turtle believes it is in a position nearby due to self-localization problem
				// the first injected fault will change the believed currentPos and nextPos for the turtle
				if(injectedFault1 && turtles.get(i).enteredGrid()) { 
					ArrayList<Position> adjPoints = getAdjacentPoints(currentPos);
					Position believedCurrentPos = adjPoints.get(new Random().nextInt(adjPoints.size()));
					movementAction nextPlannedMove = getNextMove(currentPos, nextPos);
					Position believedNextPos = getNextPosition(believedCurrentPos, nextPlannedMove); 
					if(!believedNextPos.inGrid(gridSize))
						believedNextPos = new Position(believedCurrentPos);
					currentPos = believedCurrentPos;
					nextPos = believedNextPos;
				}	

				// If the next planned position is 'unsafe' for the turtle, the correct behavior would be changing the plan.
				// However, the turtle may still move to its next planned position if the relevant injected fault is triggered
				if(otherCurrentPositions.contains(nextPos) || higherPriorityNextPlans.contains(nextPos)) {

					if(!turtles.get(i).enteredGrid()) {
						turtles.get(i).plannedNextMoves.add(0, currentPos);
						continue;
					}
					
					// Considering the current position, which is safe, and the other safe adjacent positions
					// choosing one of those safe positions RANDOMLY to move in the next step
					// if an adjacent position is chosen, the plan to the final goal should be regenerated from there
					// safe adjacent point: not currently occupied, not planned to visit by higher priority turtles
					// if that action is not possible, the agent just waits in its current position
					
					ArrayList<Position> safePositions = new ArrayList<Position>();
					safePositions.add(currentPos); 
					// the list of adjacent positions that no turtle stays there and
					// no turtle with higher priority is going to go there in the next
					
					// both injected fault 2 and fault 3 will unable the turtle from considering the current position
					// of the other adjacent positions or the next plan of the high priority turtles
					for(Position p: getAdjacentPoints(currentPos)) 
						if( (injectedFault2 || injectedFault3) || 
							(!otherCurrentPositions.contains(p) && !higherPriorityNextPlans.contains(p)) ) {
							safePositions.add(p);
						}
//						if( (!otherCurrentPositions.contains(p)   && !injectedFault2) ||
//							(!higherPriorityNextPlans.contains(p) && !injectedFault3) )
//							safePositions.add(p);

					int randomMove = (new Random()).nextInt(safePositions.size());
					movementAction nextMove = getNextMove(currentPos, safePositions.get(randomMove));
					if(nextMove == movementAction.nop) // deciding to stand still - safe move, no need extra consideration
						turtles.get(i).plannedNextMoves.add(0, turtles.get(i).currentPosition);
					else {
						Position nextPosToMove = getNextPosition(turtles.get(i).currentPosition, nextMove);
						if(!nextPosToMove.inGrid(gridSize)) 
							turtles.get(i).plannedNextMoves.add(0, turtles.get(i).currentPosition);
						else {
							turtles.get(i).replan(nextPosToMove, turtles.get(i).getGoal());
						}
					}
				} 
				// else: the next planned move is safe
				
				higherPriorityNextPlans.add(turtles.get(i).getNext());	// Defining the intention of higher priority turtles for the next moves	
			} 
			
			for(Turtle turtle: turtles) {
				turtle.moveNext();	// Executing the planned moves
//				System.out.println("Turtle "+ turtle.number+ " goes to "+turtle.currentPosition+"\n");
			}
					
			if(turtles.get(0).executedMoves.size()>100) {
				System.out.println("Possibility of deadlock.");
				System.out.println("....");
				break;
			}
		}
		
		// returning all executed moves by all turtles 
		List<ArrayList<Position>> executedMovesAll = turtles.stream()
				.map(turtle->turtle.executedMoves)
				.toList();
		return new ArrayList<ArrayList<Position>>(executedMovesAll);	
	}

	// returns the adjacent points of the given position
	private ArrayList<Position> getAdjacentPoints(Position p){
		ArrayList<Position> ans = new ArrayList<Position>();
		
		if(p.x-1>=0)
			ans.add(new Position(p.x-1, p.y));
		if(p.x+1<gridSize.x)
			ans.add(new Position(p.x+1, p.y));
		if(p.y-1>=0)
			ans.add(new Position(p.x, p.y-1));
		if(p.y+1<gridSize.y)
			ans.add(new Position(p.x, p.y+1));
		return ans;	
	}
	
	// returns true if all turtles has reached their goals, returns fail otherwise
	public boolean completeMission() {
		for (Turtle turtle : turtles)
			if(!turtle.completedMission())
				return false;
		return true;
	}
	
	// returns true if there are more than one turtles at one point at the same time in the executedMoves
	public static boolean hasCollision(ArrayList<ArrayList<Position>> executedMoves) {
		for(int time=0;time<executedMoves.get(0).size();time++) {	
			ArrayList<Position> positionsAtSameTime = new ArrayList<>();
			for(int turtleNum=0;turtleNum<executedMoves.size();turtleNum++) {
				
				Position p = executedMoves.get(turtleNum).get(time);
				if(positionsAtSameTime.contains(p)) {
					System.out.println("Collision at step: " +time+ " , position:"+p);
					return true;
				}
				else
					positionsAtSameTime.add(p);
			}
		}
		return false;
	}
	
	// this function tests when there is no fault in the turtles movement,
	// they will move with no collision with the others
	// TODO: It seems the stepsFault should be defined for each turtle here to a list of all zeros 
	// 		 or, make the runTurtles function avoid generating faults for the agents
	public static void testSafeTurtles() {
		Position grid  = new Position(10, 10);
		int numOfTests = 100000;
		int turtleNumsInGrid = 5;
		int wSteps = 5;
		int dSteps = 5;
		
		for (int testNum = 0; testNum <numOfTests; testNum++) {
			System.out.println("----- Runnint test: "+testNum+ " ------");
			ArrayList<Turtle> turtlesList = new ArrayList<Turtle>();
			for(int turtleNum=1;turtleNum<=turtleNumsInGrid;turtleNum++) {
				turtlesList.add( new Turtle(turtleNum, grid, wSteps, dSteps));
			}
			
			
			SafeTurtles safeTurtles = new SafeTurtles(turtlesList,grid);
			ArrayList<ArrayList<Position>> executedPaths = safeTurtles.runTurtles();
			
			if(hasCollision(executedPaths)) {
				System.out.println("Test Failed: a collision has happended by turtles.");
				for(ArrayList<Position> path:executedPaths) 
					System.out.println(path);
				return;
			}	
			System.out.println("Test "+ testNum+ " is finished successfully.");
		}		
	}
	
	// This function reads the whole SUT test input from the given file
	private SafeTurtles readSUTInput(String fileName){
		/*
		An example of the file content should be like this:
		Grid Size: (10,10)
		Turtle ID:1 - Start: (5,8) - Moves:up,down,down,down,down, - Faults:3,2,3,1,0,2,2,3,0,0,
		Turtle ID:2 - Start: (7,6) - Moves:right,right,up,right, - Faults:1,3,2,1,2,2,2,1,1,1,
		Turtle ID:3 - Start: (4,2) - Moves:down,down,right, - Faults:0,2,2,0,0,2,3,1,2,1,
		Turtle ID:4 - Start: (1,7) - Moves:up,up, - Faults:3,0,2,1,3,2,3,1,3,0,
		Turtle ID:5 - Start: (8,5) - Moves:up,up,up,up,up, - Faults:3,1,3,2,0,1,0,1,0,0,
		*/

		Position newGridSize = null;
		ArrayList<Turtle> newTurtles = new ArrayList<Turtle>();
		
		try {
			File myFile = new File(fileName);
			Scanner myReader = new Scanner(myFile);

			if (myReader.hasNext()) {
				// the first line declares grid size: in the form of 'Grid Size: (X,Y)'
				String XY = myReader.nextLine().split(":")[1].replace("(", "").replace(")", "");
				int gridX = Integer.parseInt(XY.strip().split(",")[0]);
				int gridY = Integer.parseInt(XY.strip().split(",")[1]);
				newGridSize = new Position(gridX, gridY);
			}

			while (myReader.hasNextLine()) {
				String data = myReader.nextLine();
				if (data.replaceAll("\\s+", "").isEmpty())
					break;
				int id = Integer.parseInt(data.split("-")[0].split(":")[1].strip());

				String start = data.split("-")[1].split(":")[1].replace("(", "").replace(")", "");
				int startX = Integer.parseInt(start.split(",")[0].strip());
				int startY = Integer.parseInt(start.split(",")[1].strip());
				Position startPosition = new Position(startX, startY);

				String[] moves = data.split("-")[2].split(":")[1].strip().split(",");
				ArrayList<movementAction> turtleMoves = new ArrayList<>();
				for (String move : moves) {
					movementAction turtleMove = switch (move) {
					case "up":
						yield movementAction.up;
					case "down":
						yield movementAction.down;
					case "left":
						yield movementAction.left;
					case "right":
						yield movementAction.right;
					case "nop":
						yield movementAction.nop;
					default:
						throw new IllegalArgumentException("Unexpected value: " + move);
					};
					turtleMoves.add(turtleMove);
				}
				
				String[] faultsStr = data.split("-")[3].split(":")[1].strip().split(",");
				ArrayList<Integer> faultsInt = new ArrayList<>();
				for (String str:faultsStr)
					faultsInt.add(Integer.parseInt(str));
					
				Turtle newTurtle = new Turtle(id, newGridSize, startPosition, turtleMoves, faultsInt);
				newTurtles.add(newTurtle);
			}
			myReader.close();
		} catch (FileNotFoundException e) {
			System.out.println("An error occurred.");
			e.printStackTrace();
		}
		
		return new SafeTurtles(newTurtles, newGridSize);	
	}
	
	// returns true if two SafeTurtles have the same faults for each turtle
	public static boolean haveTheSameStartsAndFaults(SafeTurtles st1, SafeTurtles st2) {
		
		if(st1.turtles.size()!=st2.turtles.size())
			return false;
		for(int tIndex=0; tIndex<st1.turtles.size();tIndex++) {			
			if(!st1.turtles.get(tIndex).stepsFault.equals(st2.turtles.get(tIndex).stepsFault))
				return false;
		}
		
		return true;		
	}
	
	/*
	 * This function waits on the port number '1234' to receive the file name that includes one SUT test input.
	 * By receiving the file name, it assumes that the file is located in the root project folder, and tries to execute that on the SUT.
	 * The SUT test input and the result of executing that will be saved in another folder named 'SUT inp-out log - ordered'.
	 * The results will be saved with an index in that folder, and will be cleared before the each execution of the function.
	 * It will return 'Pass' or 'Fail' to the same IP that requested the text execution (on the same port '1234') at the end.
	 */
	public static void runServer() {

		int receivePort =1234;
		int sendPort = 6789;
		
		DatagramSocket connectionSocket = null;
		
		final String executionFolderName = "SUT inp-out log - ordered";
		File reportDirectory = new File(executionFolderName);
	
		//cleaning the previous logs if it exists
		if(reportDirectory.exists()) {		
			try {
				Files.walk(reportDirectory.toPath())
				.sorted(Comparator.reverseOrder())
				.map(Path::toFile)
				.forEach(File::delete);
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}

		reportDirectory.mkdir();  // it will do nothing if the folder is already there
		
		final String inputsFolderName 	 = executionFolderName+File.separator+"inputs";
		final String outputsFolderName 	 = executionFolderName+File.separator+"outputs";
		File inputsDirectory = new File(inputsFolderName);
		File outputsDirectory = new File(outputsFolderName);
		inputsDirectory.mkdir();
		outputsDirectory.mkdir();
		
		String runReport = executionFolderName+File.separator+"All pass-fails.txt";
		
		try {
			FileWriter myWriter = new FileWriter(runReport, true); 
			
			System.out.println("Starting SUT Server..");
			connectionSocket = new DatagramSocket(receivePort);
			byte[] buf = new byte[1024];	
			
			int runCounter=1;
			while(true) {
				DatagramPacket receivingPacket= new DatagramPacket(buf, 1024);
				connectionSocket.receive(receivingPacket);
				String receivedFileName = new String(receivingPacket.getData(), 0, receivingPacket.getLength());			
				
				SafeTurtles safeTurtles = new SafeTurtles(receivedFileName);
				ArrayList<ArrayList<Position>> executedPaths = safeTurtles.runTurtles();
				
				if(hasCollision(executedPaths)) {
					System.out.println("SUT run "+ runCounter+ " is finished. --> Collision");
					myWriter.write("SUT run "+ runCounter+ " is finished. --> Collision\n");
					myWriter.flush();
				}
				else {
					System.out.println("SUT run "+ runCounter+ " is finished. --> Safe");
					myWriter.write("SUT run "+ runCounter+ " is finished. --> Safe\n");
					myWriter.flush();
				}
				String savingInpFileName = 	inputsFolderName+ File.separator + "Run " + runCounter;
				File savingInpFile = new File(savingInpFileName);

				Files.copy(new File(receivedFileName).toPath(),savingInpFile.toPath());
				
				String outputFileName = outputsFolderName+ File.separator + "Run " +runCounter;
				saveTestSUTReport(outputFileName,executedPaths);
				
				runCounter++;

				String testReport = hasCollision(executedPaths) ? "Fail": "Pass";
				InetAddress ia = receivingPacket.getAddress();
				DatagramPacket sendingPacket= new DatagramPacket(testReport.getBytes(), testReport.length(), ia, sendPort);
				// System.out.println("before sending.");
				connectionSocket.send(sendingPacket);	
			}
		} 
		catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		finally {
			if(connectionSocket!= null)
				connectionSocket.close();
		}
	}

	/*
	 * Using the Erlang/qc code, we attempt to detect 100 faults in the SUT and get the results for each filter.
	 * To detect each fault, the Erlang/qc code sends test inputs to the server, until the test gets failed for one input.
	 * Then, the Erlang/qc code will try to modify that input and sends such inputs to the execution until
	 * finding the most shrunk case.
	 * This function will separate the requests/results for fault detection and shrinking process 
	 * for each fault and for each filter.
	 * This separation will be done by comparing the requested test inputs from the Erlang/qc.
	 * The test inputs include requested faults for each filter, that is not implemented to be shrunk by the Erlang/qc code.
	 * Therefore, if the faults for all turtles are the same in two consecutive requests are not the same, we assume that
	 * those requests are in the process of fault detection. Otherwise, we will assume that those one are in the process of
	 * shrinking a failed test case.
	 * 
	 * The function expects to have a sequence of test inputs and the results of executing them in the following folders:
	 * "exp1-SUT3\G-20-A-5-W-5-D-5-R-100-GenD--SUT-randFilterOrCorrect\SUT inp-out log - ordered\inputs" and 
	 * "exp1-SUT3\G-20-A-5-W-5-D-5-R-100-GenD--SUT-randFilterOrCorrect\SUT inp-out log - ordered\outputs" and 
	 * 
	 * The function saves the results in the folder: "SUT inp-out log - formatted"
	 */
	public static void reformatInputsOutputs() throws IOException {
		// it is not working since the noshrink() function of QuickCheck is not working as we expected
		// QuickCheck still shrinks the generator for a few steps after reaching a failure
		// the next approach is to use the test report to find out which runs are for shrinking and which ones
		// are for shrinking --> reformatInputsOutputs1()
		String[] testFilters = new String[]{"NoFilter","F1","F2","F3"};
		String qcExpTestReportFolder = "exp1-SUT3"+File.separator+"G-20-A-5-W-5-D-5-R-100-GenD--SUT-randFilterOrCorrect";
		int faultDetectionNum = 100;
		
		String inpFolder	= "SUT inp-out log - ordered"+File.separator+"inputs";
		String outFolder	= "SUT inp-out log - ordered"+File.separator+"outputs";		
		String resultFolder	= "SUT inp-out log - formatted";  // the saved SUT input/outputs, not formatted ones

		if((new File(resultFolder)).exists()) {	
			//cleaning the folder of the previous runs
			try {
				System.out.println("Cleaning the folder..");
				Files.walk(new File(resultFolder).toPath())
					.sorted(Comparator.reverseOrder())
					.map(Path::toFile)
					.forEach(File::delete);
					System.out.println("Cleaning is done.");
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}

		int runIndex = 1;
		for(String testFilter: testFilters) {	
			
			File stepsFile = new File(resultFolder +File.separator+testFilter+File.separator+"Fault Detection Steps.txt");
			stepsFile.getParentFile().mkdirs();
			stepsFile.createNewFile();		
			FileWriter stepsWriter = new FileWriter(stepsFile.toString(),true);
			
			// Assuming we detected faults in 100 times for each filter and no filter
			// Each 100 faults belong to one category
			for (int faultIndex=1; faultIndex<=faultDetectionNum; faultIndex++) {
				
				SafeTurtles safeTurtlesCanditate = new SafeTurtles(inpFolder+File.separator+"Run "+ runIndex);
				SafeTurtles safeTurtlesNext 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex+1));
											
				String faultFolder = resultFolder+File.separator+testFilter+File.separator+"Fault Detection"+File.separator+"Fault "+ faultIndex;
				File faultDetectionInputs  = new File(faultFolder +File.separator+ "Fault Detection Process"+File.separator+ "Inps.txt");
				File faultDetectionOutputs = new File(faultFolder +File.separator+ "Fault Detection Process"+File.separator+ "Outs.txt");	
				faultDetectionInputs.getParentFile().mkdirs();
				faultDetectionInputs.createNewFile();
				faultDetectionOutputs.createNewFile();
				
				int counterFD=1;
				int totalSteps = 0; // the total number of steps (time unit) up to detection the fault
				while(!haveTheSameStartsAndFaults(safeTurtlesCanditate, safeTurtlesNext)) { // the fault has not revealed yet

					copyFile("Detection Run "+counterFD,inpFolder+File.separator+"Run "+ runIndex, faultDetectionInputs.toString());
					copyFile("Detection Run "+counterFD,outFolder+File.separator+"Run "+ runIndex, faultDetectionOutputs.toString());     
					
					totalSteps+=getExecutedSteps(outFolder+File.separator+"Run "+ runIndex);			
					counterFD++;
					runIndex++;
					
					safeTurtlesCanditate = safeTurtlesNext;
					safeTurtlesNext 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex+1));
					
				}			
				// the fault is revealed
				copyFile("Fault Detection Run "+counterFD,inpFolder+File.separator+"Run "+ runIndex, faultDetectionInputs.toString());
				copyFile("Fault Detection Run "+counterFD,outFolder+File.separator+"Run "+ runIndex, faultDetectionOutputs.toString());     
				
				totalSteps+=getExecutedSteps(outFolder+File.separator+"Run "+ runIndex);		
				//counterFD++;
				runIndex++;
				
				safeTurtlesCanditate = safeTurtlesNext;
				safeTurtlesNext 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex+1));		
				
				stepsWriter.write(totalSteps+"\n");
				
				// Starting the shrinking process
				
				File shrinkInputs  = new File(faultFolder +File.separator+ "Shrinking Process"+File.separator+ "Inps.txt");
				File shrinkOutputs = new File(faultFolder +File.separator+ "Shrinking Process"+File.separator+ "Outs.txt");		
				shrinkInputs.getParentFile().mkdirs();
				shrinkInputs.createNewFile();
				shrinkOutputs.createNewFile();
				
				int counterShrink=1;
				while(haveTheSameStartsAndFaults(safeTurtlesCanditate, safeTurtlesNext)) {
					copyFile("Shrinking Run "+counterShrink,inpFolder+File.separator+"Run "+ runIndex, shrinkInputs.toString());
					copyFile("Shrinking Run "+counterShrink,outFolder+File.separator+"Run "+ runIndex, shrinkOutputs.toString());     

					counterShrink++;
					runIndex++;
					if( !((new File(outFolder+File.separator+"Run "+ (runIndex+1))).exists()) )
						break;
										
					safeTurtlesCanditate = safeTurtlesNext;
					safeTurtlesNext 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex+1));			
				}
				// the last shrink file
				copyFile("Shrink Run "+counterShrink,inpFolder+File.separator+"Run "+ runIndex, shrinkInputs.toString());
				copyFile("Shrink Run "+counterShrink,outFolder+File.separator+"Run "+ runIndex, shrinkOutputs.toString()); 
				runIndex++; 
			}
			stepsWriter.close();
		}
	}
	
	/*
	 * This function takes one of those execution files and returns its results in the real order of test executions.
	 * After executing the erlang/quickcheck code, we will have some details about executing the tests with different filters.
	 * For each filter, we will have a folder named 'raw' that contains the number of 'discard', 'failedShrinkSteps',
	 * 'ShrinkSteps', and 'TestsNum'. Each of those files contain the results of executing tests in reverse order.
	 * An example of the content of such file is in the following, for the file named 'TestsNum.txt':
	 * 
                     #Turtle=5       
	 Grid=10*10      13             
	 Grid=10*10      1              
	 Grid=10*10      7              
	 Grid=10*10      3              
	 Grid=10*10      7              
	 Grid=10*10      28             
	 Grid=10*10      8  
	       
	 */
	public static ArrayList<Integer> getTestExecutions(String fileName) throws IOException{
		File file = new File(fileName);
		List<String> lines = Files.readAllLines(file.toPath());
//		lines.remove(0);  // the first line of the file includes grid information
		ArrayList<Integer> ans = new ArrayList<>(lines.stream()
				.filter(line->line.split("\\s+").length>2)
				.map(line->Integer.parseInt(line.split("\\s+")[2].strip())).toList());
		// the data is saved in the file in a reverse way
		Collections.reverse(ans);
		return ans;
		
	}
	
	//  no use of this function - wrong approach to implement.
	
	public static void reformatInputsOutputs1() throws Exception {
		
		/*
		 * Implementation Challenges:
		 * 1)QuickCheck returns the number of test executions before the failed test case.
		 * 2)noshrink() 'helps' a lot in avoiding to shrink a generator values but it is not guaranteed
		 * 3)When a test is failed in the first try, QuickCheck does not return '0' as 
		 * the number of test executions before failure. It returns '1' instead.
		 * 4) I faced that QC generator commands are not deterministic. The value
		 * that the generators provide can be completed by time. 
		 * For example the part: "?LET(XXX, Wiggling,lists:append(XXX))" that we used in our generator
		 */
		
		String expFolder 	="exp1-SUT3"+File.separator+"G-10-A-5-W-5-D-5-R-100-GenD--SUT-randFilterOrCorrect";
		
		String inpFolder	= expFolder+File.separator+"RunSUTReport"+File.separator+"inputs";
		String outFolder	= expFolder+File.separator+"RunSUTReport"+File.separator+"outputs";		
		String resultFolder	= expFolder+File.separator+"FormattedResults";

		File rfFile = new File(resultFolder);

		if(rfFile.exists()) {	
			//cleaning the folder
			try {
				System.out.println("Cleaning..");
				Files.walk(rfFile.toPath())
					.sorted(Comparator.reverseOrder())
					.map(Path::toFile)
					.forEach(File::delete);
					System.out.println("Cleaning is done.");
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
		
		String[] filterFolders = new String[] {"NoFilter", "F1", "F2", "F3"};			
		int runIndex = 1;
		
		for(String filter:filterFolders) {
			ArrayList<Integer> testNums 		= getTestExecutions(expFolder+File.separator+filter+File.separator+"Raw"+File.separator+"TestsNum.txt");
			ArrayList<Integer> succShrinkNums   = getTestExecutions(expFolder+File.separator+filter+File.separator+"Raw"+File.separator+"ShrinkSteps.txt");
			ArrayList<Integer> failedShrinkNums = getTestExecutions(expFolder+File.separator+filter+File.separator+"Raw"+File.separator+"FailedShrinkSteps.txt");
			
			File stepsFile = new File(resultFolder +File.separator+filter+File.separator+"ExecutedStepsTillCollision");
			stepsFile.getParentFile().mkdirs();
			stepsFile.createNewFile();
			FileWriter stepsWriter = new FileWriter(stepsFile.toString(),true);
			
			for(int faultNum=0;faultNum<testNums.size();faultNum++) {
				
				int totalSteps = 0; // the total number of steps (time unit) up to detection the fault
				
				String faultFolderNew = resultFolder+File.separator+filter+File.separator+"Fault "+ (faultNum+1);
				
				// Fault detection process
				int numOfTestRuns;
				// QuickCheck returns the number of test executions before reaching a failure
				// Logically, if the failure is reached in the first try, QuickCheck should report '0' as this number
				// However, when the test fails in the first try, it reports '1' as this number
				
				if(testNums.get(faultNum)==1) {
					SafeTurtles safeTurtlesCanditate = new SafeTurtles(inpFolder+File.separator+"Run "+ runIndex);
					SafeTurtles safeTurtlesNext 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex+1));
					if(haveTheSameStartsAndFaults(safeTurtlesCanditate, safeTurtlesNext))
						numOfTestRuns = 1;
					else
						numOfTestRuns = 2;
				}
				else
					numOfTestRuns = testNums.get(faultNum)+1;
				
				
				for(int i=1;i<=numOfTestRuns;i++,runIndex++) { 				

					File faultDetectionInputs  = new File(faultFolderNew +File.separator+ "FaultDetection"+File.separator+ "Inps.txt");
					File faultDetectionOutputs = new File(faultFolderNew +File.separator+ "FaultDetection"+File.separator+ "Outs.txt");	
					faultDetectionInputs.getParentFile().mkdirs();
					faultDetectionInputs.createNewFile();
					faultDetectionOutputs.createNewFile();

					copyFile("Detection Run "+i+", "+runIndex,inpFolder+File.separator+"Run "+ runIndex, faultDetectionInputs.toString());
					copyFile("Detection Run "+i+", "+runIndex,outFolder+File.separator+"Run "+ runIndex, faultDetectionOutputs.toString()); 
					
					totalSteps+=getExecutedSteps(outFolder+File.separator+"Run "+ runIndex);	
					
					boolean collision = getHasCollision(outFolder+File.separator+"Run "+ runIndex);
					if(i<numOfTestRuns && collision) {
						System.out.println("Error: run more than needed in filter "+ filter+ ", fault "+ (faultNum+1));
					}
				}				
				stepsWriter.write(totalSteps+"\n");

				SafeTurtles previousST   = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex-1));
				SafeTurtles currentST 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ runIndex);
				if(!haveTheSameStartsAndFaults(previousST, currentST))
					System.out.println("Error: not shrinking the last test in filter "+ filter+", fault "+ (faultNum+1));

							
				//shrinking process
				for(int i=1;i<=succShrinkNums.get(faultNum)+failedShrinkNums.get(faultNum);i++,runIndex++) {
					File shrinkInputs  = new File(faultFolderNew +File.separator+ "Shrink"+File.separator+ "Inps.txt");
					File shrinkOutputs = new File(faultFolderNew +File.separator+ "Shrink"+File.separator+ "Outs.txt");		
					shrinkInputs.getParentFile().mkdirs();
					shrinkInputs.createNewFile();
					shrinkOutputs.createNewFile();
					
					copyFile("Shrink Run "+i+", "+runIndex,inpFolder+File.separator+"Run "+ runIndex, shrinkInputs.toString());
					copyFile("Shrink Run "+i+", "+runIndex,outFolder+File.separator+"Run "+ runIndex, shrinkOutputs.toString()); 

					// previousST   = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex-1));
					// currentST 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ runIndex);
					// if(!haveTheSameStartsAndFaults(previousST, currentST))
					// 	throw new Exception("Error in filter: "+ filter+", fault:"+ faultNum+ " inside shrinking." );
				}

				File currentFile = new File(inpFolder+File.separator+"Run "+ runIndex);
				if(currentFile.exists()){
					previousST   = new SafeTurtles(inpFolder+File.separator+"Run "+ (runIndex-1));
					currentST 	 = new SafeTurtles(inpFolder+File.separator+"Run "+ runIndex);
					if(haveTheSameStartsAndFaults(previousST, currentST))
						System.out.println("Error in copying all shrink files in filter "+ filter+", fault "+ (faultNum+1) );
				}			
			}
			
			stepsWriter.close();
		}
		
		File notExistingRunFile = new File(inpFolder+File.separator+"Run "+ runIndex);
		if(notExistingRunFile.exists())
			throw new Exception("An Error Occured in reformatting input/outputs.");
		
	}
	
	/*
	 * It takes an execution results file and returns the number of execution steps before the SUT execution is finished.
	 * This information exist in the first line of the such file.
	 * The first line of the execution results file is like below:
	 * '#Info# Executed steps before collision: 6, Found collision: false'
	 */
	private static int getExecutedSteps(String fileName) throws IOException {
		File myFile = new File(fileName);
		Scanner myReader = new Scanner(myFile);

		int numberOfSteps = -1;
		if (myReader.hasNextLine()) {
			String data = myReader.nextLine();
			if(data.startsWith("#Info#")) {
				numberOfSteps = Integer.parseInt( data.split(",")[0].split(":")[1].strip() );
			}			
		}
		// TODO Auto-generated method stub
		myReader.close();
		return numberOfSteps;
	}

	/*
	 * It takes an execution results file and returns true if it contains any turtle collisions, false otherwise.
	 * This information exist in the first line of the such file.
	 * The first line of the execution results file is like below:
	 * '#Info# Executed steps before collision: 6, Found collision: false'
	 */
	private static Boolean getHasCollision(String fileName) throws IOException {
		File myFile = new File(fileName);
		Scanner myReader = new Scanner(myFile);

		Boolean collision = null ;
		if (myReader.hasNextLine()) {
			String data = myReader.nextLine();
			if(data.startsWith("#Info#")) {
				myReader.close();
				return (Boolean.parseBoolean( data.split(",")[1].split(":")[1].strip() ));
			}			
		}
		myReader.close();
		return collision;
	}

	// It copies the file located in 'sourceFile' to 'destinationFile' and add 'title' as the first line of the 'destinationFile'
	public static void copyFile(String title, String sourceFile, String destinationFile) throws IOException {
		FileWriter myWriter = new FileWriter(destinationFile,true);
		
		myWriter.write(title+"\n");
		for (String line: Files.readAllLines(new File(sourceFile).toPath()))
			myWriter.write(line+"\n");
		
		myWriter.write("\n");
		myWriter.close();
	}
	
	/*
	 *  It takes the execution results and save that in the given fileName.
	 *  An example of the save file in mentioned in the following:
	 *  
	    #Info# Executed steps before collision: 3, Found collision: true
		Turtle 1 :[(-1,-1), (5,8), (5,9), (5,8), (5,7), (5,6), (5,5), (-1,-1), (-1,-1), (-1,-1)]
		Turtle 2 :[(-2,-2), (7,6), (8,6), (9,6), (9,7), (9,7), (-2,-2), (-2,-2), (-2,-2), (-2,-2)]
		Turtle 3 :[(-3,-3), (4,2), (4,1), (4,0), (5,0), (-3,-3), (-3,-3), (-3,-3), (-3,-3), (-3,-3)]
		Turtle 4 :[(-4,-4), (1,7), (1,8), (1,9), (-4,-4), (-4,-4), (-4,-4), (-4,-4), (-4,-4), (-4,-4)]
		Turtle 5 :[(-5,-5), (8,5), (9,5), (9,6), (9,6), (9,6), (9,7), (8,7), (8,8), (8,9)]
	 */
	public static void saveTestSUTReport(String fileName, ArrayList<ArrayList<Position>> executedPaths) {
		
		boolean collisionFound = false;
		int time;
		
		for(time=1;time<executedPaths.get(0).size();time++) {		
			ArrayList<Position> positionsAtSameTime = new ArrayList<>();
			for(int turtleNum=0;turtleNum<executedPaths.size();turtleNum++) {
				
				Position p = executedPaths.get(turtleNum).get(time);
				if(positionsAtSameTime.contains(p)) {
					collisionFound = true;
					break;
				}
				else
					positionsAtSameTime.add(p);
			}
			if(collisionFound)
				break;
		}
		if(!collisionFound) // time is incremented by naturally exiting the loop
			time--;
		
	    try {
	        FileWriter myWriter = new FileWriter(fileName);
	        
	        myWriter.write("#Info# Executed steps before collision: "+ time+ ", Found collision: "+ collisionFound+ "\n");
	        
	        for(int turtleNum=1;turtleNum<=executedPaths.size();turtleNum++) {
	        	myWriter.write("Turtle "+turtleNum+ " :"+ executedPaths.get(turtleNum-1)+"\n");
	        }
	        myWriter.close();
	      } catch (IOException e) {
	        System.out.println("An error occurred.");
	        e.printStackTrace();
	      }	
	}
	
	
	/*
	 * To run the server, this piece of code should be executed in the same directory that the test input generator.
	 * Test data generator just notifies (via sockets) the SUT executor that the test data is ready, which is 
	 * supposed to be in the same directory.
	 * 
	 * So, please put this file close to the test input generator.
	 * Run this code, so that the server listens to the port, and take the inputs when they are ready.
	 * After execution of this code, run the test input generator, to generate the inputs.
	 * The input generator code is supposed to notify this code via a socket when the input is ready.
	 * Then, this code will execute SUT for that input and saves the test input and execution results in a file.
	 * Note that, the test generator may produce the results faster the test executor, but only the ones
	 * that will be executed will be saved with the results in the file at the end.
	 */
	
	public static void main(String[] args) {

		/*
		 * Steps:
		 * 	run the server (by commenting/uncommenting relevant files in this main method)
		 * 	run the code to prepare the test inputs file repetitively 
		 *  and notify the server to get the input and execute SUT with that input (on port 1234)
		 * 	reformat the results if needed
		 */
		
//		 running the server to listen to the requests on the port 1234
		 runServer();

		
		// reformat the results, by separating the attempts for finding fault and shrinking process
//		try {
//			System.out.println("Starting to reformat input/output..");
//			reformatInputsOutputs();
//			System.out.println("Reformatting is done.");
//		} catch (Exception e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		
		// testing with one input
//		SafeTurtles safeTurtles = new SafeTurtles("inputs_z3.txt");
//		ArrayList<ArrayList<Position>> executedPaths = safeTurtles.runTurtles();
//		
//		if(hasCollision(executedPaths)) {
//			System.out.println("Test Failed: a collision has happened by turtles.");
//			for(ArrayList<Position> path:executedPaths) 
//				System.out.println(path);
//		}
//		else
//			System.out.println("Test is passed successfully.");
		
		// testing with more inputs
		// testSafeTurtles();
	}

}

class Turtle{
	
	private boolean missionCompleted = false;
	public int number;	// ID of a turtle, that should be unique among turtles
	public ArrayList<Integer> stepsFault = new ArrayList<>(); // stepsFault[i] defines the fault of the turtle in step i
	/*
	we clarify if the turtle is going to the have a fault using 'stepsFault'
	we have three kinds of faults, mentioned in the paper, indexed with 1, 2, and 3
	if the fault in one step is zero, it means that the turtle will act correctly with no fault in that step
	for example, stepsFault = [3,2,0] clarifies that:
	in the first step, the turtle will have the fault type 3
	in the first step, the turtle will have the fault type 2
	in the first step, the turtle will act correctly, with no fault
	*/
	
	private Position outOfGridPosition; // the turtles are all out of the board at the beginning
	public Position currentPosition; 	// the current position of the trutle
	private final Position start;		// the starting position of the turtle in the grid
	private final Position goal;		// the goal position of the turtle to reach
	public Position gridSize;			// the grid size that the turtle moves on that
	public ArrayList<Position> plannedNextMoves;	// each turtle has a plan for its next moves
	public ArrayList<Position> executedMoves = new ArrayList<>();	// each turtle has some executed some moves
	
	// construct the turtle by giving movement plan
	// stepsFault will not be initialized with this constructor. They will be initialized later on if needed. 
	public Turtle(int number, Position gridSize, ArrayList<Position> movementPlan) {
		this.number = number;
		outOfGridPosition = new Position(-number,-number);
		currentPosition = new Position(outOfGridPosition);
		executedMoves.add(currentPosition);
		this.gridSize = gridSize;
		this.plannedNextMoves= movementPlan;
		this.goal = plannedNextMoves.get(plannedNextMoves.size()-1);
		this.start= plannedNextMoves.get(0);	
	}
	
	// construct the turtle by giving the starting point and a sequence of actions
	public Turtle(int number, Position gridSize, Position startPosition, 
			ArrayList<SafeTurtles.movementAction> movement_actions, ArrayList<Integer> stepsFault) {
		this.number = number;
		outOfGridPosition = new Position(-number,-number);
		currentPosition = new Position(outOfGridPosition);
		executedMoves.add(currentPosition);
		this.gridSize = gridSize;
		this.plannedNextMoves= gethPath(gridSize, startPosition, movement_actions);
		this.goal = plannedNextMoves.get(plannedNextMoves.size()-1);
		this.start= plannedNextMoves.get(0);
		this.stepsFault.addAll(stepsFault);
	}
	
	// construct the turtle with random path (for testing the code)
	// random path will be generated within the constructor using the number of waiting and displacement steps
	public Turtle(int number, Position gridSize, int randomWaitSteps, int randomDispSteps) {
		this.number = number;
		outOfGridPosition = new Position(-number,-number);
		currentPosition   = new Position(outOfGridPosition);
		executedMoves.add(currentPosition);
		this.gridSize = gridSize;
		this.plannedNextMoves = makeRandomPlan(gridSize, randomWaitSteps, randomDispSteps);
		System.out.println("Turtle "+number+": "+this.plannedNextMoves);
		this.goal = plannedNextMoves.get(plannedNextMoves.size()-1);
		this.start= plannedNextMoves.get(0);
	}
	
	// generated a random path, with a random starting point within the grid, and random waiting and dis. steps afterwards
	public static ArrayList<Position> makeRandomPlan(Position grid, int randomWaiSteps, int randomDispSteps) {
		Position randomStartPos = new Position(1+new Random().nextInt(grid.x), 1+new Random().nextInt(grid.y));
		ArrayList<SafeTurtles.movementAction> moves = new ArrayList<>();
		moves.addAll(Collections.nCopies(randomWaiSteps, SafeTurtles.movementAction.nop));
		for(int i=0;i<randomDispSteps;i++) {
			SafeTurtles.movementAction randMove = SafeTurtles.movementAction.values()[ new Random().nextInt(4)];
			moves.add(randMove);
		}
		Collections.shuffle(moves);
		return gethPath(grid,randomStartPos,moves);
	}
	
	// returns the path generated by the starting point and a sequence of movement actions
	public static ArrayList<Position> gethPath(Position grid, Position startPos, ArrayList<SafeTurtles.movementAction> moves) {
		
		ArrayList<Position> path = new ArrayList<Position>();
		path.add(startPos);
		
		for(SafeTurtles.movementAction act:moves) {
			Position lastPosition = path.get(path.size()-1);
				
			Position next = switch (act) {
				case up: 	yield new Position(lastPosition.x, lastPosition.y+1);
				case down: 	yield new Position(lastPosition.x, lastPosition.y-1);
				case left: 	yield new Position(lastPosition.x-1, lastPosition.y);
				case right: yield new Position(lastPosition.x+1, lastPosition.y);
				case nop: 	yield new Position(lastPosition.x, lastPosition.y);						
				default:	throw new IllegalArgumentException("Unexpected Action: " + act);
			};
			if(next.inGrid(grid))
				path.add(next);	
			else
				path.add(lastPosition);
		}				
		return path;
	}
	
	// making a random (shortest) plan from psition 'fromPosition' to position 'toPosition'
	// updating 'plannedNextMoves' with the new plan
	public void replan(Position fromPosition,Position toPosition) {
		int horozontalDistance = toPosition.x - fromPosition.x;
		int verticalDistance   = toPosition.y - fromPosition.y;
		
		ArrayList<SafeTurtles.movementAction> acts = new ArrayList<SafeTurtles.movementAction>();
		
		if(horozontalDistance>0)
			for (int i = 0; i < horozontalDistance; i++)
				acts.add(SafeTurtles.movementAction.right);
		else
			for (int i = 0; i < Math.abs(horozontalDistance); i++)
				acts.add(SafeTurtles.movementAction.left);
		
		if(verticalDistance>0)
			for (int i = 0; i < verticalDistance; i++)
				acts.add(SafeTurtles.movementAction.up);
		else
			for (int i = 0; i < Math.abs(verticalDistance); i++)
				acts.add(SafeTurtles.movementAction.down);
		
		Collections.shuffle(acts);
		
		plannedNextMoves.clear();
		plannedNextMoves.add(fromPosition);
		for(SafeTurtles.movementAction act:acts) {
			Position lastPosition=plannedNextMoves.get(plannedNextMoves.size()-1);
				
			Position next = switch (act) {
				case up: 	yield new Position(lastPosition.x, lastPosition.y+1);
				case down: 	yield new Position(lastPosition.x, lastPosition.y-1);
				case left: 	yield new Position(lastPosition.x-1, lastPosition.y);
				case right: yield new Position(lastPosition.x+1, lastPosition.y);
				case nop: 	yield new Position(lastPosition.x, lastPosition.y);						
				default:	throw new IllegalArgumentException("Unexpected Action: " + act);
			};
			plannedNextMoves.add(next);			
		}
		
		if(plannedNextMoves.isEmpty()) {// when the current position is the same goal position
			plannedNextMoves.add(currentPosition);
		}
	}
	
	// returns the next point in the plan, if any
	public Position getNext() {
		if(plannedNextMoves.isEmpty())
			return null;
		return plannedNextMoves.get(0);
	}
	
	public Position getGoal() {
		return goal;
	}
	
	public Position getStart() {
		return start;
	}
	
	// takes the neext position from the planned path and moves there
	public void moveNext() {
		if(missionCompleted) {
			currentPosition = new Position(outOfGridPosition);
			executedMoves.add(currentPosition);
		}
		else {
			Position next = plannedNextMoves.remove(0);
			currentPosition = next;
			executedMoves.add(next);
			
			if(plannedNextMoves.isEmpty())
				missionCompleted=true;
		}
	}
	
	public boolean enteredGrid() {
		return !currentPosition.equals(new Position(outOfGridPosition));
	}
	
	public boolean completedMission() {
		return missionCompleted;
	}
	
}

class Position{
	public int x;
	public int y;
	
	public Position(Position p) {
		this.x = p.x;
		this.y = p.y;
	}
	
	public Position(int x, int y) {
		this.x = x;
		this.y = y;
	}
	
	public Position(String P) { // P in the form of (X,Y)
		String XY = P.replace("(", "").replace(")", "");
		this.x = Integer.parseInt(XY.strip().split(",")[0]);
		this.y = Integer.parseInt(XY.strip().split(",")[1]);
	}
	
	boolean inGrid(Position grid) {
//		if(x>=0 && x<grid.x && y>=0 && y<grid.y)
		if(x>=1 && x<=grid.x && y>=1 && y<=grid.y)
			return true;
		return false;
	}
	
	@Override
	public String toString() {
		return "("+x+","+y+")";
	}
	
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof Position) {
			Position p = (Position) obj;
			return (p.x==this.x && p.y==this.y);
		}
		return false;
	}
}
