import java.util.ArrayList;
import java.util.Arrays;

public class SplashSearch{
  /*set up vars.
  * create the master node
  * set forced assignment nodes
  * enter a do while loop for the entire search(end is master&no children)
  *   enter a loop that generates nodes if active then descends
  *   checks for best solution (or creates first best solution)
  *   climbs up until node with children is found \
  *    (closing nodes that are childless)
  */

  int[][][] searchArray = new int[8][8][2];
  Node currentNode = new Node();

  //Stores the lowest penalty found in tree
  int lowestPenalty = -1;

  //Stores the current best machine task assignment
  ArrayList<Integer> bestAssignment = new ArrayList<Integer>();

	//Records which machines have not been used in this branch
  ArrayList<Integer> remainingMachines = new ArrayList<Integer>(Arrays.asList(0,1,2,3,4,5,6,7));

  //Records which tasks have not been used in this branch
  ArrayList<Integer> remainingTasks = new ArrayList<Integer>(Arrays.asList(0,1,2,3,4,5,6,7));

	//Records task assignments up to and including this node
  ArrayList<Integer> currentPath = new ArrayList<Integer>(Arrays.asList(-1,-1,-1,-1,-1,-1,-1,-1));

  //Records forced assignments
  ArrayList<int[]> forcedAssignments = new ArrayList<int[]>();
  /*constructor
  * pntArray = penalty too near array
  */
  public SplashSearch(int[][][] pntArray){
    searchArray = pntArray;

    //Extract forced assignments
    Boolean firstPenFound = false;


    //For each machine
    for(int m=0 ; m <= 7 ; m++){
      //For each task
      int[] tempPair = new int[2];
      for(int j=0 ; j <= 7 ; j++){

        //If entry is valid, and is not the first valid, set value to be added to forcedAssignments
        if(searchArray[m][j][0] != -1){
          if(firstPenFound){
            firstPenFound = false;
            break;
          }
          firstPenFound = true;

          tempPair[0] = m;
          tempPair[1] = j;
        }
      }

      //After break, firstPenFound is true only if exactly one non -1 was found
      if(firstPenFound){
        forcedAssignments.add(tempPair);
      }
      firstPenFound = false;
    }
  }

  public void treeConstruct(){
    forceAssignNode();

    do{
      while(remainingMachines.size() != 0){								//Check if all possible branches have been created from current node
        buildBranches();
        if(currentNode.children.size()!=0){
          pickBestChild();
        }else{
          break;
        }

      }
      //Should always now be in a leaf

      updateBestSoln();
      backTrack();
    }while(currentNode.parent != null || currentNode.children.size() != 0);							//Repeat until node is master again

    if(lowestPenalty == -1){
     SplashOutput.printError(7);
    }
    char printTask;
    //Result
		String solutionText= "Solution";
		for(int i=0 ; i < bestAssignment.size(); i++){
      printTask = (char)('A' + bestAssignment.get(i) %26);
      solutionText+=" " + printTask + " ";
    }
    solutionText+= "; Quality: " + lowestPenalty;
    SplashOutput.printFile(solutionText);

  }


  public void forceAssignNode(){
    //Forced assignments
    for(int i=0 ; i < forcedAssignments.size() ; i++){
			int[] forcedPair = forcedAssignments.get(i);
      // new node takes parent node, mach, task, pen

 
			Node newNode = new Node(currentNode, forcedPair[0], forcedPair[1], searchArray[forcedPair[0]][forcedPair[1]][0]);

      //Close previous (parent of newNode) node so it won't make new nodes again
			currentNode.active = false;
			currentNode = newNode;									//Make the new node the current node
			remainingMachines.remove(arrayListSearch(remainingMachines, currentNode.machine));				//Remove machine and task from available list and adds to path
			remainingTasks.remove(arrayListSearch(remainingTasks, currentNode.task));
			currentPath.set(currentNode.machine, currentNode.task);
		}
  }


  public void buildBranches(){
    if(currentNode.active){								 //Check if children have never been created

        //Optional: the next machine to assign to (implement algorithm to pick best next machine)

        int nextMachine = remainingMachines.get(0);
        for(int task=0; task < remainingTasks.size(); task++)		//For each unassigned task
        {
          int nextTask = remainingTasks.get(task);
          //Create a new node which child to 'currentNode'
          int i = searchArray[nextMachine][nextTask][0];
          if(i!=-1){
            // need method for TNT and TNP

            // make a new Node(parent,mach, task, pen)
            Node node = new Node(currentNode, nextMachine, remainingTasks.get(task), searchArray[nextMachine][nextTask][0]);
            //System.out.println("Node machine: " + node.machine + "  Index: " + node.task + "   created"); // error checking line
          }

        }

        //Close 'currentNode' so it won't make new nodes again, as you have just created all possible children
        currentNode.active = false;
    }
  }


  public void pickBestChild(){
    currentNode = currentNode.children.get(0);						//Pick a node to climb down to

    //once you picked a node, make sure that same node does not make a child of it's own data
    remainingMachines.remove(arrayListSearch(remainingMachines, currentNode.machine));
    remainingTasks.remove(arrayListSearch(remainingTasks, currentNode.task));

    //add the picked node to the current solution path
    currentPath.set(currentNode.machine, currentNode.task);
  }

  public void updateBestSoln(){
    //TNT/P checking for solutions. Will remove the solution from the running if it
    //has collisions or the TNP causes it's pen to increase past the lowestPenalty
    if(remainingMachines.size()==0){
    for(int taskAtM=0; taskAtM<7;taskAtM++){
      //check all 8 mach/task pairs

      //if the task at mach i and task on mach i+1 are not TNT add TNP(or 0) to accmulated
      if(searchArray[currentPath.get(taskAtM)][currentPath.get(taskAtM+1)][1]!=-1){
        currentNode.accumulatedPenalty+=searchArray[currentPath.get(taskAtM)][currentPath.get(taskAtM+1)][1];
      }else{
        return;
      }

    }
    //if that task is on mach 8 do this
    if(searchArray[currentPath.get(7)][currentPath.get(0)][1]!=-1){
      currentNode.accumulatedPenalty+=searchArray[currentPath.get(7)][currentPath.get(0)][1];
    }else{
      //else, leave without updating best assignment
      return;
    }
    }

    if(remainingMachines.size()==0){
      if(currentNode.accumulatedPenalty < lowestPenalty || lowestPenalty == -1){
        bestAssignment.clear();
          for(Integer i : currentPath){
            bestAssignment.add(new Integer(i)); //Record new best path
          }
        lowestPenalty = currentNode.accumulatedPenalty;						//Remember lowest penalty
      }
    }
  }

  public void backTrack(){
    while(currentNode.children.size() == 0 && currentNode.parent != null){	//Until there is a child to climb down to
      currentNode.parent.children.remove(currentNode);	//Remove child node from parents
      remainingMachines.add(currentNode.machine);			//Re-add machine and task to available list and removes from path
      remainingTasks.add(currentNode.task);
      currentPath.set(currentNode.machine, -1);
      currentNode = currentNode.parent;					//Climb up
      //System.out.println("Climbed up to machine: " +currentNode.machine+ "  task: " +currentNode.task);
    }
  }

  //Finds index of an element in an array list
  static int arrayListSearch(ArrayList<Integer> list, int element)
  {
    int index = -1;

    for(int i=0; i<list.size(); i++)
    {
      if(list.get(i) == element)
      {
        index = i;
        break;
      }
    }

    return index;
  }
}
