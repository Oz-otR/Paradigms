import java.util.ArrayList;

public class Node {
	boolean active = true;

	//int nodePenalty = 0;										//Records this nodes penalty (may not be needed)
	int accumulatedPenalty = 0;									//Records total penalty including this node

	Node parent = null;
	ArrayList<Node> children = new ArrayList<Node>();

	int machine = -1;
	int task = -1;


	//Unconstructed node used for master
	Node(){	}


	//Regular node
	Node(Node above, int assignedMachine, int assignedTask, int penalty){
		//New branch or leaf node
		machine = assignedMachine;
		task = assignedTask;
		parent = above;				//Link child to parent
		parent.addChildNode(this);	//Link parent to child

		//nodePenalty = penalty;
		accumulatedPenalty = parent.accumulatedPenalty + penalty;
	}


	//Executed in the parent when a child is created
	public void addChildNode(Node child){
		children.add(child);					//Add child parent relationship
		//System.out.println("Parent:" + this.machine + "  was just given child: " + child.machine);
		//System.out.println("Parent:" + this.machine + "  now has # of children: " + this.children.size());

	}
}