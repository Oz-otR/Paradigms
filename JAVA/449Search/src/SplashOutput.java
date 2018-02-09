import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class SplashOutput {
	
   public static void printFile(String message) {
	   BufferedWriter bufferWriter = null;
	      try {
		 String output = message;
	         //Specify the file name and path here
		 File file = new File(Scheduler.outputFile);
		  if (!file.exists()) {
		     file.createNewFile();
		  }

		  FileWriter fw = new FileWriter(file);
		  bufferWriter = new BufferedWriter(fw);
		  bufferWriter.write(output);
	          System.out.println("Output to file successful and output to file is: " + output);


	      } catch (IOException ioe) {
		   ioe.printStackTrace();
		}
		finally
		{
		   try{
		      if(bufferWriter!=null)
			 bufferWriter.close();
		   }catch(Exception ex){
		       System.out.println("Error in closing the BufferedWriter"+ex);
		    }
		}
   }
   public static void printError(int output) {
	  int erroroutput = output;
	  String errorMessage = "test";
	  switch(erroroutput) {
	  //Catch all error for parsing
	  case 1:
		  errorMessage = "Error while parsing input file";
		  printFile(errorMessage);
		  System.exit(0);
		  break;
      //Error thrown during parsing forced partial assignment if and only if there exist two pairs with either/or the same task/machine
	  case 2:
		  errorMessage = "partial assignment error";
		  printFile(errorMessage);
		  System.exit(0);
		  break;
      //Error thrown if and only if there is an element in a pair that is not in the correct interval for machine or task
	  case 3:
		  errorMessage = "invalid machine/task";
		  printFile(errorMessage);
		  System.exit(0);
		  break;
      //Error thrown if and only if there is not 8 lines with 8 natural number entries per line in the input for machine penalites
	  case 4:
		  errorMessage = "Machine Penalty Error";
		  printFile(errorMessage);
		  System.exit(0);
		  break;
      //Error thrown in in the too near input there is a task in the three tuples that is outside the set of tasks
	  case 5:
		  errorMessage = "invalid task";
		  printFile(errorMessage);
		  System.exit(0);
		  break;
	 //Error thrown if in the Machine Penalty or the too-near task penalty that is not in the natural numbers
	  case 6:
		  errorMessage = "invalid penalty";
		  printFile(errorMessage);
		  System.exit(0);
		  break;
	//If there is no valid solution that meet the hard constraints but the file was parsed correctly and none of the above errors have been thrown.
	  case 7:
		  errorMessage = "No valid solution possible!";
		  printFile(errorMessage);
		  System.exit(0);
		  break;
	  default:
		  errorMessage = "Integer not in range ";
	  }
   }
}