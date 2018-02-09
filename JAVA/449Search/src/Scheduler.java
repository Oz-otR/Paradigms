/*
    Team: Splash Zone
	-------------------------------------------------------
	This program is designed to solve scheduling problem of pairing 8 system to 8 task.
		- Program will accept file to find the minimum solution to given problem.
*/

public class Scheduler
{
    static String outputFile;
	//
    public static void main (String[] args)
    {
        if (args.length < 2) {
            System.out.println("Please initiate program in following syntax.");
            System.out.println("java Scheduler (inputName) (outputName)");
            System.exit(0);
        }
        outputFile = args[1];
        int[][][] pntArray = new int[8][8][2];
        try {
            SplashParser parseInstance = new SplashParser(args[0]);
            
            pntArray = parseInstance.fetchMatrix();
	   
            
            for(int m = 0;m<8;m++){
                System.out.println();
              for(int t = 0;t<8;t++){

                System.out.print( "[" + pntArray[m][t][0] + "]");
              }
            }
            System.out.println();
            for(int m = 0;m<8;m++){
                System.out.print("\n");
              for(int t = 0;t<8;t++){

                System.out.print( "[" + pntArray[m][t][1] + "]");
              }
            }
            System.out.print("\n");
            
        } catch (Exception e) {
            System.out.println("Unhandled General Error");
            SplashOutput.printError(1);
        }
        
        
        SplashSearch searchInstance = new SplashSearch(pntArray);
        searchInstance.treeConstruct();
    }
    
}