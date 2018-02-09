import java.io.*;

public class SplashParser {

    private BufferedReader br;
    private FileReader fr;

    public int[][][] masterGrid; // General Matrix, at third place, 0 is used for Logic Matrix, and 1 is used for Task Matrix.
    //Boolean for each step of parsing string
    //Set flag as we parse
    private boolean firstFlag = false; // Checks if name was ever processed by the SplashParser.
    private boolean setName = false; //True when name is not in correct format -->name::
    private boolean setFPA = false; //Check if structure remains--> no error
    private boolean setFM = false;
    private boolean setTNT = false;
    private boolean setMP = false;
    private boolean setTNP = false;
    private boolean setCollision = false;
    private byte flagCounter = 0;
    private int lineCounter = 0; // lineCounter tracks the number of lines that have been parsed for machine penalty. If it isn't exactly 8 after MP is processed, throw error.
    private boolean expectFlags = true;

    private int execCycle = 0; //Debugging variable.

    public static final byte ASCII_INT_CHAR_FIX = -48;
    public static final byte ASCII_CAP_CHAR_FIX = -65;

    public static final byte FORBIDDEN = -1;
    public static final byte SIZEMAX = 8;
    public static final byte ASSIGNED = -2;
    public static final byte PLACEHOLDER = 0;
    public static final byte LOGIC = 0;
    public static final byte TASK = 1;

    public static final byte BRACKET = 40;
    public static final byte[] ASCII_INT_RANGE = {47,58};
    public static final byte[] ASCII_UPPER_RANGE = {64,91};
    public static final byte[] ASCII_LOWER_RANGE = {96,123};

    public SplashParser(String inputName) {
        try {
            // Program will immediately try to find the input file and read them.
	    //May need to update version to include exception handling for non-ascii input (check with prof if need to catch exception for input not in ascii --> outside of parser scope)

	    //Create new fileReader with INPUTNAME
        ////Create new BufferedReader
            fr = new FileReader(inputName);
            br = new BufferedReader(fr);

	    //tempStr is string being processed
            String tempStr = br.readLine();
	    //Verify will try to pick up the first letter of given string in first while loop
            byte verify;
	    //Current data structures before matching to Richards output

	        // Master Matrix initialization, two 8x8 matrix, 0 for logic 1 for task.
            masterGrid = new int[SIZEMAX][SIZEMAX][2];

            while (tempStr != null) {
                // If the processed string is not empty then read the first character.
                // In case newline was processed, set flag to expect either another newline, or flag setting text.
                if (!tempStr.contentEquals("")) {
                    verify = (byte) tempStr.charAt(0);
                } else {
                    verify = PLACEHOLDER; // If it is, assign null ASCII text, and trigger flag expectation.
                    expectFlags = true;
                }

                // Clears out whitespace from start, and end of the text.
                tempStr = tempStr.trim();

                // If the line starts with english letter
                if (((verify > ASCII_LOWER_RANGE[0]) && (verify < ASCII_LOWER_RANGE[1])) || ((verify > ASCII_UPPER_RANGE[0]) && (verify < ASCII_UPPER_RANGE[1]))) {
                    if (!expectFlags && !setName) {
                        parseError("inFault");
                    } else {
                        flagProcessor(tempStr);
                    }
                } else if (tempStr.startsWith("(") && tempStr.endsWith(")")) // If the line starts with bracket
                {
                    // FPA, FM, TNT, TNP takes in "(int, char)" as input. As such, it detects '('.
                    int[] processedString = pairProcessor(tempStr.substring(1, tempStr.length()-1));
                    assignOperator(processedString);
                }
                else if ((verify > ASCII_INT_RANGE[0]) && (verify < ASCII_INT_RANGE[1])) // If the line starts with number
                {
                    if (setMP == true && lineCounter < SIZEMAX) {
                        // If a line starts with number it is presumed that it is the line for Machine Penalty.
                        int[] tempValue = lineProcessor(tempStr);
                        for (byte n = 0; n < SIZEMAX; n++)
                        {
                            // Assign values to all valid possible positions in matrix.
                            if (masterGrid[lineCounter][n][LOGIC] != FORBIDDEN)
                            {
                                masterGrid[lineCounter][n][LOGIC] = tempValue[n];
                            }
                        }
                        // Line counter is used to check how many line of number has been proceed.
                        lineCounter = lineCounter + 1;
                    } else if (setName == true) {
                        // Checks for integer starting name.If name starts with number
                        setName = false;
                    } else {
                        parseError("intCrash");
                    }
                } else if (verify != PLACEHOLDER) {
                    // Three conditions: If name is supposed to be set, and gets some string, presume that was the name.
                    // If Name was already set, but still gets something, fault.
                    // Otherwise, it must have been line consist entirely of whitespaces. Skip.
                    if (!tempStr.contentEquals("")) {
                        if (setName == true) {
                            setName = false;
                        } else {
                            parseError("inFault");
                        }
                    } else {
                        expectFlags = true;
                    }
                }

                // This line will check if any valid line was processed, in that case, reset newline seeker for flags.
                if (!tempStr.contentEquals("")) {
                    expectFlags = false;
                }

                // readLine here, so EOL can be terminated correctly.
                tempStr = br.readLine();
                execCycle++;
            }

            // Handler for collision caused no-solution matrix is moved outside of actual parsing process.
            // This section also checks if Too-Near-Penalty was attempted to be parsed.
            if (setTNP == false) {
                parseError("inFault");
            } else if (setCollision == true) {
                parseError("collision");
            }

        } catch (FileNotFoundException e) {
            System.out.println("No file named " + inputName + " found, aborting.");
            e.printStackTrace();
        } catch (IOException e) {
            parseError("0");
        }
    }

    // Returns Operation/Task matrix as requested.
    public int[][][] fetchMatrix() {
        return masterGrid;
    }

    // For debug purpose, prints initialized state of operation or task matrix.
    public void printMe() {
      for(int m = 0;m<8;m++){
          System.out.println();
        for(int t = 0;t<8;t++){

          System.out.print( "[" + masterGrid[m][t][0] + "]");
        }
      }
      System.out.println();
      for(int m = 0;m<8;m++){
          System.out.print("\n");
        for(int t = 0;t<8;t++){

          System.out.print( "[" + masterGrid[m][t][1] + "]");
        }
      }
      System.out.println();
        /*System.out.println("Initiating Debug Output.");
        for (byte i=0; i < 2; i++) {
            for (byte j=0; j < SIZEMAX; j++) {
                for (byte k=0; k < SIZEMAX; k++) {
                    System.out.print("[" + masterGrid[j][k][i] + "]");
                }
                System.out.println("");
            }
            System.out.println("");
        }*/
    }

    // This function is used to assign pair wise input (FPA, FM, TNT, TNP) into appropriate data structure.
    private void assignOperator(int[] newPairs) {

        if (setTNP == true) {
            // If target location already forbidden, rewrite is not required.
            if (masterGrid[newPairs[0]][newPairs[1]][TASK] != FORBIDDEN)  {
                masterGrid[newPairs[0]][newPairs[1]][TASK] = newPairs[2];
            }
        }
        else if (setTNT == true) {
            // TNT shouldn't have any error during assignment.
            masterGrid[newPairs[0]][newPairs[1]][TASK] = FORBIDDEN;
        }
        else if (setFM == true) {
            // If FM is mapped to FPA, there will be no solution, which will be handled here.
            if (masterGrid[newPairs[0]][newPairs[1]][LOGIC] != ASSIGNED)  {
                masterGrid[newPairs[0]][newPairs[1]][LOGIC] = FORBIDDEN;
            } else {
                setCollision = true;
            }
        }
        else if (setFPA == true) {
            // Assigns FPA, also catches illegal FPA mapping.
            if (masterGrid[newPairs[0]][newPairs[1]][LOGIC] != FORBIDDEN) {
                for (byte n = 0; n < SIZEMAX; n++) {
                    masterGrid[newPairs[0]][n][LOGIC] = FORBIDDEN;
                    masterGrid[n][newPairs[1]][LOGIC] = FORBIDDEN;
                }
                masterGrid[newPairs[0]][newPairs[1]][LOGIC] = ASSIGNED;
            } else {
                parseError("fpa");
            }
        }
}

    // This function handles flags for data assignment and error checking. It will also check if invalid strings are in the input.
    // Potential fringe case --> if line being parsed starts with a bracket it will be processed and trigger a different error instead of "error while parsing input file"

    private void flagProcessor(String flagText) {
	    // Check format of initial label
        if (flagText.equals("Name:") && flagCounter == 0)
        {
            setName = true;
            firstFlag = true;
        }
        else if (flagText.equals("forced partial assignment:") && firstFlag && flagCounter == 1)
        {
            // Missing Name input check
            if (setName == true) {
                parseError("inFault");
            }
            setFPA = true;
        }
        else if (flagText.equals("forbidden machine:") && setFPA && flagCounter == 2)
        {
            setFM = true;
            setFPA = false;
        }
        else if (flagText.equals("too-near tasks:") && setFM && flagCounter == 3)
        {
            setTNT = true;
            setFM = false;
        }
        else if (flagText.equals("machine penalties:") && setTNT && flagCounter == 4)
        {
            setMP = true;
            setTNT = false;
        }
        else if (flagText.equals("too-near penalities") && setMP && flagCounter == 5)
        {
            if (lineCounter != SIZEMAX) {
                // Correct MP input is always 8 lines.
                parseError("intCrash");
            }
            setTNP = true;
            setMP = false;
        }
        else
        {
            // Checks for Name input for later ASCII index.
            if (setName == true && setFPA == false) {
                setName = false;
                flagCounter--;
            } else {
                parseError("inFault");
            }
        }
        flagCounter++;
    }

    // Ask if whitespace counts as possible task input.
    //

    // This function processes string in form of "(machine,task)", "(task,task)", or "(machine,task,penalty)" and processes into an array for data structure.
    private int[] pairProcessor(String tempStr) {
        // Counts all whitespaces, then split based on comma.
        String[] retStr = tempStr.replace(" ", "#").split(",");
        int[] retVal = new int[retStr.length];

        // Check if any extranous whitespace has been introduced.
        // Catching null text has been moved to general error catching.

        // This should be discontinued.
        for (byte i=0; i < retStr.length; i++) {
            if (retStr[i].indexOf("#") != -1) {
                parseError("inFault");
            } else if (retStr[i].length() > 1 && i < 2) {
                if (!setTNP) {
                    parseError("nullMT");
                } else {
                    parseError("nullT");
                }
            }
        }

        try {
            // Try to parse the machine (int) input.
            retVal[0] = Integer.parseInt(retStr[0]) - 1;
            if (setTNT || setTNP) {
                retVal[0] = 81;
            }
        } catch (NumberFormatException e) {
            // If it fails, it must be task or invalid, process it as though it is task
            retVal[0] = (int) retStr[0].charAt(0);
            if (setTNT || setTNP) {
                retVal[0] = retVal[0] + ASCII_CAP_CHAR_FIX;
            }
            // Just checks if FPA or FM is set at the moment... That should not take two task as input.
        }

        // Second input is always task.
        retVal[1] = (int) retStr[1].charAt(0);
        retVal[1] = retVal[1] + ASCII_CAP_CHAR_FIX;
        if (retStr.length > 2 || setTNP) {
            // If there are more than three variable discovered, it is TNP, process penalty. If program tries to process more than two entry when TNP is not being processed, or more than 3 argument is passed, trigger inFault.
            if (setTNP && retStr.length == 3) {
                retVal[2] = penaltyVerify(retStr[2]);
            } else {
                parseError("inFault");
            }
        }

        // Check to see if assignment is in expected range. If not, trigger invalid error.
        if ((retVal[1] > 7) || (retVal[1] < 0)) {
            if (!setTNP) {
                parseError("nullMT");
            } else {
                parseError("nullT");
            }
        }
        else if ((retVal[0] > 7) || (retVal[0] < 0)) {
            if (!setTNP) {
                parseError("nullMT");
            } else {
                parseError("nullT");
            }
        }

        return retVal;
    }

    // Handles line of integer for MP.
    private int[] lineProcessor(String target) {
        // Adjust the line such that it is in predictable pattern format of emptyspace-number cycle.
        String tempStr = blankCanner(target);
        String[] retStr = tempStr.split(" ");
        int[] retVal = new int[SIZEMAX];

        // If there are more or less than 8 entries, crash.
        if (retStr.length != SIZEMAX) {
            parseError("intCrash");
        }
        for (byte i=0; i < SIZEMAX; i++) {
            retVal[i] = penaltyVerify(retStr[i]);
        }

        return retVal;
    }

    // Analyzes if given panalty is valid or invalid. Checks for any non-zero, non-natural number.
    private int penaltyVerify(String target) {
        int output = PLACEHOLDER;

        try {
            output = Integer.parseInt(target);
        } catch (NumberFormatException e) {
            parseError("nullP");
        }
        if (output < 0) {
            parseError("nullP");
        }
        return output;
    }

    // This function will scan entire line, then compress multiple empty space into single empty space.
    private String blankCanner(String target) {
        int trace = 0;
        int endOfLine = target.length();
        char[] tempChars = target.toCharArray();

        // Goes through entire string as character array, and replaces any extra empty spaces into '#'.
        while (trace < endOfLine-1) {
            if (tempChars[trace] == ' ' || tempChars[trace] == '#') {
                if (tempChars[trace+1] == ' ') {
                    tempChars[trace+1] = '#';
                    // Temporary Ad-hoc, As instructed, triggering canning sequence will cause error instead.
                    parseError("inFault");
                }
            }
            trace = trace + 1;
        }

        // Create new text strings using processed character arrays, then replace all placeholder '#' into null.
        String newTarget = new String(tempChars);
        newTarget = newTarget.replace("#", "");

        return newTarget;
    }

    // Catch no name error here by ordering of errors and if statements checking setName status when later error triggers.
    private void parseError(String erCode) {

                systemStatePrinter();
              if (erCode.contentEquals("fpa"))
              {
                  System.out.println("Partial Assignment Error");
                  System.out.println("Assignment requested is impossible due to duplicated machine or task assignment.");
                  SplashOutput.printError(2);
              }
              if (erCode.contentEquals("intCrash"))
              {
                  System.out.println("Machine Penalty Error");
                  System.out.println("Provided data either falls short or exceeds required data input.");
                  SplashOutput.printError(4);
              }
              else if (erCode.contentEquals("collision"))
              {
                  System.out.println("No valid solution possible!");
                  System.out.println("Forced assignment collides with forbidden machine!");
                  SplashOutput.printError(7);
              }
              else if (erCode.contentEquals("nullMT"))
              {
                  System.out.println("invalid machine/task");
                  System.out.println("Input tried to assign task to the non-existent machine or non-existent Task");
                  SplashOutput.printError(3);
              }
              else if (erCode.contentEquals("nullT"))
              {
                System.out.println("invalid task");
                  System.out.println("Input tried to assign task to non-existent Task");
                  SplashOutput.printError(5);
              }
              else if (erCode.contentEquals("nullP"))
              {
                  System.out.println("Invalid Penalty Error");
                  System.out.println("Input is neither natural number or 0.");
                  SplashOutput.printError(6);
              }
              else if (erCode.contentEquals("inFault"))
              {
                  System.out.println("Error while parsing input file");
                  SplashOutput.printError(1);
              }
              else
              {
                  System.out.println("Unknown error has occured during input parsing. Aborting.");
              }

              System.exit(0);
          }

    public void systemStatePrinter() {
        if (setName == true) {
            System.out.println("Set Name: true");
        }
        if (setFPA == true) {
            System.out.println("Set FPA: true");
        }
        if (setFM == true) {
            System.out.println("Set FM: true");
        }
        if (setTNT == true) {
            System.out.println("Set TNT: true");
        }
        if (setMP == true) {
            System.out.println("Set MP: true");
        }
        if (setTNP == true) {
            System.out.println("Set TNP: true");
        }
        if (setCollision == true) {
            System.out.println("Set Collision: true");
        }
        System.out.println("Cycle: " + execCycle);
        printMe();
    }
}