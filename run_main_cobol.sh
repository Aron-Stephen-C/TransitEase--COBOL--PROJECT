if [ -z "$1" ]; then
  echo "Error: Please provide the path to the COBOL file."
  exit 1
fi

# Get the file path and name
filePath=$1
fileDirname=$(dirname "$filePath")
fileBasename=$(basename "$filePath")

# Determine the file extension and strip it
fileExtension="${fileBasename##*.}"
fileBasenameWithoutExt="${fileBasename%.*}"

# Check if the file has a supported COBOL extension
if [[ "$fileExtension" != "cob" && "$fileExtension" != "cbl" ]]; then
  echo "Error: Unsupported file extension. Please provide a .cob or .cbl file."
  exit 1
fi

# Check if the file exists
if [ ! -f "$filePath" ]; then
  echo "Error: File not found at $filePath."
  exit 1
fi

# Compile the COBOL program
outputFile="$fileDirname/$fileBasenameWithoutExt"
cobc -x "$filePath" -o "$outputFile"
if [ $? -ne 0 ]; then
  echo "Error: Compilation failed. Please check your COBOL source code."
  exit 1
fi

# Ensure the compiled file is executable
if [ ! -x "$outputFile" ]; then
  chmod +x "$outputFile"
fi

# Run the compiled executable
echo "Running the program..."
"$outputFile"
executionStatus=$?

# Check if the program ran successfully
if [ $executionStatus -ne 0 ]; then
  echo "Error: The program encountered an error during execution."
  exit $executionStatus
fi

