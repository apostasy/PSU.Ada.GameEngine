# Read the programs in the bin directory
# and run them with the test files in the test directory

# Set the path to the bin directory
BIN_DIR=bin
# Set the path to the test directory
TEST_DIR=test_output

# Get the list of programs in the bin directory that contain the term test
PROGRAMS=$(ls $BIN_DIR | grep test)

# For each program in the bin directory
for PROGRAM in $PROGRAMS

do
    echo "Running tests for $PROGRAM"
    ./$BIN_DIR/$PROGRAM
done