package Test_Logger is

   procedure Log_Test
      (Test_Name : String;
       Passed    : Boolean;
       Expected  : String;
       Actual    : String);

end Test_Logger;
