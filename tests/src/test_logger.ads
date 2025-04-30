package Test_Logger is

   procedure Log_Test
      (Test_Name : String;
       Passed    : Boolean;
       Expected  : String;
       Actual    : String);

   function Center_Text (Text : String; Width : Natural) return String;
end Test_Logger;