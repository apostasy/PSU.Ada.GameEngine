with Ada.Text_IO; use Ada.Text_IO;

package body Test_Logger is
   -- ANSI color codes
   Green : constant String := ASCII.ESC & "[32m"; -- Green text
   Red   : constant String := ASCII.ESC & "[31m"; -- Red text
   Reset : constant String := ASCII.ESC & "[0m";  -- Reset to default


   procedure Log_Test
      (Test_Name : String;
       Passed    : Boolean;
       Expected  : String;
       Actual    : String) 
   is
   begin
      if Passed then
         Put_Line(Test_Name & ":" & Green & " PASSED" & Reset);
      else
         Put_Line(Test_Name & ":" & Red & " FAILED" & Reset);
         Put_Line("   Expected: " & Expected);
         Put_Line("   Actual  : " & Actual);
      end if;
   end Log_Test;
end Test_Logger;
