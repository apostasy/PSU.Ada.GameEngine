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


   function Center_Text (Text : String; Width : Natural) return String is
      Total_Dashes : Natural := Width - Text'Length;
      Left_Dashes : Natural := Total_Dashes / 2;
      Right_Dashes : Natural := Total_Dashes - Left_Dashes;
   begin
      if Width < Text'Length then
         return Text;
      else
         return (1 .. Left_Dashes => '-') & Text & (1 .. Right_Dashes => '-');
      end if;
   end Center_Text;

end Test_Logger;