With GameMath; use GameMath;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Logger;           use Test_Logger;


package body Tests.Vector_Math is

Test_Vec1 : Vec2 := New_Vec2(1.0, 2.0);
Test_Vec2 : Vec2 := New_Vec2(3.0, 4.0);
Test_Vec3 : Vec2 := New_Vec2(1.0, 2.0);

Added_Vectors : Vec2 := Test_Vec1 + Test_Vec2;
Sub_Vectors : Vec2 := Test_Vec1 - Test_Vec2;
Mult_Vector : Vec2 := Test_Vec1 * 2.0;
Div_Vector : Vec2 := Test_Vec1 / 2.0;

procedure Test is

   Indent : String := "    ";
   Separator : String := "===============================================";
begin

   Put_Line ("Vector Math Test");
   Put_Line (Separator);
   
   Log_Test("Vector Addition",
            Added_Vectors.X = 4.0 and Added_Vectors.Y = 6.0,
            "(4.0, 6.0)",
            Added_Vectors'Image);

   Log_Test("Vector Subtraction",
            Sub_Vectors.X = -2.0 and Sub_Vectors.Y = -2.0,
            "(-2.0, -2.0)",
            Sub_Vectors'Image);

   Log_Test("Vector Multiplication",
            Mult_Vector.X = 2.0 and Mult_Vector.Y = 4.0,
            "(2.0, 4.0)",
            Mult_Vector'Image);

   Log_Test("Vector Division",
            Div_Vector.X = 0.5 and Div_Vector.Y = 1.0,
            "(0.5, 1.0)",
            Div_Vector'Image); 

   Log_Test("Vector Equality",
            Test_Vec1 = Test_Vec3,
            "True",
            Boolean'Image(Test_Vec1 = Test_Vec3));   

   Put_Line (Separator);

end Test;
end Tests.Vector_Math;