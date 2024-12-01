with Ada.Text_IO; use Ada.Text_IO;
with Test_Logger; use Test_Logger;
with ECS.Vec2;

procedure ECS_Vec2_Tests is
   use ECS.Vec2;

   procedure Test_Initialization is
      V : Vec2 := New_Vec2(1.0, 2.0);
   begin
      Log_Test("Initialization X", V.X = 1.0, "1.0", Float'Image(V.X));
      Log_Test("Initialization Y", V.Y = 2.0, "2.0", Float'Image(V.Y));
   end Test_Initialization;

   procedure Test_Equality is
      V1, V2 : Vec2 := New_Vec2(1.0, 2.0);
      V3 : Vec2 := New_Vec2(3.0, 4.0);
   begin
      Log_Test("Equality Test 1", V1 = V2, "True", Boolean'Image(V1 = V2));
      Log_Test("Equality Test 2", not(V1 = V3), "False", Boolean'Image(V1 = V3));
   end Test_Equality;

   procedure Test_Arithmetic is
      V1 : Vec2 := New_Vec2(1.0, 2.0);
      V3 : Vec2 := New_Vec2(3.0, 4.0);
      Sum : Vec2 := V1 + V3;
      Diff : Vec2 := V3 - V1;
      Scaled : Vec2 := V1 * 2.0;
      Divided : Vec2 := V3 / 2.0;
   begin
      Log_Test("Addition", Sum = New_Vec2(4.0, 6.0), "(4.0, 6.0)", "(" & Float'Image(Sum.X) & ", " & Float'Image(Sum.Y) & ")");
      Log_Test("Subtraction", Diff = New_Vec2(2.0, 2.0), "(2.0, 2.0)", "(" & Float'Image(Diff.X) & ", " & Float'Image(Diff.Y) & ")");
      Log_Test("Scaling", Scaled = New_Vec2(2.0, 4.0), "(2.0, 4.0)", "(" & Float'Image(Scaled.X) & ", " & Float'Image(Scaled.Y) & ")");
      Log_Test("Division", Divided = New_Vec2(1.5, 2.0), "(1.5, 2.0)", "(" & Float'Image(Divided.X) & ", " & Float'Image(Divided.Y) & ")");
   end Test_Arithmetic;

   procedure Test_Procedures is
      V : Vec2 := New_Vec2(1.0, 2.0);
      Addend : Vec2 := New_Vec2(2.0, 3.0);
      Scale_Factor : Float := 2.0;
   begin
      Add(V, Addend);
      Log_Test("Add Procedure", V = New_Vec2(3.0, 5.0), "(3.0, 5.0)", "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
      
      Scale(V, Scale_Factor);
      Log_Test("Scale Procedure", V = New_Vec2(6.0, 10.0), "(6.0, 10.0)", "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
   end Test_Procedures;

   procedure Test_Normalize is
      V : Vec2 := New_Vec2(3.0, 4.0);
   begin
      Normalize(V);
      Log_Test("Normalize Procedure", V.X = 3.0 / 5.0 and V.Y = 4.0 / 5.0, 
               "(0.6, 0.8)", 
               "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
   end Test_Normalize;

   procedure Test_Rotate is
      V : Vec2 := New_Vec2(1.0, 0.0);
   begin
      Rotate(V, 90.0);
      Log_Test("Rotate Procedure", Nearly_Equals(V.X, 0.0) and Nearly_Equals(V.Y,1.0), 
               "(0.0, 1.0)", 
               "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
   end Test_Rotate;

   procedure Test_Dot is
      V1 : Vec2 := New_Vec2(1.0, 2.0);
      V2 : Vec2 := New_Vec2(3.0, 4.0);
      Result : Float := Dot(V1, V2);
   begin
      Log_Test("Dot Product", Result = 11.0, "11.0", Float'Image(Result));
   end Test_Dot;

   procedure Test_Dist_Squared is
      V1 : Vec2 := New_Vec2(1.0, 2.0);
      V2 : Vec2 := New_Vec2(4.0, 6.0);
      Result : Float := Dist_Squared(V1, V2);
   begin
      Log_Test("Dist_Squared", Result = 25.0, "25.0", Float'Image(Result));
   end Test_Dist_Squared;

begin
   Put_Line("Running ECS.Vec2 Tests...");
   
   Test_Initialization;
   Test_Equality;
   Test_Arithmetic;
   Test_Procedures;
   Test_Normalize;
   Test_Rotate;
   Test_Dot;
   Test_Dist_Squared;

end ECS_Vec2_Tests;
