with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with Ada.Strings.Text_Buffers;
package body ECS.Vec2 is

   procedure My_Put_Image
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : Vec2)
   is
      X     : constant String := Value.X'Image;
      Y     : constant String := Value.Y'Image;
      Result: constant String := "( " & X & ", " & Y & " )";
   begin
      Output.Put(Result);
   end My_Put_Image;
   
   
   function New_Vec2 (X_In, Y_In : Float) return Vec2 is
   begin
      return (X => X_In, Y => Y_In);
   end New_Vec2;

   -- Compare two vectors

   function "=" (Lhs, Rhs : Vec2) return Boolean is
   begin
      return (Lhs.X = Rhs.X and Lhs.Y = Rhs.Y);
   end "=";

   -- Add two vectors and return a new vector
   function "+" (Lhs, Rhs : Vec2) return Vec2 is
   begin
      return (X => Lhs.X + Rhs.X, Y => Lhs.Y + Rhs.Y);
   end "+";

   -- Subtract two vectors and return a new vector
   function "-" (Lhs, Rhs : Vec2) return Vec2 is
   begin
      return (X => Lhs.X - Rhs.X, Y => Lhs.Y - Rhs.Y);
   end "-";

   -- Multiply two vectors and return a new vector
   function "*" (V : Vec2; Val : Float) return Vec2 is
   begin
      return (X => V.X * Val, Y => V.Y * Val);
   end "*";

   -- Subtract two vectors and return a new vector
   function "/" (V : Vec2; Val : Float) return Vec2 is
   begin
      return (X => V.X / Val, Y => V.Y / Val);
   end "/";

   -- Add in place 
   procedure Add (V : in out Vec2; Rhs : Vec2) is
   begin
      V := V + Rhs;
   end Add;

   -- Scale in place
   procedure Scale (V : in out Vec2; S : Float) is
   begin
      V := V * S;
   end Scale;

   procedure Normalize (V : in out Vec2) is
      Magnitude : Float := Sqrt(V.X * V.X + V.Y * V.Y);
   begin
      V := V / Magnitude;
   end Normalize;

   procedure Rotate (V : in out Vec2; Deg : Float) is
      Radians : constant Float := Deg * Ada.Numerics.Pi / 180.0;
      Cos_Val : constant Float := Cos(Radians);
      Sin_Val : constant Float := Sin(Radians);
      New_X   : Float := V.X * Cos_Val - V.Y * Sin_Val;
      New_Y   : Float := V.X * Sin_Val + V.Y * Cos_Val;
   begin
      V.X := New_X;
      V.Y := New_Y;
   end Rotate;

   -- returns the dot product of two vectors
   function Dot (Lhs, Rhs : Vec2) return Float is
   begin
      return (Lhs.X * Rhs.X + Lhs.Y * Rhs.Y);
   end Dot;

   -- This is a short hand way of applying the distance formula without sqrt 
   function Dist_Squared (Lhs, Rhs : Vec2) return Float is
   begin
      return (Lhs.X - Rhs.X) ** 2 + (Lhs.Y - Rhs.Y) ** 2;
   end Dist_Squared;

   -- helper function to compare floats 
   function Nearly_Equals(A, B : Float; Epsilon : Float := 1.0E-6) return Boolean is
   begin
      return abs(A - B) <= Epsilon;
   end Nearly_Equals;
end ECS.Vec2;