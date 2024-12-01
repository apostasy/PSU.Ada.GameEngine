with Ada.Strings.Text_Buffers;
package ECS.Vec2 is
   type Vec2 is record
      X : Float := 0.0;
      Y : Float := 0.0;
   end record with Put_Image => My_Put_Image;

   procedure My_Put_Image
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : Vec2);

   function New_Vec2 (X_In, Y_In : Float) return Vec2;

   function "=" (Lhs, Rhs : Vec2) return Boolean;
   function "+" (Lhs, Rhs : Vec2) return Vec2;
   function "-" (Lhs, Rhs : Vec2) return Vec2;
   function "*" (V : Vec2; Val : Float) return Vec2;
   function "/" (V : Vec2; Val : Float) return Vec2;

   procedure Add (V : in out Vec2; Rhs : Vec2);
   procedure Scale (V : in out Vec2; S : Float);
   procedure Normalize (V : in out Vec2);
   procedure Rotate (V : in out Vec2; Deg : Float);

   function Dot (Lhs, Rhs : Vec2) return Float;
   function Dist_Squared (Lhs, Rhs : Vec2) return Float;
   function Nearly_Equals(A, B : Float; Epsilon : Float := 1.0E-6) return Boolean;
end ECS.Vec2;