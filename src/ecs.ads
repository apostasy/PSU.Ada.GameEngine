with GameMath; use GameMath;
package ECS is
   type MousePosition is record
      PreviousPos : Vec2;
      CurrentPos : Vec2;
   end record;


   -- Package level static variables
   MousePos       : MousePosition := ((0.0,0.0),(0.0,0.0));
   WindowWidth    : Integer := 0;
end ECS;