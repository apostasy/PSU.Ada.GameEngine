with Bricks;
package body Bricklayer is 

   Level_Width : constant Integer := 12;
   StartX      : constant Float := 16.0;
   StartY      : constant Float := 30.0;
   Block_Width : constant Float := 16.0;
   Block_Height : constant Float := 8.0; 

   procedure Lay_Bricks(
      Manager : access ECS.Entity_Manager.Entity_Manager_T'Class;
      Texture : Texture_Access; 
      lvl : Level
   ) is
   xPos : Float;
   yPos : Float;
   begin
      xPos := StartX;
      yPos := StartY;
      for I in 0 .. lvl'Length - 1 loop
         exit when lvl(I) = -1;
         if lvl(I) >= 1 then
            Bricks.Add_Brick (Manager, (xPos,yPos), lvl(I), Texture);
         end if;
         xPos := xPos + Block_Width;
         if (I+1) mod Level_Width = 0 then
            yPos := yPos + Block_Height;
            xPos := StartX;
         end if;
      end loop;
   end Lay_Bricks;
end Bricklayer;