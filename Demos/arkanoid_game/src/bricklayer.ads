with ECS.Component; use ECS.Component;
with ECS.Entity_Manager;
package Bricklayer is
   type Level is array (0 .. 255) of Integer;
   procedure Lay_Bricks(
      Manager : access ECS.Entity_Manager.Entity_Manager_T'Class;
      Texture : Texture_Access; 
      lvl : Level
      );
end Bricklayer;