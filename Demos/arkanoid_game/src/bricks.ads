with ECS.Component; use ECS.Component;
with GameMath; use GameMath;
with ECS.Entity_Manager;
package Bricks is


procedure Add_Brick(
   Manager : access ECS.Entity_Manager.Entity_Manager_T'Class; 
   Position : Vec2; 
   Brick_Type : Integer;  
   Texture : Texture_Access
   );


end Bricks;