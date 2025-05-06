with ECS.Entity_Manager;   use ECS.Entity_Manager;
with ECS.Entity;           use ECS.Entity;
with GameMath;             use GameMath;
package Colliders is
   procedure Add_Wall(
      Manager        : access Entity_Manager_T'Class; 
      Width,Height   : Float;
      Position       : Vec2;
      Id             : Id_T
      );
end Colliders;