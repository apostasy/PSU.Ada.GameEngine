with ECS.System; use ECS.System;
with ECS.Entity; use ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with ECS.Vec2; use ECS.Vec2;
with Test_Logger; use Test_Logger;
with Ada.Text_IO; use Ada.Text_IO;

procedure ECS_Integration_Test_One is
   -- Entity Manager and Entities
   Manager : Manager_Access := new Entity_Manager_T'(Entities => Entity_List.Empty_Vector, 
                                                   ToBeAdded => Entity_List.Empty_Vector);

   Player : Entity_Access := Manager.all.AddEntity("Playr");
   E1 : Entity_Access := Manager.all.AddEntity("E0001");

   -- Systems
   Mover : Mover_T;
   Collision : Collision_T;

   -- Player components
   Transform_P : Component_Access := new Transform_T'(Position => (X => 50.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_P : Transform_T renames Transform_T(Transform_P.all);
   Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_P      : Component_Access := new AABB_T'(
      Left => T_P.Position.X, 
      Bottom => T_P.Position.Y + 5.0, 
      Right => T_P.Position.X + 5.0, 
      Top => T_P.Position.Y);
   Collision_Params_P : Component_Access := new Collision_Params_T'(
      Collision_Enabled => True,
      Collision_Occurred => False,
      Destroy_On_Collision => True,
      Left_Bound => False,
      Right_Bound => False,
      Top_Bound => False,
      Bottom_Bound => False
   );
   C_P : Collision_Params_T renames Collision_Params_T(Collision_Params_P.all);

   -- E1 components
   Transform_E1 : Component_Access := new Transform_T'(Position => (X => 100.0, Y => 20.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_E1 : Transform_T renames Transform_T(Transform_E1.all);
   Rigidbody_E1 : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_E1      : Component_Access := new AABB_T'(
      Left => T_E1.Position.X, 
      Bottom => T_E1.Position.Y + 5.0, 
      Right => T_E1.Position.X + 5.0, 
      Top => T_E1.Position.Y);
   Collision_Params_E1 : Component_Access := new Collision_Params_T'(
      Collision_Enabled => True,
      Collision_Occurred => False,
      Destroy_On_Collision => True,
      Left_Bound => False,
      Right_Bound => False,
      Top_Bound => False,
      Bottom_Bound => False
   );
   C_E1 : Collision_Params_T renames Collision_Params_T(Collision_Params_E1.all);

   Direction : Vec2 := T_E1.Position - T_P.Position;
begin
   Put_Line("Start integration test of Entity, Components, Mover and Collision systems, Entity Manager, and Vec2");
   New_Line;
   -- Add entity components
   Player.all.Add_Component(Transform_P);
   Player.all.Add_Component(Rigidbody_P);
   Player.all.Add_Component(AABB_P);
   Player.all.Add_Component(Collision_Params_P);
   E1.all.Add_Component(Transform_E1);
   E1.all.Add_Component(Rigidbody_E1);
   E1.all.Add_Component(AABB_E1);
   E1.all.Add_Component(Collision_Params_E1);


   Manager.all.Update; 

   -- Get the unit vector for the direction to move Player
   Normalize(Direction);
   -- Set the speed
   Scale(Direction, 5.0);
   Put_Line("Player moving in direction " & Direction'Image);
   -- Assign new velocity vector to player
   T_P.Velocity.X := Direction.X;
   T_P.Velocity.Y := Direction.Y;
-- Move player to e1 until they collide
while not (C_P.Collision_Occurred and C_E1.Collision_Occurred) loop
   Mover.Execute(1.0,Manager);
   Collision.Execute(1.0,Manager);
   Put_Line("Player position: " & T_P.Position'Image & " E1 position: " & T_E1.Position'Image);
end loop;
Log_Test("Entity collision occurred", True, "","");
Log_Test("Entities in list, waiting destruction", Natural(Manager.Entities.Length) = 2, "2", Natural'Image(Natural(Manager.Entities.Length)));
Manager.all.update;
Log_Test("Destroyed entities removed from list", Natural(Manager.Entities.Length) = 0, "0", Natural'Image(Natural(Manager.Entities.Length)));

end ECS_Integration_Test_One;