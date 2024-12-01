with ECS.System; use ECS.System;
with ECS.Entity; use ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with Test_Logger; use Test_Logger;
with Ada.Text_IO; use Ada.Text_IO;

procedure ECS_Systems_Tests is
   -- Initial ECS 

   -- Entity Manager and Entities
   Manager : Manager_Access := new Entity_Manager_T'(Entities => Entity_List.Empty_Vector, 
                                                   ToBeAdded => Entity_List.Empty_Vector);

   Player : Entity_Access := Manager.all.AddEntity("Playr");
   E1 : Entity_Access := Manager.all.AddEntity("E0001");

   -- Systems
   Mover : Mover_T;
   Collision : Collision_T;

   -- Player components
   Transform_P : Component_Access := new Transform_T'(Position => (X => 1.0, Y => 20.0), Velocity => (X => 2.0, Y => 0.0), Rotation => 0.0);
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
      Destroy_On_Collision => False,
      Left_Bound => False,
      Right_Bound => False,
      Top_Bound => False,
      Bottom_Bound => False
   );
   C_P : Collision_Params_T renames Collision_Params_T(Collision_Params_P.all);

   -- E1 components
   Transform_E1 : Component_Access := new Transform_T'(Position => (X => 5.0, Y => 20.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
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
      Destroy_On_Collision => False,
      Left_Bound => False,
      Right_Bound => False,
      Top_Bound => False,
      Bottom_Bound => False
   );
   C_E1 : Collision_Params_T renames Collision_Params_T(Collision_Params_E1.all);

   begin

      Player.all.Add_Component(Transform_P);
      Player.all.Add_Component(Rigidbody_P);
      Player.all.Add_Component(AABB_P);
      Player.all.Add_Component(Collision_Params_P);
      E1.all.Add_Component(Transform_E1);
      E1.all.Add_Component(Rigidbody_E1);
      E1.all.Add_Component(AABB_E1);
      E1.all.Add_Component(Collision_Params_E1);


      Manager.all.Update; 


      Put_Line("Running ECS.System tests...");
      New_Line;
      Put_Line("Testing Mover_T");
      Mover.Execute(1.0, Manager);
      New_Line;
      Log_Test(Player.all.Id & " Moved to correct X position", T_P.Position.X = 3.0, "3.0", Float'Image(T_P.Position.X));
      Log_Test(Player.all.Id & " Moved to correct Y position", T_P.Position.Y = 20.0, "20.0", Float'Image(T_P.Position.Y));
      New_Line;
      Put_Line("Testing Collision_T");
      New_Line;
      T_P.Velocity.X := -2.0;
      Mover.Execute(1.0, Manager); 
      -- Player = (1.0, 20.0), E1 = (5.0, 20.0) Both Entities size = 5x5
      Collision.Execute(1.0,Manager);
      Log_Test("Left collision between " & Player.all.Id & " & " & E1.all.Id, C_P.Collision_Occurred = True, "True", Boolean'Image(C_P.Collision_Occurred));
      T_P.Velocity.X := 9.0;
      Mover.Execute(1.0, Manager);
      -- Player = (10.0, 20.0), E1 = (5.0, 20.0)
      Collision.Execute(1.0,Manager);
      Log_Test("Right collision between " & Player.all.Id & " & " & E1.all.Id, C_P.Collision_Occurred = True, "True", Boolean'Image(C_P.Collision_Occurred));
      T_P.Velocity.X := -5.0;
      T_P.Velocity.Y := -5.0;
      Mover.Execute(1.0, Manager);
      -- Player = (5.0, 15.0), E1 = (5.0, 20.0)
      Collision.Execute(1.0,Manager);
      Log_Test("Top collision between " & Player.all.Id & " & " & E1.all.Id, C_P.Collision_Occurred = True, "True", Boolean'Image(C_P.Collision_Occurred));
      T_P.Velocity.Y := 10.0;
      Mover.Execute(1.0, Manager);
      -- Player = (5.0, 25.0), E1 = (5.0, 20.0)
      Collision.Execute(1.0,Manager);
      Log_Test("Bottom collision between " & Player.all.Id & " & " & E1.all.Id, C_P.Collision_Occurred = True, "True", Boolean'Image(C_P.Collision_Occurred));
      T_P.Velocity.X := 40.0;
      -- Player = (45.0, 25.0), E1 = (5.0, 20.0)
      Mover.Execute(1.0, Manager);
      Collision.Execute(1.0,Manager);
      Log_Test("No collision between " & Player.all.Id & " & " & E1.all.Id, C_P.Collision_Occurred = False, "False", Boolean'Image(C_P.Collision_Occurred) & " and " & Boolean'Image(C_E1.Collision_Occurred));

end ECS_Systems_Tests;
