with Ada.Text_IO; use Ada.Text_IO;
with ecs.entity; use ecs.entity;
with ecs.component; use ecs.component;
with ecs.entity_manager; use ecs.entity_manager;
with ecs.system; use ecs.system;
with ecs.vec2; use ecs.vec2;

procedure ECS_Tests is

   Manager : Manager_Access := new Entity_Manager_T'(Entities => Entity_List.Empty_Vector, 
                                                   ToBeAdded => Entity_List.Empty_Vector);
   Player : Entity_Access := Manager.all.AddEntity("Playr");
   E1 : Entity_Access := Manager.all.AddEntity("E0001");
   E2 : Entity_Access := Manager.all.AddEntity("E0002");
   E3 : Entity_Access := Manager.all.AddEntity("E0003");
   Mover : Mover_T;
   Collision : Collision_T;
   Transform_P : Component_Access := new Transform_T'(Position => (X => 1.0, Y => 2.0), Velocity => (X => 2.0, Y => 0.0), Rotation => 0.0);
   T_P : Transform_T renames Transform_T(Transform_P.all);
   Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_P      : Component_Access := new AABB_T'(
      Left => T_P.Position.X, 
      Bottom => T_P.Position.Y + 5.0, 
      Right => T_P.Position.X + 5.0, 
      Top => T_P.Position.Y);
   
   Transform_E1 : Component_Access := new Transform_T'(Position => (X => 7.0, Y => 2.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_E1 : Transform_T renames Transform_T(Transform_E1.all);
   Rigidbody_E1 : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_E1      : Component_Access := new AABB_T'(
      Left => T_E1.Position.X, 
      Bottom => T_E1.Position.Y + 5.0, 
      Right => T_E1.Position.X + 5.0, 
      Top => T_E1.Position.Y);

   Transform_E2 : Component_Access := new Transform_T'(Position => (X => 15.0, Y => 2.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_E2 : Transform_T renames Transform_T(Transform_E2.all);
   Rigidbody_E2 : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_E2      : Component_Access := new AABB_T'(
      Left => T_E2.Position.X, 
      Bottom => T_E2.Position.Y + 5.0, 
      Right => T_E2.Position.X + 5.0, 
      Top => T_E2.Position.Y);

   Transform_E3 : Component_Access := new Transform_T'(Position => (X => 21.0, Y => 2.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_E3 : Transform_T renames Transform_T(Transform_E3.all);
   Rigidbody_E3 : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_E3      : Component_Access := new AABB_T'(
      Left => T_E3.Position.X, 
      Bottom => T_E3.Position.Y + 5.0, 
      Right => T_E3.Position.X + 5.0, 
      Top => T_E3.Position.Y);
   begin

      Player.all.Add_Component(Transform_P);
      Player.all.Add_Component(Rigidbody_P);
      Player.all.Add_Component(AABB_P);
      E1.all.Add_Component(Transform_E1);
      E1.all.Add_Component(Rigidbody_E1);
      E1.all.Add_Component(AABB_E1);
      E2.all.Add_Component(Transform_E2);
      E2.all.Add_Component(Rigidbody_E2);
      E2.all.Add_Component(AABB_E2);
      E3.all.Add_Component(Transform_E3);
      E3.all.Add_Component(Rigidbody_E3);
      E3.all.Add_Component(AABB_E3);

      Manager.all.Update;  
        --1/60 of a second after program launch
      for I in 1 .. 20 loop
         Put_Line("Calling Mover");
         Execute (Mover,1.0/60.0, Manager);
         Execute (Collision,1.0/60.0, Manager);
      end loop;

end ECS_Tests;

  