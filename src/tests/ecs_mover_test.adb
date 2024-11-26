with Ada.Text_IO; use Ada.Text_IO;
with ecs.entity; use ecs.entity;
with ecs.component; use ecs.component;
with ecs.entity_manager; use ecs.entity_manager;
with ecs.system; use ecs.system;

  procedure ECS_Mover_Test is

  Manager : Entity_Manager_T;
  Player : Entity_Access := Manager.AddEntity("Playr");
  Enemy : Entity_Access := Manager.AddEntity("Enemy");
  Mover : Mover_T;

  Transform : Component_Access := new Transform_T'(X => 1.0, Y => 2.0, Rotation => 0.0);
  Rigidbody : Component_Access := new Rigidbody_T'(Mass => 1.0);

  begin
  Put_Line("Running Tests");

  Player.all.Add_Component(Transform);
  Player.all.Add_Component(Rigidbody);

  Manager.Update;  
    -- 1/60 of a second after program launch
  Execute ( Mover, 1.0/60.0, Player);

   declare
      Transform_Component : Transform_T := Transform_T(Player.Get_Component(Transform_T'Tag).all);
      Expected_Y : Float := 2.0 + Float(1.0/60.0) * (-9.8);
   begin
      Put_Line("Transform_Component.Y is: " & Float'Image(Transform_Component.Y) & ". Expected_Y: " & Float'Image(Expected_Y));
      if Transform_Component.Y = Expected_Y then
         Put_Line("Mover Test Passed");
      else 
         Put_Line("Mover Test Failed");   
      end if;
   end;

  end ECS_Mover_Test;

  