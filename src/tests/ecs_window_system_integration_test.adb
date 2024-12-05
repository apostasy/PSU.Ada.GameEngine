
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Window; use Window;
with Win32; use Win32;
with System;
with Renderer; use Renderer;
with Ada.Real_Time; use Ada.Real_Time;
with ECS.Color; use ECS.Color;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Vec2; use ECS.Vec2;
with ECS.System; use ECS.System;
with ECS.Entity; use ECS.Entity;
with ECS.Component; use ECS.Component;

procedure ECS_Window_System_Integration_Test is
Title : Unbounded_String := To_Unbounded_String("Game Window");
GameWindow : Window_Access;
Buffer : Win32.Byte_Array_Access := new Win32.Byte_Array(0 .. 800 * 600 * 4);
SkyBlue : Color := (R => 135, G => 206, B => 236, A => 255);
Start_Time, Stop_Time : Time;
Elapsed_Time          : Time_Span;

-- Entity Manager and Entities
Manager : Manager_Access := new Entity_Manager_T'(Entities => Entity_List.Empty_Vector, 
                                                   ToBeAdded => Entity_List.Empty_Vector);

Player : Entity_Access := Manager.all.AddEntity("Playr");
E1 : Entity_Access := Manager.all.AddEntity("E0001");

-- Systems
Mover : Mover_T;
Collision : Collision_T;
Render : Render_T := (800,Buffer);
-- Player components
Transform_P : Component_Access := new Transform_T'(Position => (X => 50.0, Y => 500.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
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

Shape_P : Component_Access := new Shape_T'(
   Sides => 6,
   Radius => 25,
   C => (R=> 0, G => 255, B => 0, A => 255)
);

   -- E1 components
Transform_E1 : Component_Access := new Transform_T'(Position => (X => 600.0, Y => 100.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
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

Shape_E1 : Component_Access := new Shape_T'(
   Sides => 5,
   Radius => 25,
   C => (R => 255, G => 0, B => 0, A => 255)
);

Direction : Vec2 := T_E1.Position - T_P.Position;



begin
      -- Add entity components
   Player.all.Add_Component(Transform_P);
   Player.all.Add_Component(Rigidbody_P);
   Player.all.Add_Component(AABB_P);
   Player.all.Add_Component(Collision_Params_P);
   Player.all.Add_Component(Shape_P);
   E1.all.Add_Component(Transform_E1);
   E1.all.Add_Component(Rigidbody_E1);
   E1.all.Add_Component(AABB_E1);
   E1.all.Add_Component(Collision_Params_E1);
   E1.all.Add_Component(Shape_E1);

   Start_Time := Clock;
   Stop_Time := Clock;

   GameWindow := New_Window(800,600,Title);
   Put_Line ("Start Engine");

   Normalize(Direction);
   -- Set the speed
   Scale(Direction, 40.0);
   Put_Line("Player moving in direction " & Direction'Image);
   -- Assign new velocity vector to player
   -- Dummy comment
   T_P.Velocity.X := Direction.X;
   T_P.Velocity.Y := Direction.Y;

   declare
      Message        : MSG_Access := new MSG;
      Has_Msg        : Boolean := True;
      Lp_Result      : LRESULT;
   begin
      while Has_Msg loop
      Stop_Time := Clock;
      Elapsed_Time := Stop_Time - Start_Time;
      Start_Time := Stop_Time;
      Lp_Result := Dispatch_Message (Message);
      Has_Msg := Get_Message (Message, System.Null_Address, 0, 0);

               --  -- Process emitted events here
               --  Process_Events(Manager.all);
      Manager.all.update;
      Clear_Screen(Buffer.all, ECS.Color.Black, 800, 600);
      Mover.Execute(To_Duration(Elapsed_Time), Manager);
      Collision.Execute(To_Duration(Elapsed_Time),Manager);
      Render.Execute(To_Duration(Elapsed_Time), Manager);
      GameWindow.Draw_Buffer(Buffer.all'Address);
      end loop;
   end;
end ECS_Window_System_Integration_Test;