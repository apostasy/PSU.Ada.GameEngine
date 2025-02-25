-- console helpers
with Ada.Text_IO; use Ada.Text_IO;

-- window imports
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Window;                use Window;
with Win32;                 use Win32;
with System;
with Ada.Real_Time;         use Ada.Real_Time;

-- game engine imports
with Graphics.Renderer;     use Graphics.Renderer;
with Graphics.Color;        use Graphics.Color;
with ecs.Entity_Manager;    use ecs.Entity_Manager;
with ecs.Event;             use ecs.Event;
with ecs.Event_Manager;     use ecs.Event_Manager;
with ecs.Vec2;              use ecs.Vec2;
with ecs.System;            use ecs.System;
with ecs.System.Movement;   use ecs.System.Movement;
with ecs.System.Collision;  use ecs.System.Collision;
with ecs.System.Render;     use ecs.System.Render;
with ecs.System.User_Input; use ecs.System.User_Input;
with ecs.entity;    use ecs.entity;
with ecs.Component; use ecs.Component;
with Input_Callbacks; use Input_Callbacks;

-- Ada interfacing
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces;
with Interfaces.C;

procedure Presentation_Demo_1 is

   package IC renames Interfaces.C;
   use IC;

   -- configure window
   Width                 : Integer                 := 800;
   Height                : Integer                 := 600;
   Title : Unbounded_String        := To_Unbounded_String ("Game Window");
   GameWindow            : Window_Access;
   Buffer                : Win32.Byte_Array_Access :=
     new Win32.Byte_Array (0 .. Width * Height * 4);
   SkyBlue               : Color := (R => 135, G => 206, B => 236, A => 255);
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Time_Span;

   -- Entity Manager and Entities
   Manager   : Manager_Access                                  :=
     new Entity_Manager_T'
       (Entities  => Entity_List.Empty_Vector,
        ToBeAdded => Entity_List.Empty_Vector);
   Event_Mgr : ecs.Event_Manager.Platform_Event_Handler_Access :=
     new Platform_Event_Handler;

   Player : Entity_Access := Manager.all.AddEntity ("Playr");   

   -- Systems
   Mover     : Mover_T     := (Width, Height);
   Collision : Collision_T := (Width, Height);
   Render    : Render_T    := (Width, Height, Buffer);
   UserInput          : User_Input_T     := (Player, Event_Mgr, False, True);

   -- Player components
   Transform_P        : Component_Access :=
     new Transform_T'
       (Position => (X => 400.0, Y => 300.0), Velocity => (X => 0.0, Y => 0.0),
        Rotation => 0.0);
   T_P                : Transform_T renames Transform_T (Transform_P.all);
   Rigidbody_P        : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_P             : Component_Access :=
     new AABB_T'
       (Left  => T_P.Position.X, Bottom => T_P.Position.Y + 5.0,
        Right => T_P.Position.X + 5.0, Top => T_P.Position.Y);
   Collision_Params_P : Component_Access :=
     new Collision_Params_T'
       (Collision_Enabled    => False, Collision_Occurred => False,
        Destroy_On_Collision => True, Wall_Collision => False);

   C_P :
     Collision_Params_T renames Collision_Params_T (Collision_Params_P.all);

   Shape_P : Component_Access :=
     new Quad_T'
       (Width => 50.0, Height => 50.0,
        C     => (R => 255, G => 255, B => 0, A => 255));

begin  

   -- Register Input Callbacks
   Register_Input_Callback (16#20#, Space_Key'Access);
   Register_Input_Callback (16#57#, W_Key'Access);
   Register_Input_Callback (16#41#, A_Key'Access);
   Register_Input_Callback (16#53#, S_Key'Access);
   Register_Input_Callback (16#44#, D_Key'Access);

   -- Add Player entity components
   Player.all.Add_Component (Transform_P);
   Player.all.Add_Component (Rigidbody_P);
   Player.all.Add_Component (AABB_P);
   Player.all.Add_Component (Collision_Params_P);
   Player.all.Add_Component (Shape_P);

   Start_Time := Clock;
   Stop_Time  := Clock;

   -- instantiate window
   GameWindow := New_Window (IC.int (Width), IC.int (Height), Title);
   Put_Line ("Start Engine");

   declare

      Message   : MSG_Access := new MSG;
      Has_Msg   : Boolean    := True;
      Lp_Result : LRESULT;

   begin
      Put_Line ("Hello, World!");

      -- main process loop
      while Has_Msg loop
         Stop_Time    := Clock;
         Elapsed_Time := Stop_Time - Start_Time;
         Start_Time   := Stop_Time;
         Lp_Result    := Dispatch_Message (Message);
         Has_Msg      := Get_Message (Message, System.Null_Address, 0, 0);
         -- Process emitted events here - for debug purposes
         Manager.all.Update;
         Clear_Screen (Buffer.all, Graphics.Color.Blue, Width, Height);
         UserInput.Execute (To_Duration (Elapsed_Time), Manager);
         Collision.Execute (To_Duration (Elapsed_Time), Manager);
         Mover.Execute (To_Duration (Elapsed_Time), Manager);
         Render.Execute (To_Duration (Elapsed_Time), Manager);
         Draw_Buffer (Buffer.all'Address);
      end loop;

   end;

end Presentation_Demo_1;
