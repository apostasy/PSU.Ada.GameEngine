
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Text;
with Window; use Window;
with Win32; use Win32;
with System;
with Graphics.Renderer; use Graphics.Renderer;
with Ada.Real_Time; use Ada.Real_Time;
with Graphics.Color; use Graphics.Color;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Event; use ECS.Event;
with ECS.Event_Manager; use ECS.Event_Manager;
with GameMath; use GameMath;
with ECS.System; use ECS.System;
with ECS.System.Movement; use ECS.System.Movement;
with ECS.System.Collision; use ECS.System.Collision;
with ECS.System.Render; use ECS.System.Render;
with ECS.System.User_Input; use ECS.System.User_Input;

with Input_Callbacks; use Input_Callbacks;

with ECS.Entity; use ECS.Entity;
with ECS.Component; use ECS.Component;
with Interfaces.C;

procedure ECS_Window_System_Integration_Test is
package IC renames Interfaces.C; use IC;
Width       : Integer                  := 640;
Height      : Integer                  := 360;
Title       : Unbounded_String         := To_Unbounded_String("Game Window");
GameWindow  : Window_Access;
Buffer      : Win32.Byte_Array_Access  := new Win32.Byte_Array(0 .. Width * Height * 4);
SkyBlue     : Color                    := (R => 135, G => 206, B => 236, A => 255);
Start_Time  : Time;
Stop_Time   : Time;
Elapsed_Time: Duration;

-- Entity Manager and Entities
Manager     : Manager_Access           := new Entity_Manager_T'(Entities => Entity_List.Empty_Vector, 
                                                   ToBeAdded => Entity_List.Empty_Vector);
Event_Mgr   : ECS.Event_Manager.Platform_Event_Handler_Access := new Platform_Event_Handler;
Player      : Entity_Access            := Manager.all.AddEntity("Playr");
E1          : Entity_Access            := Manager.all.AddEntity("E0001");
Score       : Entity_Access            := Manager.all.AddEntity ("Score");

-- Systems
Mover       : Mover_T      := (Width, Height);
Collision   : Collision_T  := (Width, Height);
Render      : Render_T     := (Width, Height, Buffer);
UserInput   : User_Input_T := (Player, Event_Mgr, False, True);
-- Player components
Transform_P : Component_Access := new Transform_T'(Position => (X => 400.0, Y => 300.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
T_P : Transform_T renames Transform_T(Transform_P.all);
Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 1.0);
AABB_P      : Component_Access := new AABB_T'(
   Left => T_P.Position.X, 
   Bottom => T_P.Position.Y, 
   Right => T_P.Position.X, 
   Top => T_P.Position.Y);
Collision_Params_P : Component_Access := new Collision_Params_T'(
   Collision_Enabled => True,
   Collision_Occurred => False,
   Destroy_On_Collision => True,
   Prev_Frame_Collision => False
);
C_P         : Collision_Params_T renames Collision_Params_T(Collision_Params_P.all);

Shape_P     : Component_Access := new Quad_T'(
   Width => 25.0,
   Height => 25.0,
   C => (R=> 255, G => 255, B => 0, A => 255)
);

   -- E1 components
Transform_E1: Component_Access := new Transform_T'(Position => (X => 300.0, Y => 100.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
T_E1        : Transform_T renames Transform_T(Transform_E1.all);
Rigidbody_E1: Component_Access := new Rigidbody_T'(Mass => 1.0);
AABB_E1     : Component_Access := new AABB_T;
Collision_Params_E1 : Component_Access := new Collision_Params_T'(
   Collision_Enabled => True,
   Collision_Occurred => False,
   Destroy_On_Collision => True,
   Prev_Frame_Collision => False
);
C_E1 : Collision_Params_T renames Collision_Params_T(Collision_Params_E1.all);

Shape_E1 : Component_Access := new Quad_T'(
   Width => 50.0,
   Height => 50.0,
   C => (R => 255, G => 0, B => 0, A => 255)
);

Transform_Score : Component_Access := new Transform_T'((5.0,300.0),(50.0,0.0),0.0);
Rigidbody_Score : Component_Access := new Rigidbody_T'(Mass=> 0.0);
AABB_Score      : Component_Access := new AABB_T;
Col_Score       : Component_Access := new Collision_Params_T'(False,False,False,False);
Shape_Score     : Component_Access := new Quad_T'(0.0,0.0,(0,0,0,0));
Text_Score      : Component_Access := new Text_T'(To_Unbounded_String ("TEST TEXT ENTITY"),(255,255,255,255));



begin

   Register_Key_Callback (16#20#, Space_Key'Access); -- Todo: Add all Key constants to win32.ads file
   Register_Key_Callback (16#57#, W_Key'Access);
   Register_Key_Callback (16#41#, A_Key'Access);
   Register_Key_Callback (16#53#, S_Key'Access);
   Register_Key_Callback (16#44#, D_Key'Access);
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
   Score.all.Add_Component(Transform_Score);
   Score.all.Add_Component(Rigidbody_Score);
   Score.all.Add_Component(AABB_Score);
   Score.all.Add_Component(Col_Score);
   Score.all.Add_Component(Shape_Score);
   Score.all.Add_Component(Text_Score);

   Start_Time := Clock;
   Stop_Time := Clock;

   GameWindow := New_Window(IC.int(Width),IC.int(Height),Title);
   Put_Line ("Start Engine");
 
   declare
      Message        : MSG_Access := new MSG;
      Has_Msg        : Boolean := True;
      Lp_Result      : LRESULT;
      FPS            : Integer;
   begin
      while Has_Msg loop
         Stop_Time := Clock;
         Elapsed_Time := To_Duration(Stop_Time - Start_Time);
         FPS := Integer(1.0 / Float(Elapsed_Time));
         Start_Time := Stop_Time;
         Lp_Result := Dispatch_Message (Message);
         Has_Msg := Get_Message (Message, System.Null_Address, 0, 0);
         Manager.all.update;
         Clear_Screen(Buffer.all,Graphics.Color.Blue, Width, Height);
         UserInput.Execute(Elapsed_Time, Manager);
         Collision.Execute(Elapsed_Time,Manager);
         Mover.Execute(Elapsed_Time, Manager);
         Render.Execute(Elapsed_Time, Manager);

         Draw_String(Buffer.all, 200,200, 0, 0, "HELLO TEAM", Graphics.Color.White, Width,Height);
         Draw_String(Buffer.all, 250, 250, 0, 0, "0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ", (255,19,240,0), Width, Height); 
         Draw_String(Buffer.all, 50, 50, 0, 0, "FPS:" & Integer'Image(FPS), Graphics.Color.Green, Width, Height);
         Draw_String(Buffer.all, 50, 65, 0, 0, "ENTITY COUNT:" & Manager.all.Entities.Length'Image, Graphics.Color.Green,Width,Height);
         Draw_Buffer(Buffer.all'Address);
         --delay 0.008; -- temporary measure to control frame rate
      end loop;
   end;
end ECS_Window_System_Integration_Test;

