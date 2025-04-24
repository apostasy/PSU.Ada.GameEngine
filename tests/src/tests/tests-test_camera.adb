with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Renderer; use Graphics.Renderer;
with Graphics.Color; use Graphics.Color;
with Ada.Real_Time; use Ada.Real_Time;
with System;
with Window; use Window;
with Win32; use Win32;

-- Game Engine ECS modules
with ECS.Component;           use ECS.Component;
with ECS.Entity;              use ECS.Entity;
with ECS.Entity_Manager;      use ECS.Entity_Manager;
with ECS.Event;               use ECS.Event;
with ECS.Event_Manager;       use ECS.Event_Manager;
with ECS.System;              use ECS.System;
with ECS.System.Animation;    use ECS.System.Animation;
with ECS.System.Collision;    use ECS.System.Collision;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Render;       use ECS.System.Render;
with ECS.System.User_Input;   use ECS.System.User_Input;
with GameMath;                use GameMath;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Renderer;       use Graphics.Renderer;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;

-- User defined modules
with Input_Callbacks; use Input_Callbacks;
with ECS.System.Enemy_Spawner;use ECS.System.Enemy_Spawner;

package body Tests.Test_Camera is

   Width       : Integer                  := 540;
   Height      : Integer                  := 360;

   Title       : Unbounded_String         := To_Unbounded_String("Game Window");
   GameWindow  : Window_Access;
   Buffer      : Win32.Byte_Array_Access  := new Win32.Byte_Array(0 .. Width * Height * 4);
   SkyBlue     : Color                    := (R => 135, G => 206, B => 236, A => 255);
   Start_Time  : Time;
   Stop_Time   : Time;
   Elapsed_Time: Duration;

   -- Entity Manager and Entities
   Manager               : Manager_Access           := new Entity_Manager_T' (Entity_List.Empty_Vector,Entity_List.Empty_Vector);
   Event_Mgr             : Platform_Event_Handler_Access := new Platform_Event_Handler;
   Player                : Entity_Access            := Manager.all.AddEntity ("Playr");

   -- Systems
   Render             : Render_T         := (Width, Height, Buffer);
   Animation          : Animation_T;
   EnemySpawner       : Enemy_Spawn_T;
   Mover              : Mover_T          := (Width, Height);
   UserInput          : User_Input_T     := (Player, Event_Mgr, False, True);
   Collision          : Collision_T      := (Width, Height);

   -- player components
   Transform_P : Component_Access := new Transform_T'(Position => (X => 50.0, Y => 150.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
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
      Wall_Collision => False
   );
   C_P         : Collision_Params_T renames Collision_Params_T(Collision_Params_P.all);
   Shape_P     : Component_Access := new Quad_T'(
      Width => 36.0,
      Height => 53.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );

   Walk_P : Single_Animation_Access := new Single_Animation_T'(80,0,0.1,0.0,14,27,14,27,0,8);
   Idle_P : Single_Animation_Access := new Single_Animation_T'(80,0,0.1,0.0,21,27,21,27,0,6);

   Anim_Comp : constant Animation_Component_T := (
      Animations => (others => null), 
      Textures => (others => null),
      Current => Idle
   );

   Animations_P : Component_Access := new Animation_Component_T'(Anim_Comp);

   Walk_Texture_P : Texture_Access;
   Idle_Texture_P : Texture_Access;

   player_walk : constant String := "..\Data\Walk-S.qoi";
   player_idle : constant String := "..\Data\Idle-S.qoi";

   level_1_background : constant String := "..\Data\Fighter Background 1.qoi";
   level_2_background : constant String := "..\Data\Fighter Background 2.qoi";

   Level_1                 : Entity_Access            := Manager.all.AddEntity ("Level 1");
   Level_1_Transform         : Component_Access := new Transform_T'(Position => (X => 0.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   Level_1_T                 : Transform_T renames Transform_T(Level_1_Transform.all);
   Level_1_Shape             : Component_Access := new Quad_T'(
      Width => 4500.0,
      Height => 300.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );

   Level_2                 : Entity_Access            := Manager.all.AddEntity ("Level 2");
   Level_2_Transform         : Component_Access := new Transform_T'(Position => (X => 0.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   Level_2_T                 : Transform_T renames Transform_T(Level_2_Transform.all);
   Level_2_Shape             : Component_Access := new Quad_T'(
      Width => 4500.0,
      Height => 300.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );


   Camera_P : Component_Access := new Camera_Component_T'(
      Position => (X => 0.0, Y => 0.0),
      Width => Width,
      Height => Height,
      Zoom => 1.0,
      Buffer => Buffer
   );
   Camera_T : Camera_Component_T renames Camera_Component_T(Camera_P.all);

procedure Test is

 begin

   Put_Line("Test Camera");

   -- Set animations
   Anims_P : Animation_Component_T renames Animation_Component_T(Animations_P.all);
   Anims_P.Animations(Walk) := Walk_P;
   Anims_P.Animations(Idle) := Idle_P;

   -- Define input keys
   Register_Key_Callback (16#20#, Space_Key'Access); -- Todo: Add all Key constants to win32.ads file
   Register_Key_Callback (16#57#, W_Key'Access);
   Register_Key_Callback (16#41#, A_Key'Access);
   Register_Key_Callback (16#53#, S_Key'Access);
   Register_Key_Callback (16#44#, D_Key'Access);

   -- Add entity components
   Player.all.Add_Component (Transform_P);
   Player.all.Add_Component (Rigidbody_P);
   Player.all.Add_Component (AABB_P);
   Player.all.Add_Component (Collision_Params_P);
   Player.all.Add_Component (Shape_P);
   Player.all.Add_Component (Animations_P);

   Player.all.Add_Component (Camera_P);

   Start_Time := Clock;
   Stop_Time := Clock;

   GameWindow := New_Window(IC.int(Width),IC.int(Height),Title);

   declare
      Message        : MSG_Access := new MSG;
      Has_Msg        : Boolean := True;
      Lp_Result      : LRESULT;
      FPS            : Integer;
      Texture_Image  : QOI_Image_Data;
   begin

      Texture_Image        := Load_QOI(player_walk);
      Walk_Texture_P       := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
      Texture_Image        := Load_QOI(player_idle);
      Idle_Texture_P       := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);   
      
      Anims_P.Textures(Walk) := Walk_Texture_P;
      Anims_P.Textures(Idle) := Idle_Texture_P;

      while Has_Msg loop      

         Clear_Screen(Buffer.all,Graphics.Color.Blue, Width, Height);
         
         Stop_Time := Clock;
         Elapsed_Time := To_Duration(Stop_Time - Start_Time);
         FPS := Integer(1.0 / Float(Elapsed_Time));
         Start_Time := Stop_Time;
         Lp_Result := Dispatch_Message (Message);
         Has_Msg := Get_Message (Message, System.Null_Address, 0, 0);         

         Manager.all.Update;
         UserInput.Execute (Elapsed_Time, Manager);
         --  EnemySpawner.Execute (Elapsed_Time, Manager);
         Collision.Execute (Elapsed_Time, Manager);
         Mover.Execute (Elapsed_Time, Manager);
         Render.Execute (Elapsed_Time, Manager);
         Animation.Execute(Elapsed_Time, Manager);

         Camera_T.Position.X := T_P.Position.X - Float(Width / 4);

         Draw_Buffer (Buffer.all'Address);

      end loop;

   end;

end Test;

end Tests.Test_Camera;