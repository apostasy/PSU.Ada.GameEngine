-- Ada Libraries
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces.C;
with System;
-- Game Engine ECS modules
with ECS.Component;           use ECS.Component;
with ECS.Entity;              use ECS.Entity;
with ECS.Entity_Manager;      use ECS.Entity_Manager;
with ECS.Entity_Manager_List; use ECS.Entity_Manager_List;
with ECS.Event;               use ECS.Event;
with ECS.Event_Manager;       use ECS.Event_Manager;
with ECS.System;              use ECS.System;
with ECS.System.Animation;    use ECS.System.Animation;
with ECS.System.Collision;    use ECS.System.Collision;
with ECS.System.Enemy_Spawner;use ECS.System.Enemy_Spawner;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Render;       use ECS.System.Render;
with ECS.System.User_Input;   use ECS.System.User_Input;
with ECS.Vec2;                use ECS.Vec2;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Renderer;       use Graphics.Renderer;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
-- win32 interface
with Win32;                 use Win32;
with Window;                use Window;
-- User defined modules
with Input_Callbacks; use Input_Callbacks;

procedure ECS_Scene_Manager_Test is
   package IC renames Interfaces.C;
   use IC;
   Width                 : Integer                 := 640;
   Height                : Integer                 := 360;
   Title                 : Unbounded_String        := To_Unbounded_String ("Game Window");
   GameWindow            : Window_Access;
   Buffer                : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. Width * Height * 4);
   Start_Time, Stop_Time, Launch_Time : Time;
   Elapsed_Time          : Duration;
   -- Entity Manager and Entities

   Main_Scene_ID         : constant Unbounded_String := To_Unbounded_String ("Scene1");
   Next_Scene_ID         : constant Unbounded_String := To_Unbounded_String ("Scene2");

   Main_Scene            : Manager_Access           := new Entity_Manager_T' (Main_Scene_ID, Entity_List.Empty_Vector,Entity_List.Empty_Vector);
   Next_Scene            : Manager_Access           := new Entity_Manager_T' (Next_Scene_ID, Entity_List.Empty_Vector,Entity_List.Empty_Vector);

   Scene_Manager         : Manager_List_Access      := new Entity_Manager_List_T'(Entity_Manager_List.Empty_Vector, Main_Scene);

   Event_Mgr             : Platform_Event_Handler_Access := new Platform_Event_Handler;
   Player                : Entity_Access            := Main_Scene.all.AddEntity ("Playr");
   
   -- Systems
   Mover              : Mover_T          := (Width, Height);
   Collision          : Collision_T      := (Width, Height);
   Render             : Render_T         := (Width, Height, Buffer);
   UserInput          : User_Input_T     := (Player, Event_Mgr, False, True);
   EnemySpawner       : Enemy_Spawn_T;
   Animation          : Animation_T;
   -- Player components
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

   Animation_P : Component_Access := new Animation_Component_T'(80,0,0.1,0.0,14,27,14,27,0,8);
   Texture_P : Component_Access;

   bkgrd  : constant String := "Data\terrace_360.qoi";
   player_texture : constant String := "Data\Walk-S.qoi";
   warp_texture : constant String := "Data\testimage.qoi";

   Warp1                 : Entity_Access            := Main_Scene.all.AddEntity ("WarpZ");
   Warp2                 : Entity_Access            := Next_Scene.all.AddEntity ("WarpZ");
   -- Warp1 components
   Transform_W1 : Component_Access := new Transform_T'(Position => (X => Float(Width) / 4.0, Y => Float(Height) / 4.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_W1 : Transform_T renames Transform_T(Transform_W1.all);
   Rigidbody_W1 : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_W1      : Component_Access := new AABB_T'(
      Left => T_W1.Position.X, 
      Bottom => T_W1.Position.Y, 
      Right => T_W1.Position.X, 
      Top => T_W1.Position.Y);
   Collision_Params_W1 : Component_Access := new Collision_Params_T'(
      Collision_Enabled => True,
      Collision_Occurred => False,
      Destroy_On_Collision => false,
      Wall_Collision => False
   );
   C_W1         : Collision_Params_T renames Collision_Params_T(Collision_Params_W1.all);
   Shape_W1     : Component_Access := new Quad_T'(
      Width => 50.0,
      Height => 50.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );
   Texture_W1 : Component_Access;
   warp1_texture : constant String := "Data\red.qoi";

   -- Warp2 components
   Transform_W2 : Component_Access := new Transform_T'(Position => (X => Float(Width) * 0.75, Y => Float(Height) * 0.75), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_W2 : Transform_T renames Transform_T(Transform_W2.all);
   Rigidbody_W2 : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_W2      : Component_Access := new AABB_T'(
      Left => T_W2.Position.X, 
      Bottom => T_W2.Position.Y, 
      Right => T_W2.Position.X, 
      Top => T_W2.Position.Y);
   Collision_Params_W2 : Component_Access := new Collision_Params_T'(
      Collision_Enabled => True,
      Collision_Occurred => False,
      Destroy_On_Collision => False,
      Wall_Collision => False
   );
   C_W2         : Collision_Params_T renames Collision_Params_T(Collision_Params_W2.all);
   Shape_W2     : Component_Access := new Quad_T'(
      Width => 50.0,
      Height => 50.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );
   Texture_W2 : Component_Access;
   warp2_texture : constant String := "Data\green.qoi";

begin
   -- Define input keys
   Register_Input_Callback (16#20#, Space_Key'Access); -- Todo: Add all Key constants to win32.ads file
   Register_Input_Callback (16#57#, W_Key'Access);
   Register_Input_Callback (16#41#, A_Key'Access);
   Register_Input_Callback (16#53#, S_Key'Access);
   Register_Input_Callback (16#44#, D_Key'Access);
   -- Add entity components
   Player.all.Add_Component (Transform_P);
   Player.all.Add_Component (Rigidbody_P);
   Player.all.Add_Component (AABB_P);
   Player.all.Add_Component (Collision_Params_P);
   Player.all.Add_Component (Shape_P);
   Player.all.Add_Component (Animation_P);

   Next_Scene.all.AddEntity (Player);
   
   Warp1.all.Add_Component (Transform_W1);
   Warp1.all.Add_Component (Rigidbody_W1);
   Warp1.all.Add_Component (AABB_W1);
   Warp1.all.Add_Component (Collision_Params_W1);
   Warp1.all.Add_Component (Shape_W1);
   Texture_W1 := new Texture_T'(50,50,Load_QOI(warp1_texture).Data);
   Warp1.all.Add_Component (Texture_W1);

   Warp2.all.Add_Component (Transform_W2);
   Warp2.all.Add_Component (Rigidbody_W2);
   Warp2.all.Add_Component (AABB_W2);
   Warp2.all.Add_Component (Collision_Params_W2);
   Warp2.all.Add_Component (Shape_W2);
   Texture_W2 := new Texture_T'(50,50,Load_QOI(warp2_texture).Data);
   Warp2.all.Add_Component (Texture_W2);

   Scene_Manager.all.AddManager (Main_Scene);
   Scene_Manager.all.AddManager (Next_Scene);

   -- Used to calculate the frame time
   Start_Time := Clock;
   Stop_Time  := Clock;

   Launch_Time := Clock;
   Total_Time : Duration;

   GameWindow := New_Window (IC.int (Width), IC.int (Height), Title);
   declare
      Message           : MSG_Access := new MSG;
      Has_Msg           : Boolean    := True;
      Lp_Result         : LRESULT;
      Texture_Image     : QOI_Image_Data;
      Background_Image  : QOI_Image_Data;
   begin

      -- Load textures
      Background_Image    := Load_QOI (bkgrd);
      Texture_Image       := Load_QOI(player_texture);
      Texture_P           := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
      Player.all.Add_Component (Texture_P);
      -- Windows message loop (game loop)
      while Has_Msg loop
         Stop_Time    := Clock;
         Elapsed_Time := To_Duration(Stop_Time - Start_Time);

         Total_Time := To_Duration(Stop_Time - Launch_Time);         

         Start_Time   := Stop_Time;
         Lp_Result    := Dispatch_Message (Message);
         Has_Msg      := Get_Message (Message, System.Null_Address, 0, 0);
         Scene_Manager.all.Current_Manager.Update;
         --  Main_Scene.all.Update;
         -- Game system calls
         if not Started then
            Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height), 0,0, Width, Height,Natural(Background_Image.Desc.Width));
            Draw_String(Buffer.all,255,166,0,0,"PRESS ANY KEY",(255,255,255,255),Width,Height);
            Draw_Buffer (Buffer.all'Address);
            UserInput.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
         elsif GameOver then
            Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height), 0,0, Width, Height, Natural(Background_Image.Desc.Width));
            Draw_String(Buffer.all,280,166,0,0,"GAMEOVER",(255,255,255,255),Width,Height);
            Draw_Buffer (Buffer.all'Address);
         else
            UserInput.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
            EnemySpawner.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
            Collision.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
            Mover.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
            Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height), 0,0, Width, Height,Natural(Background_Image.Desc.Width));
            Draw_String(Buffer.all,1,7,0,0,"SCORE:" & Integer'Image(Score),(255,255,255,255),Width,Height);
            Render.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
            Animation.Execute(Elapsed_Time, Scene_Manager.all.Current_Manager);
            Draw_Buffer (Buffer.all'Address);

            declare
               WarpZ : Entity_Access := Scene_Manager.all.Current_Manager.all.GetEntity ("WarpZ");
               WarpZ_Collision_Params : Component_Access := WarpZ.all.Get_Component(Collision_Params_T'Tag);
               WarpZ_CP renames Collision_Params_T (WarpZ_Collision_Params.all);
            begin
               --  Put_line("Checking for WarpZ");
               --  Put_Line(WarpZ.all.Id);

               if WarpZ = null then
                  Put_Line("No Warp Zone");
                  goto Continue;
               end if;

               if Total_Time > 1.0 then
                  Launch_Time := Clock;
                  --  Put_Line("WarpZ Collided: " & Boolean'Image(WarpZ_CP.Collision_Occurred));
                  Put_Line("Current Scene: " & Scene_Manager.all.Current_Manager.ID'Image);                 

                  -- swap scenes manually
                  --  if Scene_Manager.all.Current_Manager.ID = Main_Scene_ID then
                  --     Scene_Manager.all.SetManager (Next_Scene_ID);
                  --  else
                  --     Scene_Manager.all.SetManager (Main_Scene_ID);
                  --  end if;
               end if;

               if WarpZ_CP.Collision_Occurred then
                  Put_Line("WarpZ Collision Occurred");
                  WarpZ_CP.Collision_Occurred := False;
                  if Scene_Manager.all.Current_Manager.ID = Main_Scene_ID then
                     Scene_Manager.all.SetManager (Next_Scene_ID);
                  else
                     Scene_Manager.all.SetManager (Main_Scene_ID);
                  end if;
               end if;
            end;            
         end if;
         <<Continue>>
      end loop;
  end;
end ECS_Scene_Manager_Test;