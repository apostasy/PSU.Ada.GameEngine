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
with ECS.Entity_Manager_List; use ECS.Entity_Manager_List;
with GameMath;                use GameMath;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Renderer;       use Graphics.Renderer;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;

-- User defined modules
with Input_Callbacks; use Input_Callbacks;
with ECS.System.Enemy_Spawner;use ECS.System.Enemy_Spawner;

procedure Tech_Demo_Camera is

   Width       : Integer                  := 540;
   Height      : Integer                  := 450;

   Title       : Unbounded_String         := To_Unbounded_String("Game Window");
   GameWindow  : Window_Access;
   Buffer      : Win32.Byte_Array_Access  := new Win32.Byte_Array(0 .. Width * Height * 4);
   SkyBlue     : Color                    := (R => 135, G => 206, B => 236, A => 255);
   Start_Time  : Time;
   Stop_Time   : Time;
   Launch_Time   : Time;
   Elapsed_Time: Duration;

   -- Entity Manager and Entities

   Main_Scene_ID : constant Unbounded_String := To_Unbounded_String ("Main");
   Next_Scene_ID : constant Unbounded_String := To_Unbounded_String ("Next");

   Main_Scene               : Manager_Access           := new Entity_Manager_T' (Main_Scene_ID, Entity_List.Empty_Vector,Entity_List.Empty_Vector);
   Next_Scene               : Manager_Access           := new Entity_Manager_T' (Next_Scene_ID, Entity_List.Empty_Vector,Entity_List.Empty_Vector);

   Scene_Manager : Manager_List_Access := new Entity_Manager_List_T'(Entity_Manager_List.Empty_Vector, Main_Scene);

   Event_Mgr             : Platform_Event_Handler_Access := new Platform_Event_Handler;

   -- Systems
   Render             : Render_T         := (Width, Height, Buffer);
   Animation          : Animation_T;
   EnemySpawner       : Enemy_Spawn_T;
   Mover              : Mover_T          := (Width, Height);
   Collision          : Collision_T      := (Width, Height);

   -- camera

   --  Camera_E : Entity_Access := Manager.all.AddEntity ("Cam__");
   --  Camera_Transform : Component_Access := new Transform_T'(
   --     Position => (X => 0.0, Y => 0.0),
   --     Velocity => (X => 0.0, Y => 0.0),
   --     Rotation => 0.0
   --  );
   Camera_P : Component_Access := new Camera_Component_T'(
      Position => (X => 0.0, Y => 0.0),
      Width => Width,
      Height => Height,
      Zoom => 1.0,
      Buffer => Buffer
   );
   Camera_T : Camera_Component_T renames Camera_Component_T(Camera_P.all);

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

   level_1_background : constant String := "..\Data\Fighter Backgroun 1.qoi";
   level_2_background : constant String := "..\Data\Fighter Backgroun 2.qoi";

   --  Level_1                 : Entity_Access            := Manager.all.AddEntity ("Lvl_1");
   --  Level_1_Transform         : Component_Access := new Transform_T'(Position => (X => -100.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   --  Level_1_T                 : Transform_T renames Transform_T(Level_1_Transform.all);
   --  Level_1_Shape             : Component_Access := new Quad_T'(
   --     Width => 4500.0,
   --     Height => 450.0,
   --     C => (R=> 0, G => 0, B => 0, A => 255)
   --  );

   Level_1_Texture : Component_Access;

   Level_2                 : Entity_Access            := Next_Scene.all.AddEntity ("Lvl_2");
   Level_2_Transform         : Component_Access := new Transform_T'(Position => (X => -100.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   Level_2_T                 : Transform_T renames Transform_T(Level_2_Transform.all);
   Rigidbody_Level_2 : Component_Access := new Rigidbody_T'(Mass => 0.0);
   AABB_Level_2      : Component_Access := new AABB_T'(
      Left => Level_2_T.Position.X, 
      Bottom => Level_2_T.Position.Y, 
      Right => Level_2_T.Position.X, 
      Top => Level_2_T.Position.Y);
   Collision_Params_Level_2 : Component_Access := new Collision_Params_T'(
      Collision_Enabled => True,
      Collision_Occurred => False,
      Destroy_On_Collision => True,
      Wall_Collision => False
   );
   C_TLevel_2         : Collision_Params_T renames Collision_Params_T(Collision_Params_Level_2.all);
   Level_2_Shape             : Component_Access := new Quad_T'(
      Width => 4500.0,
      Height => 450.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );

   Level_2_Texture : Component_Access;

   Tests             : Entity_Access            := Main_Scene.all.AddEntity ("Tests");

   Transform_Test : Component_Access := new Transform_T'(Position => (X => -100.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_Test : Transform_T renames Transform_T(Transform_Test.all);
   Rigidbody_Test : Component_Access := new Rigidbody_T'(Mass => 0.0);
   AABB_Test      : Component_Access := new AABB_T'(
      Left => T_Test.Position.X, 
      Bottom => T_Test.Position.Y, 
      Right => T_Test.Position.X, 
      Top => T_Test.Position.Y);
   Collision_Params_Test : Component_Access := new Collision_Params_T'(
      Collision_Enabled => True,
      Collision_Occurred => False,
      Destroy_On_Collision => True,
      Wall_Collision => False
   );
   C_Test         : Collision_Params_T renames Collision_Params_T(Collision_Params_Test.all);
   Shape_Test     : Component_Access := new Quad_T'(
      Width => 36.0,
      Height => 53.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );
   
   Player             : Entity_Access            := Main_Scene.all.AddEntity ("Playr");
   UserInput          : User_Input_T     := (Player, Event_Mgr, False, True);

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

      warp_texture : constant String := "..\Data\testimage.qoi";

   Warp1                 : Entity_Access            := Main_Scene.all.AddEntity ("WarpZ");
   Warp2                 : Entity_Access            := Next_Scene.all.AddEntity ("WarpZ");
   -- Warp1 components
   Transform_W1 : Component_Access := new Transform_T'(Position => (X => 1200.0, Y => Float(Height) / 4.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
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
   warp1_texture : constant String := "..\Data\red.qoi";

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
   warp2_texture : constant String := "..\Data\green.qoi";

 begin

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

   Tests.all.Add_Component (Transform_Test);
   Tests.all.Add_Component (Rigidbody_Test);
   Tests.all.Add_Component (AABB_Test);
   Tests.all.Add_Component (Collision_Params_Test);
   Tests.all.Add_Component (Shape_Test);

   -- Add entity components
   Player.all.Add_Component (Transform_P);
   Player.all.Add_Component (Rigidbody_P);
   Player.all.Add_Component (AABB_P);
   Player.all.Add_Component (Collision_Params_P);
   Player.all.Add_Component (Shape_P);
   Player.all.Add_Component (Animations_P);

   Player.all.Add_Component (Camera_P);

   Next_Scene.all.AddEntity (Player);

   --  Level_2.all.Add_Component (Level_2_Transform);
   --  Level_2.all.Add_Component (Rigidbody_Level_2);
   --  Level_2.all.Add_Component (AABB_Level_2);
   --  Level_2.all.Add_Component (Collision_Params_Level_2);
   --  Level_2.all.Add_Component (Level_2_Shape);   

   Level_2.all.Add_Component (Transform_Test);
   Level_2.all.Add_Component (Rigidbody_Test);
   Level_2.all.Add_Component (AABB_Test);
   Level_2.all.Add_Component (Collision_Params_Test);
   Level_2.all.Add_Component (Shape_Test);
   
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


   Start_Time := Clock;
   Stop_Time := Clock;

   Launch_Time := Clock;
   Total_Time : Duration;

   GameWindow := New_Window(IC.int(Width),IC.int(Height),Title);

   Scene_Manager.all.AddManager (Main_Scene);
   Scene_Manager.all.AddManager (Next_Scene);

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

      Texture_Image        := Load_QOI(level_1_background);
      Level_1_Texture := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
      Tests.all.Add_Component (Level_1_Texture);

      Texture_Image        := Load_QOI(level_2_background);
      Level_2_Texture := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
      Level_2.all.Add_Component (Level_2_Texture);
      

      while Has_Msg loop      

         Clear_Screen(Buffer.all,Graphics.Color.Blue, Width, Height);
         
         Stop_Time := Clock;
         Elapsed_Time := To_Duration(Stop_Time - Start_Time);
         FPS := Integer(1.0 / Float(Elapsed_Time));
         Start_Time := Stop_Time;
         Lp_Result := Dispatch_Message (Message);
         Has_Msg := Get_Message (Message, System.Null_Address, 0, 0);     

         Scene_Manager.all.Current_Manager.Update;

         UserInput.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
         --  EnemySpawner.Execute (Elapsed_Time, Manager);
         Collision.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
         Mover.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
         Render.Execute (Elapsed_Time, Scene_Manager.all.Current_Manager);
         Animation.Execute(Elapsed_Time, Scene_Manager.all.Current_Manager);

         Draw_Buffer (Buffer.all'Address);

         Camera_T.Position.X := T_P.Position.X - Float(Width / 4);

         --  if T_P.Position.X > 200.0 then
         --     T_P.Position.X := 50.0;
         --     Put_Line(Scene_Manager.all.Current_Manager.ID'Image & " " & T_P.Position.X'Image);
         --     if Scene_Manager.all.Current_Manager.ID = Main_Scene_ID then
         --        Put_Line("Switching to Next Scene");
         --        Scene_Manager.all.SetManager (Next_Scene_ID);               
         --     else
         --        Put_Line("Switching to Main Scene");
         --        Scene_Manager.all.SetManager (Main_Scene_ID);
         --     end if;
         --  end if;

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
         <<Continue>>

      end loop;

   end;


end Tech_Demo_Camera;