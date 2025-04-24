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
-- win32 interface
with System.Secondary_Stack;
with Win32;                 use Win32;
with Window;                use Window;

with Input_Callbacks; use Input_Callbacks;

procedure Game is

   bkgrd  : constant String := "Data\blue_sky_640x480px.qoi";
   ground_img : constant String := "Data\grass_dirt_ground_640x100px.qoi";
   cloud0_img : constant String := "Data\cloud_328x150px.qoi";
   cloud1_img : constant String := "Data\cloud_280x98px.qoi";

   package IC renames Interfaces.C;
   use IC;
   Width                 : Integer                 := 250;
   Height                : Integer                 := 250;
   Title                 : Unbounded_String        := To_Unbounded_String ("Game Window");
   GameWindow            : Window_Access;
   Buffer                : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. Width * Height * 4);
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Duration;

   -- Entity Manager and Entities
   Manager               : Manager_Access           := new Entity_Manager_T' (Entity_List.Empty_Vector,Entity_List.Empty_Vector);
   Event_Mgr             : Platform_Event_Handler_Access := new Platform_Event_Handler;
   Player                : Entity_Access            := Manager.all.AddEntity ("Playr");

   -- Systems
   Render             : Render_T         := (Width, Height, Buffer);
   Animation          : Animation_T;

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

   player_walk : constant String := "Data\Walk-S.qoi";
   player_idle : constant String := "Data\Idle-S.qoi";

   -- cloud components
   Cloud_0                 : Entity_Access            := Manager.all.AddEntity ("Cloud");
   Cloud_Transform         : Component_Access := new Transform_T'(Position => (X => 0.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   Cloud_T                 : Transform_T renames Transform_T(Cloud_Transform.all);
   Cloud_Shape             : Component_Access := new Quad_T'(
      Width => 328.0,
      Height => 150.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );

   Cloud_1                 : Entity_Access            := Manager.all.AddEntity ("Cloud");
   Cloud_Transform_1       : Component_Access := new Transform_T'(Position => (X => 375.0, Y => 75.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   Cloud_T_1                 : Transform_T renames Transform_T(Cloud_Transform_1.all);
   Cloud_Shape_1           : Component_Access := new Quad_T'(
      Width => 328.0,
      Height => 150.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );

   Cloud_2                 : Entity_Access            := Manager.all.AddEntity ("Cloud");
   Cloud_Transform_2       : Component_Access := new Transform_T'(Position => (X => 25.0, Y => 75.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   Cloud_T_2               : Transform_T renames Transform_T(Cloud_Transform_2.all);
   Cloud_Shape_2           : Component_Access := new Quad_T'(
      Width => 280.0,
      Height => 98.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );

   Cloud_3                 : Entity_Access            := Manager.all.AddEntity ("Cloud");
   Cloud_Transform_3       : Component_Access := new Transform_T'(Position => (X => 300.0, Y => 25.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   Cloud_T_3               : Transform_T renames Transform_T(Cloud_Transform_3.all);
   Cloud_Shape_3           : Component_Access := new Quad_T'(
      Width => 280.0,
      Height => 98.0,
      C => (R=> 0, G => 0, B => 0, A => 255)
   );

begin
   -- Set animations
   Anims_P : Animation_Component_T renames Animation_Component_T(Animations_P.all);
   Anims_P.Animations(Walk) := Walk_P;
   Anims_P.Animations(Idle) := Idle_P;

   -- Add entity components
   Player.all.Add_Component (Transform_P);
   Player.all.Add_Component (Rigidbody_P);
   Player.all.Add_Component (AABB_P);
   Player.all.Add_Component (Collision_Params_P);
   Player.all.Add_Component (Shape_P);
   Player.all.Add_Component (Animations_P);

   Cloud_0.all.Add_Component (Cloud_Transform);
   Cloud_0.all.Add_Component (Cloud_Shape);   

   Cloud_1.all.Add_Component (Cloud_Transform_1);
   Cloud_1.all.Add_Component (Cloud_Shape_1);

   Cloud_2.all.Add_Component (Cloud_Transform_2);
   Cloud_2.all.Add_Component (Cloud_Shape_2);

   Cloud_3.all.Add_Component (Cloud_Transform_3);
   Cloud_3.all.Add_Component (Cloud_Shape_3);

   -- Used to calculate the frame time
   Start_Time := Clock;
   Stop_Time  := Clock;
   GameWindow := New_Window (IC.int (Width), IC.int (Height), Title);

   Cloud_Texture : Component_Access;

declare
   Message           : MSG_Access := new MSG;
   Has_Msg           : Boolean    := True;
   Lp_Result         : LRESULT;
   Texture_Image     : QOI_Image_Data;
   Background_Image  : QOI_Image_Data;
   Ground_Image      : QOI_Image_Data;
   Cloud_Image       : QOI_Image_Data;

begin

   Background_Image     := Load_QOI (bkgrd);
   Ground_Image         := Load_QOI (ground_img);
   Cloud_Image          := Load_QOI (cloud0_img);

   Cloud_Texture        := new Texture_T'(Integer(Cloud_Image.Desc.Width),Integer(Cloud_Image.Desc.Height),Cloud_Image.Data);

   Cloud_0.all.Add_Component (Cloud_Texture);
   Cloud_1.all.Add_Component (Cloud_Texture);

   Cloud_Image          := Load_QOI (cloud1_img);
   Cloud_Texture        := new Texture_T'(Integer(Cloud_Image.Desc.Width),Integer(Cloud_Image.Desc.Height),Cloud_Image.Data);

   Cloud_2.all.Add_Component (Cloud_Texture);
   Cloud_3.all.Add_Component (Cloud_Texture);

   Texture_Image        := Load_QOI(player_walk);
   Walk_Texture_P       := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
   Texture_Image        := Load_QOI(player_idle);
   Idle_Texture_P       := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);   
   
   Anims_P.Textures(Walk) := Walk_Texture_P;
   Anims_P.Textures(Idle) := Idle_Texture_P;

   T_P.Position.Y := Float(Height - Integer(Ground_Image.Desc.Height) - Integer(53));

   -- Windows message loop (game loop)
   while Has_Msg loop
      Stop_Time    := Clock;
      Elapsed_Time := To_Duration(Stop_Time - Start_Time);
      Start_Time   := Stop_Time;
      Lp_Result    := Dispatch_Message (Message);
      Has_Msg      := Get_Message (Message, System.Null_Address, 0, 0);

      Manager.all.Update;

      -- background sprites
      --  Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height), 0,0, Width, Height,Natural(Background_Image.Desc.Width));
      --  Draw_Image_To_Buffer (Buffer.all, Ground_Image.Data, 0, (Height - Integer(Ground_Image.Desc.Height)), Integer(Ground_Image.Desc.Width), Integer(Ground_Image.Desc.Height), 0,0, Width, Height,Natural(Ground_Image.Desc.Width));

      Render.Execute (Elapsed_Time, Manager);
      Animation.Execute(Elapsed_Time, Manager);

      Cloud_T.Position.X := Cloud_T.Position.X + 0.1;

      if(Cloud_T.Position.X > Float(Width)) then
         Cloud_T.Position.X := -328.0;
      end if;

      Cloud_T_1.Position.X := Cloud_T_1.Position.X + 0.1;

      if(Cloud_T_1.Position.X > Float(Width)) then
         Cloud_T_1.Position.X := -328.0;
      end if;

      Cloud_T_2.Position.X := Cloud_T_2.Position.X + 0.33;

      if(Cloud_T_2.Position.X > Float(Width)) then
         Cloud_T_2.Position.X := -328.0;
      end if;

      Cloud_T_3.Position.X := Cloud_T_3.Position.X + 0.33;

      if(Cloud_T_3.Position.X > Float(Width)) then
         Cloud_T_3.Position.X := -328.0;
      end if;

      Draw_Buffer (Buffer.all'Address);

   end loop;
end;
end Game;