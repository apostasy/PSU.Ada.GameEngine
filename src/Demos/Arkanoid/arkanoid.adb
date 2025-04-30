-- Ada Libraries
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces.C;
with System;
-- Game Engine ECS modules
with ECS;                     use ECS;
with ECS.Component;           use ECS.Component;
with ECS.Entity;              use ECS.Entity;
with ECS.Entity_Manager;      use ECS.Entity_Manager;
with ECS.Event;               use ECS.Event;
with ECS.Event_Manager;       use ECS.Event_Manager;
with ECS.System;              use ECS.System;
with ECS.System.Animation;    use ECS.System.Animation;
with ECS.System.Collision;    use ECS.System.Collision;
with ECS.System.Enemy_Spawner;use ECS.System.Enemy_Spawner;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Render;       use ECS.System.Render;
with ECS.System.User_Input;   use ECS.System.User_Input;
with GameMath;                use GameMath;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Renderer;       use Graphics.Renderer;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
-- win32 interface
with Win32;                   use Win32;
with Window;                  use Window;
-- User defined modules
with Arkanoid_Inputs;         use Arkanoid_Inputs;
with Custom_Components;       use Custom_Components;
with Bricks;                  use Bricks;
with Bricklayer;              use Bricklayer;
with Colliders;               use Colliders;
with ECS.System.Arkanoid;     use ECS.System.Arkanoid;
procedure Arkanoid is

   Width                 : Integer                 := 224;
   Height                : Integer                 := 240;
   Title                 : Unbounded_String        := To_Unbounded_String ("Arkanoid Clone");
   GameWindow            : Window_Access;
   Buffer                : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. Width * Height * 4);
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Duration;
   -- Entity Manager and Entities
   Manager               : Manager_Access           := new Entity_Manager_T' (Entity_List.Empty_Vector,Entity_List.Empty_Vector);
   Event_Mgr             : Platform_Event_Handler_Access := new Platform_Event_Handler;
   Player                : Entity_Access            := Manager.all.AddEntity ("Playr");
   Ball                  : Entity_Access            := Manager.all.AddEntity ("Ball1");

   -- Systems
   Mover                : Mover_T          := (Width, Height);
   Collision            : Collision_T      := (Width, Height);
   Render               : Render_T         := (Width, Height, Buffer);
   UserInput            : User_Input_T     := (Player, Event_Mgr, False, True);
   EnemySpawner         : Enemy_Spawn_T;
   Animation            : Animation_T;
   Arkanoid             : Arkanoid_T;

   -- Textures
   bkgrd                : constant String       := "Data\bkgrd.qoi";
   Vaus_File            : constant String       := "Data\vaus.qoi";
   Bricks_File          : constant String       := "Data\bricks.qoi";
   Vaus_Texture         : Texture_Access;
   Bricks_Texture       : Texture_Access;



   -- Player components
   Transform_P : Component_Access := new Transform_T'(Position => (X => 150.0, Y => 210.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_P : Transform_T renames Transform_T(Transform_P.all);
   Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_P      : Component_Access := new AABB_T'(
      Left     => T_P.Position.X, 
      Bottom   => T_P.Position.Y, 
      Right    => T_P.Position.X, 
      Top      => T_P.Position.Y
   );
   Collision_Params_P : Component_Access := new Collision_Params_T'(
      Collision_Enabled    => True,
      Collision_Occurred   => False,
      Destroy_On_Collision => False,
      Prev_Frame_Collision       => False
   );

   Shape_P     : Component_Access := new Quad_T'(
      Width    => 32.0,
      Height   => 8.0,
      C => (R=> 255, G => 255, B => 0, A => 255)
   );
   S_P : Quad_T renames Quad_T(Shape_P.all);
   Paddle : Single_Animation_Access := new Single_Animation_T'(0,0,0.0,0.0,32,16,32,16,0,1);
   Anim_Comp : constant Animation_Component_T := (
      Animations  => (others => null), 
      Textures    => (others => null),
      Current     => Idle
   );
   Animations_P : Component_Access := new Animation_Component_T'(Anim_Comp);
   
   

   Transform_Ball : Component_Access := new Transform_T'((T_P.Position.X + (S_P.Width / 2.0) - 2.0, T_P.Position.Y - 4.0),(0.0,0.0),0.0);
   T_Ball : Transform_T renames Transform_T(Transform_Ball.all);
   Rigidbody_Ball : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_Ball      : Component_Access := new AABB_T'(
      Left     => T_Ball.Position.X,
      Bottom   => T_Ball.Position.Y, 
      Right    => T_Ball.Position.X,
      Top      => T_Ball.Position.Y
   );
   Collision_Ball : Component_Access := new Collision_Params_T'(True,False,False,False);
   Shape_Ball     : Component_Access := new Quad_T'(
      Width    => 5.0,
      Height   => 4.0,
      C        => (0, 0, 0, 0)
   );
   Anim_Ball : Single_Animation_Access := new Single_Animation_T'(0,0,0.0,0.0,0,40,0,40,0,1);
   Animations_Ball : Component_Access := new Animation_Component_T'(Anim_Comp);
   Ball_State : Component_Access := new Ball_State_T'(Ball_Launched => False, Previous_Pos => (0.0,0.0));



   

   -- 1 = Orange; 2 = Light Blue; 3 = Lime; 4 = Red; 5 = Blue; 6 = Pink; 7 = Yellow
   -- 10 = Crystal
   -- 20 = Indestructable

   Level1 : constant Level := (
      3 ,3 ,3 ,3 ,3 ,3 ,3 ,3 ,3 ,3 ,3 ,3 ,
      10,10,10,10,10,10,10,10,10,10,10,10,
      1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,
      2 ,2 ,2 ,2 ,2 ,2 ,2 ,2 ,2 ,2 ,2 ,2 ,
      10,10,10,10,10,10,10,10,10,10,10,10,
      others => -1
   ); 




begin
   Register_Mouse_Callback (16#200#, Mouse_Move'Access);
   Register_Mouse_Callback (16#201#, L_Button_Down'Access);
   Register_Mouse_Callback (16#202#, L_Button_Up'Access);
   Register_Mouse_Callback (16#205#, R_Button_Up'Access);

   -- Set animations
   Anims_P : Animation_Component_T renames Animation_Component_T(Animations_P.all);
   Anims_Ball : Animation_Component_T renames Animation_Component_T(Animations_Ball.all);
   Anims_P.Animations(Idle) := Paddle;
   Anims_Ball.Animations(Idle) := Anim_Ball;

   -- Add entity components
   Player.all.Add_Component (Transform_P);
   Player.all.Add_Component (Rigidbody_P);
   Player.all.Add_Component (AABB_P);
   Player.all.Add_Component (Collision_Params_P);
   Player.all.Add_Component (Shape_P);
   Player.all.Add_Component (Animations_P);

   Ball.all.Add_Component(Transform_Ball);
   Ball.all.Add_Component(Rigidbody_Ball);
   Ball.all.Add_Component(AABB_Ball);
   Ball.all.Add_Component(Collision_Ball);
   Ball.all.Add_Component(Shape_Ball);
   Ball.all.Add_Component (Ball_State);
   Ball.all.Add_Component (Animations_Ball);

   B_S : Ball_State_T renames Ball_State_T(Ball_State.all);

   -- Used to calculate the frame time
   Start_Time := Clock;
   Stop_Time  := Clock;
   GameWindow := New_Window (IC.int (Width), IC.int (Height), Title);
   declare
      Message           : MSG_Access := new MSG;
      Has_Msg           : Boolean    := True;
      Lp_Result         : LRESULT;
      Background_Image  : QOI_Image_Data;
      Texture_Image     : QOI_Image_Data;
   begin

      Background_Image        := Load_QOI (bkgrd);
      Texture_Image           := Load_QOI(Vaus_File);
      Vaus_Texture            := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
      Texture_Image           := Load_QOI(Bricks_File);
      Bricks_Texture          := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
      Anims_P.Textures(Idle)  := Vaus_Texture;
      Anims_Ball.Textures(Idle) := Vaus_Texture;


      Lay_Bricks (Manager, Bricks_Texture, Level1);
      Add_Wall (Manager, 7.0, 240.0, (0.0,   8.0), "LWall");
      Add_Wall (Manager, 7.0, 240.0, (216.0, 8.0), "RWall");
      Add_Wall (Manager, 225.0, 7.0, (0.0,   0.0), "TWall");


         -- Windows message loop (game loop)
      while Has_Msg loop
         Stop_Time    := Clock;
         Elapsed_Time := To_Duration(Stop_Time - Start_Time);
         Start_Time   := Stop_Time;
         Lp_Result    := Dispatch_Message (Message);
         Has_Msg      := Get_Message (Message, System.Null_Address, 0, 0);
         Manager.all.Update;
         Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Width), Integer(Height), 0,0, Width, Height,Natural(Background_Image.Desc.Width));
         UserInput.Execute (Elapsed_Time, Manager);
         B_S.Previous_Pos := T_Ball.Position;
         Mover.Execute (Elapsed_Time, Manager);
         Collision.Execute (Elapsed_Time, Manager);
         Arkanoid.Execute (Elapsed_Time, Manager);
         Animation.Execute(Elapsed_Time, Manager);
         Draw_String(Buffer.all,10,10,0,0,"SCORE:" & Integer'Image(Score),(255,255,255,255),Width,Height);
         Render.Execute (Elapsed_Time, Manager);
         Draw_Buffer (Buffer.all'Address);
      end loop;
   end;
end Arkanoid;