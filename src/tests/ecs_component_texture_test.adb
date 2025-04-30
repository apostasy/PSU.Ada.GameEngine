with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Window;                use Window;
with Win32;                 use Win32;
with System;
with Graphics.Renderer;     use Graphics.Renderer;
with Ada.Real_Time;         use Ada.Real_Time;
with Graphics.Color;        use Graphics.Color;
with ecs.Entity_Manager;    use ecs.Entity_Manager;
with ecs.Event;             use ecs.Event;
with ecs.Event_Manager;     use ecs.Event_Manager;
with GameMath;              use GameMath;
with ecs.System;            use ecs.System;
with ecs.System.Movement;   use ecs.System.Movement;
with ecs.System.Collision;  use ecs.System.Collision;
with ecs.System.Render;     use ecs.System.Render;
with ecs.System.User_Input; use ecs.System.User_Input;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;

with Input_Callbacks; use Input_Callbacks;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces;
with ecs.entity;    use ecs.entity;
with ecs.Component; use ecs.Component;
with Interfaces.C;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with QOI; use QOI;
with GNAT.OS_Lib;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

procedure ECS_Component_Texture_Test is


  package IC renames Interfaces.C;
  use IC;
  Width                 : Integer                 := 640;
  Height                : Integer                 := 360;
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
  Player        : Entity_Access := Manager.all.AddEntity ("Playr");

  -- Systems
  Mover              : Mover_T          := (Width, Height);
  Collision          : Collision_T      := (Width, Height);
  Render             : Render_T         := (Width, Height, Buffer);
  UserInput          : User_Input_T     := (Player, Event_Mgr, False, True);


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
   Prev_Frame_Collision => False
);
C_P         : Collision_Params_T renames Collision_Params_T(Collision_Params_P.all);

Shape_P     : Component_Access := new Quad_T'(
   Width => 36.0,
   Height => 54.0,
   C => (R=> 255, G => 255, B => 0, A => 255)
);

  -- Load Texture

  Texture_P : Component_Access;

  File       : Ada.Streams.Stream_IO.File_Type;
  Last       : Ada.Streams.Stream_Element_Offset;
  bkgrd  : constant String := "C:\ProgramData\Ada\PSU.Ada.GameEngine.Fork\Data\terrace_360.qoi";
  player_texture : constant String := "C:\ProgramData\Ada\PSU.Ada.GameEngine.Fork\Data\char.qoi";

  -- from the example in the QOI package
  -- https://github.com/Fabien-Chouteau/qoi-spark/blob/main/tests/src/tests.adb



begin

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

  Start_Time := Clock;
  Stop_Time  := Clock;

  GameWindow := New_Window (IC.int (Width), IC.int (Height), Title);
  Put_Line ("Start Engine");

  declare
    Message   : MSG_Access := new MSG;
    Has_Msg   : Boolean    := True;
    Lp_Result : LRESULT;

    Texture_Image : QOI_Image_Data;
    Background_Image : QOI_Image_Data;

  begin

    Background_Image := Load_QOI (bkgrd);

    Texture_Image := Load_QOI(player_texture);
        Texture_P := new Texture_T'
      (Width => Integer(Texture_Image.Desc.Width), Height => Integer(Texture_Image.Desc.Height), Data => Texture_Image.Data);

      Put_Line("Character width: " & Texture_Image.Desc.Width'Image & " Character height: " & Texture_Image.Desc.Height'Image);
    
    Player.all.Add_Component (Texture_P);

    while Has_Msg loop
      Stop_Time    := Clock;
      Elapsed_Time := Stop_Time - Start_Time;
      Start_Time   := Stop_Time;
      Lp_Result    := Dispatch_Message (Message);
      Has_Msg      := Get_Message (Message, System.Null_Address, 0, 0);
      -- Process emitted events here - for debug purposes
      Manager.all.Update;
      --Clear_Screen (Buffer.all, Graphics.Color.Blue, Width, Height);
      Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height),0,0, Width, Height, Natural(Background_Image.Desc.Width));
      UserInput.Execute (To_Duration (Elapsed_Time), Manager);
      Collision.Execute (To_Duration (Elapsed_Time), Manager);
      Mover.Execute (To_Duration (Elapsed_Time), Manager);
      Render.Execute (To_Duration (Elapsed_Time), Manager);
      Draw_Buffer (Buffer.all'Address);
    end loop;

  end;
end ECS_Component_Texture_Test;
