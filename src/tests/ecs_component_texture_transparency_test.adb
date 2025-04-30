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

procedure ECS_Component_Texture_Transparency_Test is

  --  type Storage_Array_Access is access all Storage_Array;

  type Input_Data is record
    Data : Storage_Array_Access;
    Desc : QOI.QOI_Desc;
  end record;

  package IC renames Interfaces.C;
  use IC;
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
  Static_Entity : Entity_Access := Manager.all.AddEntity ("Statc");
  Player    : Entity_Access := Manager.all.AddEntity ("Playr");

  -- Systems
  Mover              : Mover_T          := (Width, Height);
  Collision          : Collision_T      := (Width, Height);
  Render             : Render_T         := (Width, Height, Buffer);
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
  Collision_Params_P : Component_Access := new Collision_Params_T'(
    Collision_Enabled => False,
    Collision_Occurred => False,
    Destroy_On_Collision => True,
    Prev_Frame_Collision => False
  );
  
  C_P : Collision_Params_T renames Collision_Params_T(Collision_Params_P.all);

  Shape_P : Component_Access :=
   new Quad_T'
    (Width => 50.0, Height => 50.0,
     C     => (R => 255, G => 255, B => 0, A => 255));

  -- Load Texture

  Texture_P : Component_Access;

    -- Static Entity components
  Transform_S        : Component_Access :=
   new Transform_T'
    (Position => (X => 0.0, Y => 0.0), Velocity => (X => 0.0, Y => 0.0),
     Rotation => 0.0);
  T_S                : Transform_T renames Transform_T (Transform_S.all);
  Rigidbody_S        : Component_Access := new Rigidbody_T'(Mass => 1.0);
  AABB_S             : Component_Access :=
   new AABB_T'
    (Left  => T_S.Position.X, Bottom => T_S.Position.Y + 5.0,
     Right => T_S.Position.X + 5.0, Top => T_S.Position.Y);
  Collision_Params_S : Component_Access := new Collision_Params_T'(
    Collision_Enabled => False,
    Collision_Occurred => False,
    Destroy_On_Collision => True,
    Prev_Frame_Collision => False
  );
  
  C_S : Collision_Params_T renames Collision_Params_T(Collision_Params_S.all);

  Shape_S : Component_Access :=
   new Quad_T'
    (Width => 50.0, Height => 50.0,
     C     => (R => 255, G => 255, B => 0, A => 255));

  -- Load Static Entity Texture

  Texture_S : Component_Access;

  File       : Ada.Streams.Stream_IO.File_Type;
  TextBuffer : Ada.Streams.Stream_Element_Array (1 .. 200);
  Last       : Ada.Streams.Stream_Element_Offset;
  Red_Ball_File_Name  : constant String :=
   "D:\Hold\SWENG480\PSU.Ada.GameEngine.Clean\Data\terrace_360.qoi";
  Green_Ball_File_Name  : constant String :=
   "D:\Hold\SWENG480\PSU.Ada.GameEngine.Clean\Data\char.qoi";

  -- from the example in the QOI package
  -- https://github.com/Fabien-Chouteau/qoi-spark/blob/main/tests/src/tests.adb
  function Load_QOI (Filename : String) return Input_Data is
    use GNAT.OS_Lib;

    FD  : File_Descriptor;
    Ret : Integer;

    Result : Input_Data;
  begin

   Put_Line("Loading QOI file: " & Filename);

    FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

    if FD = Invalid_FD then
      Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
      GNAT.OS_Lib.OS_Exit (1);
    end if;

    declare
      Len     : constant Storage_Count := Storage_Count (File_Length (FD));
      In_Data : constant Storage_Array_Access := new Storage_Array (1 .. Len);
    begin
      Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

      if Ret /= In_Data'Length then
        Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
        GNAT.OS_Lib.OS_Exit (1);
      end if;

      Close (FD);

      QOI.Get_Desc (In_Data.all, Result.Desc);

      declare
        Out_Len     : constant Storage_Count        :=
         Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
        Out_Data    : constant Storage_Array_Access :=
         new Storage_Array (1 .. Out_Len);
        Output_Size : Storage_Count;
      begin
        QOI.Decode
         (Data => In_Data.all, Desc => Result.Desc, Output => Out_Data.all,
          Output_Size => Output_Size);

        Result.Data := Out_Data;

        return Result;

      end;

    end;

  end Load_QOI;

begin

  Register_Key_Callback (16#20#, Space_Key'Access); -- Todo: Add all Key constants to win32.ads file
  Register_Key_Callback (16#57#, W_Key'Access);
  Register_Key_Callback (16#41#, A_Key'Access);
  Register_Key_Callback (16#53#, S_Key'Access);
  Register_Key_Callback (16#44#, D_Key'Access);

  -- Add Static entity components
  Static_Entity.all.Add_Component (Transform_S);
  Static_Entity.all.Add_Component (Rigidbody_S);
  Static_Entity.all.Add_Component (AABB_S);
  Static_Entity.all.Add_Component (Collision_Params_S);
  Static_Entity.all.Add_Component (Shape_S);

  -- Add Player entity components
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

    Texture_Image_P, Texture_Image_S : Input_Data;

  begin

    Texture_Image_P := Load_QOI (Green_Ball_File_Name);

    Texture_Image_S := Load_QOI (Red_Ball_File_Name);

   --   Put_Line (Texture_Image_P.Data.all((Texture_Image_P.Data'Length / 2))'Image);
   --   Put_Line (Texture_Image_S.Data.all((Texture_Image_S.Data'Length / 2))'Image);

    Texture_P := new Texture_T'
      (Width => Integer(Texture_Image_P.Desc.Width), Height => Integer(Texture_Image_P.Desc.Height), Data => Texture_Image_P.Data);

    Player.all.Add_Component (Texture_P);

    Texture_S := new Texture_T'
      (Width => Integer(Texture_Image_S.Desc.Width), Height => Integer(Texture_Image_S.Desc.Height), Data => Texture_Image_S.Data);

   Static_Entity.all.Add_Component (Texture_S);

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
end ECS_Component_Texture_Transparency_Test;
