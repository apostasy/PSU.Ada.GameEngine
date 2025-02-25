-- Console helpers
with Ada.Text_IO;           use Ada.Text_IO;

-- Window
with Window;                use Window;
with Win32;                 use Win32;
with Ada.Real_Time;         use Ada.Real_Time;
with System;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- PSU Ada Game Engine
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
with Graphics.Renderer;     use Graphics.Renderer;
with Graphics.Color;        use Graphics.Color;

-- Ada C Interfacing
with Interfaces.C;

procedure Presentation_Demo_3 is

Player_Sprite_File_Name  : constant String :=
"D:\Hold\SWENG480\PSU.Ada.GameEngine.Clean\Data\char.qoi";

Player_Sprite_Texture : QOI_Image_Data := Load_QOI(Player_Sprite_File_Name);

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

begin

  Start_Time := Clock;
  Stop_Time  := Clock;

  GameWindow := New_Window (IC.int (Width), IC.int (Height), Title);
  Put_Line ("Start Engine");

  declare
    Message   : MSG_Access := new MSG;
    Has_Msg   : Boolean    := True;
    Lp_Result : LRESULT;
    begin

      while Has_Msg loop
         Stop_Time    := Clock;
         Elapsed_Time := Stop_Time - Start_Time;
         Start_Time   := Stop_Time;
         Lp_Result    := Dispatch_Message (Message);
         Has_Msg      := Get_Message (Message, System.Null_Address, 0, 0);
         -- Process emitted events here - for debug purposes
         Clear_Screen (Buffer.all, Graphics.Color.Blue, Width, Height);
         Draw_Image_To_Buffer (Buffer.all, Player_Sprite_Texture.Data, 0, 0, Integer(Player_Sprite_Texture.Desc.Width), Integer(Player_Sprite_Texture.Desc.Height), Width, Height);
         Draw_Buffer (Buffer.all'Address);
      end loop;

    end;
end Presentation_Demo_3;