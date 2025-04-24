with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Renderer; use Graphics.Renderer;
with Graphics.Color; use Graphics.Color;
with Ada.Real_Time; use Ada.Real_Time;
with System;
with Window; use Window;
with Win32; use Win32;

procedure Test_Window_String is

Width       : Integer                  := 640;
Height      : Integer                  := 360;
Title       : Unbounded_String         := To_Unbounded_String("Game Window");
GameWindow  : Window_Access;
Buffer      : Win32.Byte_Array_Access  := new Win32.Byte_Array(0 .. Width * Height * 4);
SkyBlue     : Color                    := (R => 135, G => 206, B => 236, A => 255);
Start_Time  : Time;
Stop_Time   : Time;
Elapsed_Time: Duration;

 begin

   Put_Line("Test Camera");

   Start_Time := Clock;
   Stop_Time := Clock;

   GameWindow := New_Window(IC.int(Width),IC.int(Height),Title);

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
         Clear_Screen(Buffer.all,Graphics.Color.Blue, Width, Height);

         Draw_String(Buffer.all, 200,200, 0, 0, "HELLO TEAM", Graphics.Color.White, Width,Height);
         Draw_String(Buffer.all, 250, 250, 0, 0, "0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ", (255,19,240,0), Width, Height); 
         Draw_String(Buffer.all, 50, 50, 0, 0, "FPS:" & Integer'Image(FPS), Graphics.Color.Green, Width, Height);
         Draw_Buffer(Buffer.all'Address);
         --delay 0.008; -- temporary measure to control frame rate
      end loop;

   end;

end Test_Window_String;