with System; use System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with ECS.Event_Manager; use ECS.Event_Manager;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with ECS.Event;
with ECS;

package body Window is

   package IC renames Interfaces.C; use IC;
   package ICS renames IC.Strings;

   W_Instance   : Window_Access;
   -- Declare the event manager
   Manager : aliased ECS.Event_Manager.Platform_Event_Handler;

   -- Return the lower 16 bits of LPARAM
   function LOWORD(Value : LPARAM) return WORD is
   use Interfaces;
   begin
      return WORD(Unsigned_32(Value) and 16#FFFF#);
   end;

   -- Return the upper 16 bits of LPARAM
   function HIWORD(value : LPARAM) return WORD is 
   begin
      return WORD(value / 2 ** 16);
   end HIWORD;

   -- EDITED 11/26/24 to add WM_KEYDOWN and WM_LBUTTONDOWN
   function Wnd_Proc (H_Wnd   : HWND; 
                     Msg     : IC.unsigned; 
                     W_Param : WPARAM; 
                     L_Param : LPARAM) return LRESULT is
   begin
      case Msg is
         when WM_DESTROY =>
            Post_Quit_Message (0);

         when WM_PAINT =>
            null;
         
         when WM_SIZE =>
            W_Instance.Current_Width   := IC.int(LOWORD(L_Param));
            W_Instance.Current_Height  := IC.int(HIWORD(L_Param));
            ECS.WindowWidth := Integer(LOWORD(L_Param));

         when WM_KEYDOWN =>
         declare
            KeyCode : ECS.Event.Byte := ECS.Event.Byte(W_Param);
            Event   : ECS.Event.Event_T :=
            (Source    => 0,
             EventType => ECS.Event.KeyDown,
             Data      => (KeyCode   => KeyCode,
                           MouseX      => 0,
                           MouseY      => 0,
                           W_Width     => 0, 
                           W_Height    => 0,
                           Additional  => (others => 0)));
         begin
            Emit_Event(Manager, Event);
         end;

         when WM_KEYUP =>
         declare
            KeyCode : ECS.Event.Byte := ECS.Event.Byte(W_Param);
            Event   : ECS.Event.Event_T :=
            (Source    => 0,
               EventType => ECS.Event.KeyUp,
               Data      => (KeyCode    => KeyCode,
                           MouseX     => 0,
                           MouseY     => 0,
                           W_Width     => 0, 
                           W_Height    => 0,
                           Additional => (others => 0)));
         begin
            Emit_Event(Manager, Event);
         end;

         when WM_LBUTTONDOWN =>
         declare
            MouseX      :  Integer := Integer(IC.int(LOWORD(L_Param)));
            MouseY      :  Integer := Integer(IC.int(HIWORD(L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.L_MouseDown,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0, 
                             W_Height    => 0,
                             Additional => (others => 0)));
         begin
            Emit_Event(Manager, MouseEvent);
         end;

         when WM_LBUTTONUP =>
         declare
            MouseX    : Integer := Integer(IC.int(LOWORD(L_Param)));
            MouseY    : Integer := Integer(IC.int(HIWORD(L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.L_MouseUp,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0, 
                             W_Height    => 0,
                             Additional => (others => 0)));
         begin
            Emit_Event(Manager, MouseEvent);
         end;
                  when WM_RBUTTONDOWN =>
         declare
            MouseX      :  Integer := Integer(IC.int(LOWORD(L_Param)));
            MouseY      :  Integer := Integer(IC.int(HIWORD(L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.R_MouseDown,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0, 
                             W_Height    => 0,
                             Additional => (others => 0)));
         begin
            Emit_Event(Manager, MouseEvent);
         end;

         when WM_RBUTTONUP =>
         declare
            MouseX    : Integer := Integer(IC.int(LOWORD(L_Param)));
            MouseY    : Integer := Integer(IC.int(HIWORD(L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.R_MouseUp,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0, 
                             W_Height    => 0,
                             Additional => (others => 0)));
         begin
            Emit_Event(Manager, MouseEvent);
         end;
         when WM_MOUSEMOVE =>
         declare
            MouseX    : Integer := Integer(IC.int(LOWORD(L_Param)));
            MouseY    : Integer := Integer(IC.int(HIWORD(L_Param)));
         begin
               declare
                  MouseEvent : ECS.Event.Event_T :=
                  (Source    => 0,
                   EventType => ECS.Event.MouseMove,
                   Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0, 
                             W_Height    => 0,
                             Additional => (others => 0)));
               begin
                  Emit_Event(Manager, MouseEvent);
               end;
           -- end if;
         end;
         when others =>
            return Def_Window_Proc(H_Wnd, Msg, W_Param, L_Param);
      end case;
      return 0;
   end Wnd_Proc;


   -- Creates a new window instance and returns it to the caller
   function New_Window(Width : Interfaces.C.int; Height : Interfaces.C.int; Title : Unbounded_String) return Window_Access is
      WC       : aliased WNDCLASS;
      Res_Atom : ATOM;
      
   begin
      W_Instance := new Window_T;
      -- Initialize the WNDCLASS struct
      WC.Lp_fn_Wnd_Proc    := Wnd_Proc'Access;
      WC.H_Instance        := Get_H_Instance;
      WC.Lpsz_Class_Name   := TO_LPCSTR(ICS.New_String("SampleWindowClass"));
      WC.H_br_Background   := HBRUSH(Get_Stock_Object(COLOR_BACKGROUND));

      -- Register the window class with the win32 API
      Res_ATOM := Register_Class(WC'Access);
      if Res_Atom = 0 then
         raise Program_Error with "Failed to register window class.";
      end if;

      -- Create the window
      W_Instance.Handle := Create_Window(
         Dw_Ex_Style    => 0,
         Lp_Class_Name  => WC.Lpsz_Class_Name,
         Lp_Window_Name => TO_LPCSTR(ICS.New_String(To_String(Title))),
         Dw_Style       => WS_OVERLAPPEDWINDOW,
         X              => 0,  -- defines the window start coordinate. 
         Y              => 0,
         Width          => Width,
         Height         => Height,
         H_Wnd_Parent   => System.Null_Address,
         H_Menu         => System.Null_Address,
         H_Instance     => WC.H_Instance,
         Lp_Param       => System.Null_Address
      );


      -- Window properties
      W_Instance.Width := Width;
      W_Instance.Height := Height;
      W_Instance.Title := Title;
     
      -- Show the window
      declare
         SW_Result : Boolean;
         UW_Result : Boolean;
      begin   
         SW_Result := Show_Window(W_Instance.Handle, SW_SHOW);
         UW_Result := Update_Window(W_Instance.Handle);
      end;
      return W_Instance;

   end New_Window;



   procedure Draw_Buffer(Buffer : System.Address) is
      Bmi_Reset : Byte_Array (0 .. BITMAPINFO'Size / 8 - 1) := (others => 0);
      Bmi : aliased BITMAPINFO with Address => Bmi_Reset'Address;
      Result : Interfaces.C.int;
      Handle_DC : HDC := GetDC(W_Instance.Handle);
   begin
      Bmi.bmiHeader.biSize            := BITMAPINFOHEADER'Size / 8;
      Bmi.bmiHeader.biWidth           := W_Instance.Width;   
      Bmi.bmiHeader.biHeight          := -W_Instance.Height;
      Bmi.bmiHeader.biPlanes          := 1;
      Bmi.bmiHeader.biBitCount        := 32;
      Bmi.bmiHeader.biCompression     := BI_RGB;
      Bmi.bmiHeader.biSizeImage       := 0;
      Bmi.bmiHeader.biXPelsPerMeter   := 0;
      Bmi.bmiHeader.biYPelsPerMeter   := 0;
      Bmi.bmiHeader.biClrUsed         := 0;
      Bmi.bmiHeader.biClrImportant    := 0;

      Result := Stretch_DIBits(
         H_Dc           => Handle_DC,
         X_Dest         => 0,
         Y_Dest         => 0,
         Dest_Width     => W_Instance.Current_Width,
         Dest_Height    => W_Instance.Current_Height,
         X_Src          => 0,
         Y_Src          => 0,
         Src_Width      => W_Instance.Width, 
         Src_Height     => W_Instance.Height,
         Bits           => Buffer,
         Bitmap_Info    => Bmi'Unchecked_Access,
         Usage          => DIB_RGB_COLORS,
         Rop            => SRCCOPY
      );

      -- Release the HDC to avoid memory leak
      if Handle_DC /= Null_Address then
         declare
            Result_Release : Boolean := ReleaseDC(W_Instance.Handle, Handle_DC);
         begin
            if not Result_Release then
               Put_Line("Failed to release HDC.");
            end if;
         end;
      end if;

      -- Check for drawing failure, for debug only, remove or comment out for production or handle it more gracefully
      if Result = 0 then
         Put_Line("StretchDIBits failed.");  
      end if;
   end Draw_Buffer;
end Window;