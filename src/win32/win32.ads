with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package Win32 is

   package IC renames Interfaces.C; use IC;
   package ICS renames IC.Strings;

   CS_VREDRAW        : constant := 16#1#;
   CS_HREDRAW        : constant := 16#2#;

   WM_DESTROY        : constant := 16#2#;
   WM_PAINT          : constant := 16#f#;
   WM_QUIT           : constant := 16#0012#;
   WM_SETCURSOR      : constant := 16#20#;  -- Define the set cursor message
   HTCLIENT          : constant := 16#01#;  -- Hit-test value for the client area
   COLOR_BACKGROUND  : constant := 1;
   BLACK_BRUSH       : constant := 4;

   WS_OVERLAPPEDWINDOW : constant := 16#cf0000#;
   WS_VISIBLE        : constant := 16#10000000#;

   SW_SHOW           : constant := 5;

   BI_RGB            : constant := 0;
   DIB_RGB_COLORS    : constant := 0;

   -- Key constants
   WM_KEYDOWN        : constant := 16#100#; -- Define the key-down message
   WM_KEYUP          : constant := 16#101#; -- Define the key-up message
   WM_MOUSEMOVE      : constant := 16#200#; -- Mouse move message
   WM_LBUTTONDOWN    : constant := 16#201#; -- Define the left mouse button down message
   WM_LBUTTONUP      : constant := 16#202#; -- Left mouse button up
   WM_LBUTTONDBLCLK  : constant := 16#203#; -- Left mouse button double click
   WM_RBUTTONDOWN    : constant := 16#204#; -- Right mouse button down
   WM_RBUTTONUP      : constant := 16#205#; -- Right mouse button up
   

   MK_LBUTTON        : constant := 1;       -- Left mouse button
   MK_RBUTTON        : constant := 2;       -- Right mouse button
   MK_SHIFT          : constant := 4;       -- SHIFT key
   MK_CONTROL        : constant := 8;       -- CTRL key
   MK_MBUTTON        : constant := 16;      -- Middle mouse button
   MK_XBUTTON1       : constant := 32;      -- X1 mouse button (extra button)
   MK_XBUTTON2       : constant := 64;      -- X2 mouse button (extra button)

   



   -- Windowing events -- 
   WM_SIZE           : constant := 16#0005#; -- Sent after a window's size has changed
   WM_SIZING         : constant := 16#0214#; -- Sent while a window is being resized
   WM_GETMINMAXINFO  : constant := 16#0024#; -- Sent to determine minimum and maximum window sizes
   WM_WINDOWPOSCHANGED : constant := 16#0047#; -- Sent after a window's position or size changes
    -- Text Formating options, used in DrawTextA as a bit mask ( e.g., DT_LEFT | DTVCENTER)
    -- Horizontal Alignment
   DT_LEFT           : constant := 16#0000#; -- Align text to the left
   DT_CENTER         : constant := 16#0001#; -- Center text horizontally
   DT_RIGHT          : constant := 16#0002#; -- Align text to the right

   -- Vertical Alignment
   DT_TOP            : constant := 16#0000#; -- Align text to the top (default)
   DT_VCENTER        : constant := 16#0004#; -- Center text vertically
   DT_BOTTOM         : constant := 16#0008#; -- Align text to the bottom

   -- Text Formatting
   DT_WORDBREAK      : constant := 16#0010#; -- Automatically break lines
   DT_SINGLELINE     : constant := 16#0020#; -- Single line (ignore word wrapping)
   DT_EXPANDTABS     : constant := 16#0040#; -- Expand tab characters
   DT_TABSTOP        : constant := 16#0080#; -- Set tab stops (use `DT_EXPANDTABS`)
   DT_NOCLIP         : constant := 16#0100#; -- Do not clip text (allow drawing outside rect)
   DT_EXTERNALLEADING : constant := 16#0200#; -- Include external leading in height calculations

   -- Ellipsis & Trimming
   DT_CALCRECT       : constant := 16#0400#; -- Calculate the bounding rectangle without drawing
   DT_NOPREFIX       : constant := 16#0800#; -- Ignore `&` prefix for shortcut keys
   DT_INTERNAL       : constant := 16#1000#; -- Use system font metrics

   -- Ellipsis options (used when text overflows)
   DT_END_ELLIPSIS   : constant := 16#8000#; -- Add `...` at the end of overflowing text
   DT_PATH_ELLIPSIS  : constant := 16#4000#; -- Add `...` in the middle of a file path
   DT_WORD_ELLIPSIS  : constant := 16#20000#; -- Add `...` at the end of last visible word

   -- Used in SetBkMode
   TRANSPARENT       : constant := 1;        -- Background is transparent (no fill behind text)
   OPAQUE            : constant := 2;        -- Background is filled with the current background color

 --  type LPCSTR is access constant IC.char;
   type LPCSTR is new ICS.chars_ptr;
 --  type LPSTR is access all IC.char;
   type LPSTR is new ICS.chars_ptr;
   IDI_APPLICATION : LPCSTR;
   IDC_ARROW       : LPCSTR;

   subtype PVOID is System.Address;
   subtype PCVOID is PVOID;
   subtype HANDLE is PVOID;
   subtype LPVOID is PVOID;
   subtype HWND is HANDLE;
   subtype HDC is HANDLE;
   subtype HMENU is HANDLE;
   subtype HINSTANCE is HANDLE;
   subtype HICON is HANDLE;
   subtype HCURSOR is HANDLE;
   subtype HBRUSH is HANDLE;
   subtype HGDIOBJ is HANDLE;
   subtype HBITMAP is HANDLE;

   subtype DWORD is IC.unsigned_long;
   SRCCOPY : constant DWORD := 16#cc0020#;

   subtype USHORT is IC.unsigned_short;
   subtype WORD is USHORT;

   type Byte is mod 2**8 with size => 8; 
   type Byte_Array is array (Natural range <>) of Byte;
   type Byte_Array_Access is access Byte_Array;

   type ATOM is new IC.unsigned_short;

   type WPARAM is mod 2 ** Standard'Address_Size;

   type LONG_PTR is range -(2 ** (Standard'Address_Size - 1))
                       .. +(2 ** (Standard'Address_Size - 1) - 1);
   subtype LPARAM is LONG_PTR;
   subtype LRESULT is LONG_PTR;

   type BITMAPINFOHEADER is record
      biSize          : DWORD   := 0;
      biWidth         : Ic.int  := 0;
      biHeight        : Ic.int  := 0;
      biPlanes        : WORD    := 0;
      biBitCount      : WORD    := 0;
      biCompression   : DWORD   := 0;
      biSizeImage     : DWORD   := 0;
      biXPelsPerMeter : Ic.long := 0;
      biYPelsPerMeter : Ic.long := 0;
      biClrUsed       : DWORD   := 0;
      biClrImportant  : DWORD   := 0;
   end record;

   type BITMAP is record
      bmType         : IC.long;
      bmWidth        : IC.long;
      bmHeight       : IC.long;
      bmWidthBytes   : IC.long;
      bmPlanes       : IC.short;
      bmBitsPixel    : IC.short;
      bmBits         : System.Address;
   end record;

   type RGBQUAD is record
      rgbBlue     : Byte := 0;
      rgbGreen    : Byte := 0;
      rgbRed      : Byte := 0;
      rgbReserved : Byte := 0;
   end record;

   ANYSIZE_ARRAY : constant := 0;
   type RGBQUAD_Array is array (Integer range 0 .. ANYSIZE_ARRAY) of aliased RGBQUAD;

   type BITMAPINFO is record
      bmiHeader : BITMAPINFOHEADER;
      bmiColors : RGBQUAD_Array;
   end record;

   function Check (A : ATOM) return Boolean is
      (if A = 0 then False else True);

   type WNDPROC is
     access function
       (H_Wnd : HWND; Msg : IC.unsigned; W_Param : WPARAM; L_Param : LPARAM)
        return LRESULT;

   function Load_Icon
     (H_Instance : HINSTANCE; Lp_Icon_Name : LPCSTR) return HICON
   with Import => True, Convention => C, External_Name => "LoadIconA";

   function Load_Cursor
     (H_Instance : HINSTANCE; Lp_Cursor_Name : LPCSTR) return HCURSOR
   with Import => True, Convention => C, External_Name => "LoadCursorA";

   function TO_LPCSTR is new Ada.Unchecked_Conversion (IC.Strings.chars_ptr, LPCSTR);

   Lp_Window_Name : LPCSTR := LPCSTR(ICS.New_String ("Ada Window"));
   Lp_Class_Name  : LPCSTR := LPCSTR(ICS.New_String ("Core"));
   Lp_Menu_Name   : LPCSTR := LPCSTR(ICS.New_String (""));

   type WNDCLASS is record
      Style           : IC.unsigned := CS_HREDRAW or CS_VREDRAW;
      Lp_fn_Wnd_Proc  : WNDPROC;
      Cb_Cls_Extra    : IC.int := 0;
      Cb_Wnd_Extra    : IC.int := 0;
      H_Instance      : HINSTANCE := System.Null_Address;
      H_Icon          : HICON   := Load_Icon (System.Null_Address, IDI_APPLICATION);
      H_Cursor        : HCURSOR := Load_Cursor (System.Null_Address, IDC_ARROW);
      H_br_Background : HBRUSH  := System.Null_Address;
      Lpsz_Menu_Name  : LPCSTR  := Lp_Menu_Name;
      Lpsz_Class_Name : LPCSTR  := Lp_Class_Name;
   end record;
   type WNDCLASS_Access is access all WNDCLASS;

   type POINT is record
      X, Y : IC.long;
   end record;

   type MSG is record
      H_Wnd   : HWND;
      Message : IC.unsigned;
      W_Param : WPARAM;
      L_Param : LPARAM;
      Time    : DWORD;
      Pt      : POINT;
   end record;
   type MSG_Access is access all MSG;

   type RECT is record
      Left   : IC.long := 0;
      Top    : IC.long := 0;
      Right  : IC.long := 0;
      Bottom : IC.long := 0;
   end record;
   type RECT_Access is access all RECT;

   type PAINTSTRUCT is record
      H_dc         : HDC;
      F_Erase      : Interfaces.C.int;
      Rc_Paint     : RECT;
      F_Restore    : Interfaces.C.int;
      F_Inc_Update : Interfaces.C.int;
      Rgb_Reserved : Interfaces.C.char_array(0 .. 31);
   end record;

   function Draw_Text_A(
      H_dc        : HDC;
      Lp_Ch_Text  : LPCSTR;
      C_Ch_Text   : IC.int;
      Lp_Rc       : in out RECT;
      Format      : IC.unsigned
   ) return IC.int
   with Import => True, Convention => C, External_Name => "DrawTextA";

   function Text_Out_A(
      H_dc        : HDC;
      x           : IC.int;
      y           : IC.int;
      Lp_String   : LPCSTR;
      len         : IC.int
   ) return Boolean
   with Import => True, Convention => C, External_Name => "TextOutA";

   function Set_Text_Color(
      H_dc        : HDC;
      Color       : DWORD
   ) return DWORD
   with Import => True, Convention => C, External_Name => "SetTextColor";

   function Set_Bk_Mode(
      H_dc        : HDC;
      Mode        : IC.int
   ) return IC.int
   with Import => True, Convention => C, External_Name => "SetBkMode";

   function Create_Compatible_Bitmap(
      H_dc        : HDC;
      Cx          : IC.int;
      Cy          : IC.int
   ) return HBITMAP
   with Import => True, Convention => C, External_Name => "CreateCompatibleBitmap";  

   function Begin_Paint (H_Wnd : HWND; Lp_Paint : access PAINTSTRUCT) return HDC
   with Import => True, Convention => C, External_Name => "BeginPaint";

   function End_Paint (H_Wnd : HWND; Lp_Paint : access PAINTSTRUCT) return Boolean
   with Import => True, Convention => C, External_Name => "EndPaint";

   function Set_DI_Bits_To_Device
     (H_dc         : HDC;
      X_Dest       : IC.int;
      Y_Dest       : IC.int;
      Dw_Width     : DWORD;
      Dw_Height    : DWORD;
      X_Src        : IC.int;
      Y_Src        : IC.int;
      U_Start_Scan : IC.int;
      C_Scan_Lines : IC.unsigned;
      Lpv_Bits     : PCVOID;
      Lp_Bmi       : access BITMAPINFO;
      Fu_Color_Use : IC.unsigned) return IC.int
   with Import => True, Convention => C, External_Name => "SetDIBitsToDevice";

   procedure Post_Quit_Message (N_Exit_Code : IC.int)
   with Import => True, Convention => C, External_Name => "PostQuitMessage";

   function Get_Object(
      h  : HANDLE;
      c  : IC.int;
      pv : LPVOID
   ) return IC.int
   with Import => True, Convention => C, External_Name => "GetObject";

   function Def_Window_Proc
     (H_Wnd   : HWND;
      Msg     : IC.unsigned;
      W_Param : WPARAM;
      L_Param : LPARAM)
      return LRESULT
    with Import => True, Convention => C, External_Name => "DefWindowProcA";

   function Get_Last_Error return DWORD
   with Import => True, Convention => C, External_Name => "GetLastError";

   function Fill_Rect (H_Dc : HDC; Lp_Rc : System.Address; H_br : HBRUSH) return IC.int
   with Import => True, Convention => C, External_Name => "FillRect";

   function Get_Stock_Object (Fn_Object : IC.int) return HGDIOBJ
   with Import => True, Convention => C, External_Name => "GetStockObject";


   function Create_Compatible_DC (H_Dc  : HDC) return HDC
   with Import => True, Convention => C, External_Name => "CreateCompatibleDC";

   function Create_DIB_Section (H_Dc      : HDC;
                                P_Bmi     : access BITMAPINFO;
                                I_Usage   : IC.int;
                                Ppv_Bits  : access PVOID;
                                H_Section : HANDLE;
                                Dw_Offset : DWORD)
                                return HBITMAP
   with Import => True, Convention => C, External_Name => "CreateDIBSection"; 

   function Select_Object (H_Dc : HDC; Hgdi_Obj : HGDIOBJ) return HGDIOBJ
   with Import => True, Convention => C, External_Name => "SelectObject";    

   function Set_DI_Bits (H_Dc         : HDC;
                         hbmp         : HBITMAP;
                         U_Start_Scan : IC.unsigned;
                         C_Scan_Lines : IC.unsigned;
                         Lpv_Bits     : PCVOID;
                         Lp_Bmi       : access BITMAPINFO;
                         Fu_Color_Use : IC.unsigned) return IC.int
   with Import => True, Convention => C, External_Name => "SetDIBits";

   function Bit_Blt (hdcDest : HDC;
                     nXDest  : IC.int;
                     nYDest  : IC.int;
                     nWidth  : IC.int;
                     nHeight : IC.int;
                     hdcSrc  : HDC;
                     nXSrc   : IC.int;
                     nYSrc   : IC.int;
                     dwRop   : DWORD) return Boolean
   with Import => True, Convention => C, External_Name => "BitBlt";

   function Delete_Object (Hgdi_Obj : HGDIOBJ) return Boolean
   with Import => True, Convention => C, External_Name => "DeleteObject";

   function Delete_DC (H_Dc : HDC) return Boolean
   with Import => True, Convention => C, External_Name => "DeleteDC";

   function Register_Class (Lp_Wnd_Class : access WNDCLASS) return ATOM
   with Import => True, Convention => C, External_Name => "RegisterClassA";

   function Create_Window
     (Dw_Ex_Style    : DWORD;
      Lp_Class_Name  : LPCSTR;
      Lp_Window_Name : LPCSTR;
      Dw_Style       : DWORD;
      X              : IC.int;
      Y              : IC.int;
      Width          : IC.int;
      Height         : IC.int;
      H_Wnd_Parent   : HWND;
      H_Menu         : HMENU;
      H_Instance     : HINSTANCE;
      Lp_Param       : LPVOID) return HWND
   with Import => True, Convention => C, External_Name => "CreateWindowExA";

   function Show_Window (H_Wnd : HWND; N_Cmd_Show : IC.int) return Boolean
   with Import => True, Convention => C, External_Name => "ShowWindow"; 

   function Update_Window (H_Wnd : HWND) return Boolean
   with Import => True, Convention => C, External_Name => "UpdateWindow"; 

   function Get_Message (Lp_Msg : MSG_Access; H_Wnd : HWND;
                         W_Msg_Filter_Min : IC.unsigned;
                         W_Msg_Filter_Max : IC.unsigned) return Boolean
   with Import => True, Convention => C, External_Name => "GetMessageA";

   function Dispatch_Message (Lp_Msg : MSG_Access) return LRESULT
   with Import => True, Convention => C, External_Name => "DispatchMessageA";

   function Get_H_Instance return HINSTANCE
   with Import => True, Convention => C, External_Name => "rts_get_hInstance";

   function Stretch_DIBits
      (
         H_Dc           : HDC;
         X_Dest         : IC.int;
         Y_Dest         : IC.int;
         Dest_Width     : IC.int;
         Dest_Height    : IC.int;
         X_Src          : IC.int;
         Y_Src          : IC.int;
         Src_Width      : IC.int;
         Src_Height     : IC.int;
         Bits           : PCVOID;
         Bitmap_Info    : access BITMAPINFO;
         Usage          : IC.unsigned;
         Rop            : DWORD
      ) return IC.int
   with Import => True, Convention => C, External_Name => "StretchDIBits";

   function GetDC (H_Wnd : HWND) return HDC
   with Import => True, Convention => C, External_Name => "GetDC";

   function ReleaseDC
      (
         H_Wnd : HWND;
         H_Dc  : HDC
      ) return Boolean
   with Import => True, Convention => C, External_Name => "ReleaseDC";

   function GetCursorPos (
      lpPoint : access POINT
   ) return Interfaces.C.int
   with Import => True, Convention => Stdcall, External_Name => "GetCursorPos";

   function ScreenToClient (
      H_Wnd    : HWND;
      lpPoint : access POINT
   ) return Interfaces.C.int
   with Import => True, Convention => Stdcall, External_Name => "ScreenToClient";


end Win32;