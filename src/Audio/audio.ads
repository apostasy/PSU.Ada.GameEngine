with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with Win32;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Audio is
   package IC renames Interfaces.C; use IC;
   pragma Linker_Options ("-lwinmm");

   function mciSendStringA (
      lpszCommand : Win32.LPCSTR;
      lpszReturnString : Win32.LPSTR;
      uReturnLength : IC.int;
      hwndCallback : Win32.HANDLE
   ) return IC.int;

   pragma Import (Stdcall, mciSendStringA, "mciSendStringA");
   
   procedure Play_Audio(File_Path : String);
   procedure Cleanup_Sounds;

   private
      use Ada.Strings.Unbounded;

      package Unbounded_Strings renames Ada.Strings.Unbounded;
      package Alias_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Unbounded_Strings.Unbounded_String);

      Tracked_Aliases   : Alias_Vectors.Vector;
      Audio_ID          : Natural := 0;

end Audio;