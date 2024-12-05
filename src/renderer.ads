with Interfaces;
with Interfaces.C;
with Win32; use Win32;
package Renderer is

   D : constant := 1.0 / 256.0;
   type intensity is delta D range 0.0 .. 1.0 with size => 8;
   function Max return intensity
   is (1.0 - D);

   -- Note that Max_Lines and Max_Length needuse to be static
   type Color_Data is
     array (positive range <>, positive range <>) of intensity;

   type Image
     (width  : natural := 0;
      height : natural := 0)
   is record
      r : Color_Data (1 .. width, 1 .. height);
      g : Color_Data (1 .. width, 1 .. height);
      b : Color_Data (1 .. width, 1 .. height);
      a : Color_Data (1 .. width, 1 .. height);
   end record;

   type bool is new boolean;
   for bool'size use 8;
   type color is record
      r : intensity;
      g : intensity;
      b : intensity;
      a : intensity;
   end record;
   for color use
     record
       r at 0 range 0 .. 7;
       g at 1 range 0 .. 7;
       b at 2 range 0 .. 7;
       a at 3 range 0 .. 7;
     end record;
   for color'Size use Interfaces.C.unsigned_long'size;

   procedure Clear_Screen
      (img : in out Byte_Array; c : color; Width : Natural; Height : Natural);

   procedure set_pixel_color
     (img : in out Byte_Array; x : natural; y : natural; c : color; Width : Natural);

   procedure line
     (x0  : in out natural;
      y0  : in out natural;
      x1  : in out natural;
      y1  : in out natural;
      c   : color;
      img : in out Byte_Array;
      Width : Natural);

   procedure Draw_Regular_Polygon
     (img      : in out Byte_Array;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : Float;
      Center_Y : Float;
      c        : Color;
      Width    : Natural);

   --procedure Draw_Image_To_Window (img : Image);

   generic
      type t is private;
   procedure generic_swap (x, y : in out t);

end Renderer;