with Interfaces;
with Interfaces.C;
with Win32; use Win32;
with Graphics.Color; use Graphics.Color;
with ECS.Vec2; use ECS.Vec2;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
package Graphics.Renderer is

  type Storage_Array_Access is access all Storage_Array;

   type bool is new boolean;
   for bool'size use 8;

   subtype Max_String_Length is Natural range 1 .. 255;
   type Char_Array is array (Max_String_Length) of Character;

   procedure Clear_Screen
      (img : in out Byte_Array; c : Graphics.Color.Color; Screen_Width : Natural; Screen_Height : Natural);

   procedure set_pixel_color
     (img : in out Byte_Array; x : Integer; y : Integer; c : Graphics.Color.Color; Screen_Width : Natural; Screen_Height : Natural);

   function Get_Pixel_Color (img : in out Byte_Array; x,y : Integer; Screen_Width, Screen_Height : Natural) return Graphics.Color.Color;

   procedure line
     (x0  : in out Integer;
      y0  : in out Integer;
      x1  : in out Integer;
      y1  : in out Integer;
      c   : Graphics.Color.Color;
      img : in out Byte_Array;
      Screen_Width : Natural;
      Screen_Height : Natural);

   procedure Draw_Regular_Polygon
     (img      : in out Byte_Array;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : Float;
      Center_Y : Float;
      c        : Graphics.Color.Color;
      Screen_Width    : Natural;
      Screen_Height   : Natural);

   --procedure Draw_Image_To_Window (img : Image);
   procedure Draw_Filled_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   procedure Draw_Filled_Quad(img : in out Byte_Array; X,Y,Width,Height : Float; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   procedure Draw_Character(img : in out Byte_Array; X,Y,Width,Height : Integer; Char : Character; color : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   procedure Draw_String(img : in out Byte_Array; X,Y : Integer; Width, Height : Integer; S : in String; Color : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   
   procedure Draw_Image_To_Buffer(buffer : in out Byte_Array; img: in out Storage_Array_Access; X,Y,Width,Height : Integer; Screen_Width, Screen_Height : Natural);
   procedure Draw_Image_To_Buffer(buffer : in out Byte_Array; img: in out Storage_Array_Access; X,Y,Width,Height,StartX,StartY : Integer; Screen_Width, Screen_Height,Image_Width : Natural);


   generic
      type t is private;
   procedure generic_swap (x, y : in out t);

end Graphics.Renderer;