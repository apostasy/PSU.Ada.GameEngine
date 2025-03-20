with ecs.entity;    use ecs.entity;
with ecs.component; use ecs.component;
with Ada.Tags;      use Ada.Tags;
with Ada.Text_IO;   use Ada.Text_IO;
-- with Window;        use Window;
with Ada.Numerics.Elementary_Functions;
with Graphics.Text; use Graphics.Text;


with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;



package body Graphics.Renderer is
   package IC renames interfaces.C;

   type Point is record
      X : Integer;
      Y : Integer;
   end record;

   type Polygon is array (Natural range <>) of Point;

   procedure generic_swap (x, y : in out t) is
      tmp : constant t := x;
   begin
      x := y;
      y := tmp;
   end generic_swap;
   
   procedure set_pixel_color
     (img : in out Byte_Array; x, y : Integer; c : Graphics.Color.Color; Screen_Width, Screen_Height : Natural) is
     Index : Natural := (y * Screen_Width + x) * 4;
   begin
      if (x > 0 and x < Screen_Width) and (y > 0 and y < Screen_Height) then
         Img(Index)     := Byte(C.B);
         Img(Index + 1) := Byte(C.G);
         Img(Index + 2) := Byte(C.R);
         Img(Index + 3) := Byte(C.A);
      end if;
   end set_pixel_color;

   function Get_Pixel_Color (img : in out Byte_Array; x,y : Integer; Screen_Width, Screen_Height : Natural) return Graphics.Color.Color is
      Index : Natural := ((y mod Screen_Height) * Screen_Width + (x mod Screen_Width) ) * 4;
      C : Graphics.Color.Color;
   begin
      C.B := Graphics.Color.Color_Int(Img(Index));
      C.G := Graphics.Color.Color_Int(Img(Index + 1));
      C.R := Graphics.Color.Color_Int(Img(Index + 2));
      C.A := Graphics.Color.Color_Int(Img(Index + 3));
      return C;
   end Get_Pixel_Color;

   procedure Clear_Screen ( img : in out Byte_Array; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural) is
   begin
      for Y in 0 .. Screen_Height - 1 loop
         for X in 0 .. Screen_Width - 1 loop
            set_pixel_color(img, X, Y, c, Screen_Width, Screen_Height);
         end loop;
      end loop;
   end Clear_Screen;

   procedure line
     (x0  : in out Integer;
      y0  : in out Integer;
      x1  : in out Integer;
      y1  : in out Integer;
      c   : Graphics.Color.Color;
      img : in out Byte_Array;
      Screen_Width : Natural;
      Screen_Height : Natural)

   is
      procedure swap is new generic_swap (T => Integer);
      steep              : Boolean := False;
      dx                 : Integer := x0 - x1;
      dy                 : Integer := y0 - y1;
      derror2, error2, y : integer := 0;

   begin
      if abs dx < abs dy then
         swap (x0, y0);
         swap (x1, y1);
         steep := true;
      end if;
      if x0 > x1 then
         swap (x0, x1);
         swap (y0, y1);
      end if;

      dx := x1 - x0;
      dy := y1 - y0;
      derror2 := (abs dy) * 2;
      y := y0;

      for x in x0 .. x1 loop
         if steep then
            set_pixel_color (img, y, x, c, Screen_Width, Screen_Height);
         else
            set_pixel_color (img, x, y, c, Screen_Width, Screen_Height);
         end if;
         error2 := error2 + derror2;
         if error2 > dx then
            y := y + (if y1 > y0 then 1 else -1);
            error2 := error2 - dx * 2;
         end if;
      end loop;

   end line;

   function Generate_Polygon_Vertices
     (Sides : Positive; Radius : Natural; Center_X, Center_Y : Float; Screen_Width : Natural; Screen_Height : Natural)
      return Polygon
   is
      Vertices      : Polygon (0 .. Sides - 1);
   begin
      for I in Vertices'Range loop
         declare
            Angle : constant Float := 2.0 * Ada.Numerics.Pi / Float (Sides) * Float (I);
            X_Pos : Float := Float(Center_X) + Float(Radius) * Ada.Numerics.Elementary_Functions.Cos(Angle);
            Y_Pos : Float := Float(Center_Y) + Float(Radius) * Ada.Numerics.Elementary_Functions.Sin(Angle); 
         begin
            Vertices(I).X := Integer(X_Pos);
            Vertices(I).Y := Integer(Y_Pos);
         end;
      end loop;
      return Vertices;
   end Generate_Polygon_Vertices;

   procedure Draw_Regular_Polygon
     (img      : in out Byte_Array;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : float;
      Center_Y : float;
      c        : Graphics.Color.Color;
      Screen_Width    : Natural;
      Screen_Height   : Natural)
   is
      Vertices : Polygon :=
        Generate_Polygon_Vertices (Sides, Radius, Center_X, Center_Y, Screen_Width, Screen_Height);
   begin
      for I in Vertices'Range loop
         declare
            Next_I     : constant Natural := ((I + 1) mod Vertices'Length);
            Vertex     : Point := Vertices (I);
            New_Vertex : Point := Vertices (Next_I);
         begin
            line (Vertex.X, Vertex.Y, New_Vertex.X, New_Vertex.Y, c, img, Screen_Width, Screen_Height);
         end;
      end loop;
   end Draw_Regular_Polygon;
   -- Helper for Draw_Filled_Triangle
   procedure DrawHorizontalLine(img : in out Byte_Array; X1,X2: in out Float; Y : Integer; C : Graphics.Color.Color; Screen_Width,Screen_Height : Natural) is
      procedure swap is new generic_swap(T => Float);
   begin
      -- make sure X1 is less than X2
      if X1 > X2 then 
         swap(X1, X2);
      end if;
      for X in Integer(Float'Floor(X1)) .. Integer(Float'Floor(X2)) loop
         set_pixel_color (img, X, Integer(Y), C, Screen_Width, Screen_Height);
      end loop; 
      
   end DrawHorizontalLine;
   -- Helper for Draw_Filled Triangle
   procedure Fill_Top_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural) is
   begin
      -- Calculate slopes for the left and right edges
      LeftSlope : Float := (V3.X - V1.X) / (V3.Y - V1.Y);
      RightSlope : Float := (V3.X - V2.X) / (V3.Y - V2.Y);
      -- Set x-coord for the edges
      CurXLeft : Float := V3.X;
      CurXRight : Float := V3.X;
      -- Iterate through the scanlines
      Y : Integer := Integer(Float'Floor(V3.Y));
      while Y >= Integer(Float'Floor(V1.Y)) loop
         DrawHorizontalLine(img, CurXLeft, CurXRight, Y, C, Screen_Width,Screen_Height);
         CurXLeft := CurXLeft - LeftSlope;
         CurXRight := CurXRight - RightSlope;
         Y := Y - 1;
      end loop;
   end Fill_Top_Triangle;
   -- Helper for Draw_Filled_Triangle
   procedure Fill_Bottom_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural) is
   begin
      -- Calculate slopes for the left and right edges
      LeftSlope : Float := (V2.X - V1.X) / (V2.Y - V1.Y);
      RightSlope : Float := (V3.X - V1.X) / (V3.Y - V1.Y);
      -- Set x-coord for the edges
      CurXLeft : Float := V1.X;
      CurXRight : Float := V1.X;
      -- Iterate through the scanlines
      for Y in Integer(V1.Y) .. Integer(V2.Y) loop
         DrawHorizontalLine(img, CurXLeft, CurXRight, Y, C, Screen_Width,Screen_Height);
         CurXLeft := CurXLeft + LeftSlope;
         CurXRight := CurXRight + RightSlope;
      end loop;
   end Fill_Bottom_Triangle;
   -- Scanline rasterization for filled triangle
   procedure Draw_Filled_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural) is
      procedure swap is new generic_swap (T => Float);
   begin
      -- Sort vertices by Y
      if V1.Y > V2.Y then 
         swap(V1.X, V2.X);
         swap(V1.Y, V2.Y);
      end if;
      if V2.Y > V3.Y then
         swap(V2.X, V3.X);
         swap(V2.Y, V3.Y);
      end if;
      -- Recheck the first pair to ensure order
      if V1.Y > V2.Y then
         swap(V1.X, V2.X);
         swap(V1.Y, V2.Y);
      end if;
      -- Check if triangle is flat-top (i.e. the bottom part of a split triangle, maybe this needs to be renamed)
      if V2.Y = V3.Y then
         Fill_Bottom_Triangle(img, V1, V2, V3, C, Screen_Width,Screen_Height);
      -- Check if triangle is flat-bottomed 
      elsif V1.Y = V2.Y then
         Fill_Top_Triangle(img, V1, V2, V3, C, Screen_Width,Screen_Height);
      else
         -- Split triangle into flat-top and flat-bottom triangle
         SplitX : Float := V1.X + (V2.Y - V1.Y) / (V3.Y - V1.Y) * (V3.X - V1.X);
         SplitVertex : Vec2 := (SplitX, V2.Y);
         Fill_Bottom_Triangle(img, V1, V2, SplitVertex, C, Screen_Width, Screen_Height);
         Fill_Top_Triangle(img, V2, SplitVertex, V3, C, Screen_Width, Screen_Height);
      end if;
   end Draw_Filled_Triangle;

   procedure Draw_Filled_Quad(img : in out Byte_Array; X,Y,Width,Height : Float; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural) is
      V1, V2, V3, V4, V5, V6 : Vec2;
   begin
      -- Draw triangle takes in the vertices as reference and swaps them so we need 6 vertices instead of 4 for the quad
      V1 := (X,Y);
      V2 := (X, Y + Height);
      V3 := (X + Width, Y);
      V4 := (X, Y + Height);
      V5 := (X + Width, Y + Height);
      V6 := (X + Width, Y);
      -- Draw flat-top triangle
      Draw_Filled_Triangle(img, V1, V2, V3, C, Screen_Width, Screen_Height);
      -- Draw flat-bottom triangle
      Draw_Filled_Triangle(img, V4, V5, V6, C, Screen_Width, Screen_Height);
   end Draw_Filled_Quad;

procedure Draw_Character(
   img                           : in out Byte_Array; 
   X, Y, Width, Height           : Integer; 
   Char                          : Character; 
   color                         : Graphics.Color.Color; 
   Screen_Width, Screen_Height   : Natural
   ) 
   is
   C : Text_Array := Get_Character(Char);
   StartX : Integer := X;
   StartY : Integer := Y;
begin
   for I in 0 .. C'Length - 1 loop
      declare
         Bits : Graphics.Text.Text := C(I);
      begin
         for J in reverse 0 .. 7 loop
            -- Print the most significant bit
            declare
               Bit : Integer := Integer((Bits / (2**J)) and 1);
            begin
               if Bit = 1 then
                  set_pixel_color (img, StartX, StartY, color, Screen_Width, Screen_Height); -- Print the pixel and move to the next position
                  StartX := StartX + 1;
               else
                  StartX := StartX + 1; 
               end if;
            end;
         end loop;
         StartY := StartY + 1; -- New line
         StartX := X;          -- Reset cursor
      end;
   end loop;
end Draw_Character;

procedure Draw_String(
   img                           : in out Byte_Array; 
   X,Y                           : Integer;
   Width, Height                 : Integer; 
   S                             : in String;
   Color                         : Graphics.Color.Color;
   Screen_Width, Screen_Height   : Natural
   ) 
   is
   StartX   : Integer := X;
   StartY   : Integer := Y;
   Char     : Character;
   Char_Spacing : constant Natural := 10;        
begin
   for I in 1 .. S'Length loop
      Char := S(I);
      Draw_Character (img, StartX, StartY, Width, Height, Char, Color, Screen_Width, Screen_Height);
      StartX := StartX + Char_Spacing; 
   end loop;
end Draw_String;


   procedure Draw_Image_To_Buffer(buffer : in out Byte_Array; img: in out Storage_Array_Access; X,Y,Width,Height : Integer; Screen_Width, Screen_Height : Natural) is

      function Blend_Color_Values (A, B, Alpha : Float) return Float is
      begin
         return A * Alpha + B * (1.0 - Alpha);
      end Blend_Color_Values;

   begin

      for I in 0 .. (Height - 1) loop
         begin
            for J in 0 .. (Width - 1) loop
               declare
                  Img_Index : Natural := (I * Width + J) * 4 + 1;
                  -- need to offset the buffer index by the x and y values            
                  Buffer_Index : Natural := ((Y mod Screen_Height + I) * Screen_Width + (X mod Screen_Width + J) ) * 4;

                  New_Red_Value, New_Green_Value, New_Blue_Value, New_Alpha_Value : Float;
                  Original_Red_Value, Original_Green_Value, Original_Blue_Value : Float;
                  Blended_Red, Blended_Green, Blended_Blue : Float;

               begin
                  if X + J < Screen_Width and Y + I < Screen_Height then

                     --  Put_Line("Buffer Index: " & Buffer_Index'Image & " Img Index: " & Img_Index'Image);

                     Original_Red_Value := Float(buffer(Buffer_Index+2));
                     Original_Green_Value := Float(buffer(Buffer_Index+1));
                     Original_Blue_Value := Float(buffer(Buffer_Index));

                     New_Red_Value := Float(img.all(Storage_Offset(Img_Index)));
                     New_Green_Value := Float(img.all(Storage_Offset(Img_Index+1)));
                     New_Blue_Value := Float(img.all(Storage_Offset(Img_Index+2)));
                     New_Alpha_Value := Float(img.all(Storage_Offset(Img_Index+3))) / 255.0;

                     Blended_Red := Blend_Color_Values(New_Red_Value, Original_Red_Value, New_Alpha_Value);
                     Blended_Green := Blend_Color_Values(New_Green_Value, Original_Green_Value, New_Alpha_Value);
                     Blended_Blue := Blend_Color_Values(New_Blue_Value, Original_Blue_Value, New_Alpha_Value);

                     buffer(Buffer_Index) := Byte(Blended_Blue);
                     buffer(Buffer_Index+1) := Byte(Blended_Green);
                     buffer(Buffer_Index+2) := Byte(Blended_Red);
                     buffer(Buffer_Index+3) := Byte(255); -- The window buffer does not support alpha values

                  end if;
               end;
            end loop;
         end;
      end loop;

   end Draw_Image_To_Buffer;


   procedure Draw_Image_To_Buffer(buffer : in out Byte_Array; img: in out Storage_Array_Access; X,Y,Width,Height,StartX,StartY : Integer; Screen_Width, Screen_Height,Image_Width : Natural) is

      function Blend_Color_Values (A, B, Alpha : Float) return Float is
      begin
         return A * Alpha + B * (1.0 - Alpha);
      end Blend_Color_Values;
   Img_Channel_Offset : constant Natural := 1;
   begin

      for I in 0 .. (Height - 1) loop
         begin
            for J in 0 .. (Width - 1) loop
               declare
                  Img_Index : Natural := ((StartY + I) * Image_Width + (StartX + J)) * 4 + Img_Channel_Offset;
                  -- need to offset the buffer index by the x and y values            
                  Buffer_Index : Integer := ((Y + I) * Screen_Width + (X + J) ) * 4;
                  New_Red_Value, New_Green_Value, New_Blue_Value, New_Alpha_Value : Float;
                  Original_Red_Value, Original_Green_Value, Original_Blue_Value : Float;
                  Blended_Red, Blended_Green, Blended_Blue : Float;

               begin
                  if (X + J > 0 and X + J < Screen_Width) and (Y + I > 0 and Y + I < Screen_Height) then

                     --  Put_Line("Buffer Index: " & Buffer_Index'Image & " Img Index: " & Img_Index'Image);

                     Original_Red_Value := Float(buffer(Buffer_Index+2));
                     Original_Green_Value := Float(buffer(Buffer_Index+1));
                     Original_Blue_Value := Float(buffer(Buffer_Index));

                     New_Red_Value := Float(img.all(Storage_Offset(Img_Index)));
                     New_Green_Value := Float(img.all(Storage_Offset(Img_Index+1)));
                     New_Blue_Value := Float(img.all(Storage_Offset(Img_Index+2)));
                     New_Alpha_Value := Float(img.all(Storage_Offset(Img_Index+3))) / 255.0;

                     Blended_Red    := Blend_Color_Values(New_Red_Value, Original_Red_Value, New_Alpha_Value);
                     Blended_Green  := Blend_Color_Values(New_Green_Value, Original_Green_Value, New_Alpha_Value);
                     Blended_Blue   := Blend_Color_Values(New_Blue_Value, Original_Blue_Value, New_Alpha_Value);


            
                     buffer(Buffer_Index)    := Byte(Blended_Blue);
                     buffer(Buffer_Index+1)  := Byte(Blended_Green);
                     buffer(Buffer_Index+2)  := Byte(Blended_Red);
                     buffer(Buffer_Index+3)  := Byte(255); -- The window buffer does not support alpha values

                  end if;
               end;
            end loop;
         end;
      end loop;

   end Draw_Image_To_Buffer;

   --  procedure Draw_Image_To_Window (img : Image) is
   --     use IC;

   --     x0, y0 : IC.int := 0;

   --  begin
   --     for i in img.r'range(1) loop
   --        for j in img.r'range(2) loop
   --           declare
   --              c       : color :=
   --                (img.r (i, j), img.g (i, j), img.b (i, j), img.a (i, j));
   --              c_color : IC.unsigned_long;
   --              for c_color'address use c'address;
   --           begin
   --              if c_color > 0 then
   --                 Window.draw_pixel (IC.int (i), IC.int (j), c_color);
   --              end if;
   --           end;
   --        end loop;
   --     end loop;
   --  end Draw_Image_To_Window;

end Graphics.Renderer;