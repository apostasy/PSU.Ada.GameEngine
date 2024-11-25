with ecs.entity;    use ecs.entity;
with ecs.component; use ecs.component;
with Ada.Tags;      use Ada.Tags;
with Ada.Text_IO;   use Ada.Text_IO;
with Window;        use Window;
with Ada.Numerics.Elementary_Functions;

package body renderer is
   package IC renames interfaces.C;

   type Point is record
      X : Natural;
      Y : Natural;
   end record;

   type Polygon is array (Natural range <>) of Point;

   procedure generic_swap (x, y : in out t) is
      tmp : constant t := x;
   begin
      x := y;
      y := tmp;
   end;

   procedure set_pixel_color
     (img : in out image; x : natural; y : natural; c : color) is
   begin
      img.r (x, y) := c.r;
      img.g (x, y) := c.g;
      img.b (x, y) := c.b;
      img.a (x, y) := c.a;
   end;

   procedure line
     (x0  : in out natural;
      y0  : in out natural;
      x1  : in out natural;
      y1  : in out natural;
      c   : color;
      img : in out image)

   is
      procedure swap is new generic_swap (T => natural);
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
            set_pixel_color (img, y, x, c);
         else
            set_pixel_color (img, x, y, c);
         end if;
         error2 := error2 + derror2;
         if error2 > dx then
            y := y + (if y1 > y0 then 1 else -1);
            error2 := error2 - dx * 2;
         end if;
      end loop;

   end line;

   function Generate_Polygon_Vertices
     (Sides : Positive; Radius : Natural; Center_X, Center_Y : Float)
      return Polygon
   is
      Vertices      : Polygon (0 .. Sides - 1);
      Current_Angle : Natural;
   begin
      for I in Vertices'Range loop
         declare
            Angle : constant Float :=
              2.0 * Ada.Numerics.Pi / Float (Sides) * Float (I);

         begin

            Current_Angle :=
              Natural
                (Float (Center_X)
                 + Float (Radius)
                   * Ada.Numerics.Elementary_Functions.Cos (Angle));

            Vertices (I).X :=
              Natural
                (Float (Center_X)
                 + Float (Radius)
                   * Ada.Numerics.Elementary_Functions.Cos (Angle));
            Vertices (I).Y :=
              Natural
                (Float (Center_Y)
                 + Float (Radius)
                   * Ada.Numerics.Elementary_Functions.Sin (Angle));
         end;
      end loop;
      return Vertices;
   end Generate_Polygon_Vertices;

   procedure Draw_Regular_Polygon
     (img      : in out Image;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : float;
      Center_Y : float;
      c        : Color)
   is
      Vertices : Polygon :=
        Generate_Polygon_Vertices (Sides, Radius, Center_X, Center_Y);
   begin
      for I in Vertices'Range loop
         declare
            Next_I     : constant Natural := ((I + 1) mod Vertices'Length);
            Vertex     : Point := Vertices (I);
            New_Vertex : Point := Vertices (Next_I);
         begin

            line (Vertex.X, Vertex.Y, New_Vertex.X, New_Vertex.Y, c, img);
         end;
      end loop;
   end Draw_Regular_Polygon;

   procedure Draw_Image_To_Window (img : Image) is
      use IC;

      x0, y0 : IC.int := 0;

   begin
      for i in img.r'range(1) loop
         for j in img.r'range(2) loop
            declare
               c       : color :=
                 (img.r (i, j), img.g (i, j), img.b (i, j), img.a (i, j));
               c_color : IC.unsigned_long;
               for c_color'address use c'address;
            begin
               if c_color > 0 then
                  Window.draw_pixel (IC.int (i), IC.int (j), c_color);
               end if;
            end;
         end loop;
      end loop;
   end Draw_Image_To_Window;

end Renderer;
