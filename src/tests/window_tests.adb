with Window;
with Ada.Text_IO; use Ada.Text_IO;
with Renderer;

procedure Window_Tests is

Image : Window.Image(600, 600);
Color_Red : Window.Color := (R => Window.Max, G => 0.0, B => 0.0, A => Window.Max);

Task T is
   entry Start;
end T;

task body T is
begin
   Put_Line("Running Draw Task");
   -- cheat until we get the window events working
   delay 0.1;

   Put_Line("Draw Shape");
   Renderer.Draw_Regular_Polygon (Image, 6, 10, 300.0, 300.0, Color_Red);
   Window.Draw_Image_To_Window (Image);
   Window.Redraw;
end T;

begin
   Put_Line("Running Window Tests");
   Window.Init (600, 600, "Game Engine");
   Window.Window;

end Window_Tests;