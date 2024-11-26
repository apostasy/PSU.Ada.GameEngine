with Window;
with Ada.Text_IO; use Ada.Text_IO;
with Renderer;
with ecs.entity; use ecs.entity;
with ecs.component; use ecs.component;
with ecs.entity_manager; use ecs.entity_manager;
with ecs.system; use ecs.system;
with Interfaces.C;

procedure Window_Pixel_Test is

Color_Red : Renderer.Color := (R => Renderer.Max, G => 0.0, B => 0.0, A => Renderer.Max);
c_color : Interfaces.C.unsigned_long;
for c_color'address use Color_Red'address;


Task T is
   entry Start;
end T;

task body T is
begin
   -- cheat until we get the window events working
   delay 1.0;

   Put_Line("Draw Pixel Test");

   Window.draw_pixel (Interfaces.C.int (300), Interfaces.C.int (300), c_color);
   Window.draw_pixel (Interfaces.C.int (301), Interfaces.C.int (300), c_color);
   Window.draw_pixel (Interfaces.C.int (300), Interfaces.C.int (301), c_color);
   Window.draw_pixel (Interfaces.C.int (301), Interfaces.C.int (301), c_color);
   Window.Redraw;
end T;

begin
   Put_Line("Running Window Tests");
   Window.Init (600, 600, "Game Engine");
   Window.Window;

end Window_Pixel_Test;