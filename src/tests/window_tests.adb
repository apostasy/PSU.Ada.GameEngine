with Window;
with Ada.Text_IO; use Ada.Text_IO;
with Renderer;
with ecs.entity; use ecs.entity;
with ecs.component; use ecs.component;
with ecs.entity_manager; use ecs.entity_manager;
with ecs.system; use ecs.system;
with Interfaces.C;

procedure Window_Tests is

Image : Renderer.Image(600, 600);
Color_Red : Renderer.Color := (R => Renderer.Max, G => 0.0, B => 0.0, A => Renderer.Max);

Manager : Entity_Manager_T;
Player : Entity_Access := Manager.AddEntity("Playr");
Enemy : Entity_Access := Manager.AddEntity("Enemy");
Mover : Mover_T;
Renderer_System : Renderer_T;

Player_Transform : Component_Access := new Transform_T'(X => 300.0, Y => 300.0, Rotation => 0.0);
Player_Rigidbody : Component_Access := new Rigidbody_T'(Mass => 1.0);
Player_Shape : Component_Access := new Shape_T'(Sides => 6, Radius => 10, Color => Color_Red);

Task T is
   entry Start;
end T;

task body T is
begin
   Put_Line("Running Draw Task");
   -- cheat until we get the window events working
   delay 0.5;

   Renderer_System.Image := Image;

  Player.all.Add_Component(Player_Transform);
  Player.all.Add_Component(Player_Rigidbody);
  Player.all.Add_Component(Player_Shape);

  Manager.Update;  

    -- 1/60 of a second after program launch
  Execute ( Mover, 1.0/60.0, Player);
  Execute ( Renderer_System, 1.0/60.0, Player);

   Put_Line("Draw Shape");
   --  Renderer.Draw_Regular_Polygon (Image, 6, 10, 300.0, 300.0, Color_Red);
   --  Window.Draw_Image_To_Window (Image);

   declare
      c_color : Interfaces.C.unsigned_long;
               for c_color'address use Color_Red'address;
   begin
      Window.draw_pixel (Interfaces.C.int (300), Interfaces.C.int (300), c_color);
      Window.draw_pixel (Interfaces.C.int (301), Interfaces.C.int (300), c_color);
      Window.draw_pixel (Interfaces.C.int (300), Interfaces.C.int (301), c_color);
      Window.draw_pixel (Interfaces.C.int (301), Interfaces.C.int (301), c_color);
      Window.Redraw;
   end;
end T;

begin
   Put_Line("Running Window Tests");
   Window.Init (600, 600, "Game Engine");
   Window.Window;

end Window_Tests;