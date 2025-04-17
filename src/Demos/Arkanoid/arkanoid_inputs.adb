with Ada.Text_IO; use Ada.Text_IO;
with ecs;
with ECS.Entity; use ECS.Entity;
with ECS.Component; use ECS.Component;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Renderer; use Graphics.Renderer;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package body Arkanoid_Inputs is 


   procedure L_Button_Down(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
   begin
      Put_Line ("Left mouse pressed at "& ECS.MousePos.CurrentPos'Image);
   end L_Button_Down;

   procedure L_Button_Up(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
   begin
      Put_Line ("Left mouse released at "& ECS.MousePos.CurrentPos'Image);
   end L_Button_Up;

   procedure Mouse_Move(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      Trans : Component_Access;
      Player_Entity : Entity_Access;
   begin
      Player_Entity := Manager.GetEntity("Playr");
      if Player_Entity = null then
         Put_Line ("Player entity not found");
         return;
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      declare
         T renames Transform_T(Trans.all);
      begin
         T.Position.X := ECS.MousePos.CurrentPos.X;
      end; 
   end Mouse_Move;

end Arkanoid_Inputs;