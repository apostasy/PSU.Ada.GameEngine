with Ada.Text_IO; use Ada.Text_IO;
with ecs;
with ECS.Entity; use ECS.Entity;
with ECS.Component; use ECS.Component;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Renderer; use Graphics.Renderer;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Custom_Components;    use Custom_Components;

package body Arkanoid_Inputs is 

   Mouse_Down_X : Float;

   procedure L_Button_Down(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
   begin
      Mouse_Down_X := ECS.MousePos.CurrentPos.X;
   end L_Button_Down;

   procedure L_Button_Up(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      State_B        : Component_Access;
      Trans_B        : Component_Access;
      Ball_Entity    : Entity_Access;
   begin
      Ball_Entity    := Manager.GetEntity("Ball1");
      State_B        := Ball_Entity.all.Get_Component (Ball_State_T'Tag);
      Trans_B        := Ball_Entity.all.Get_Component (Transform_T'Tag);
      declare
         S_B renames Ball_State_T(State_B.all);
         T_B renames Transform_T(Trans_B.all);
         X_Delta : Float;
         Threshold : constant Float := 0.0;
         BigThreshold : constant Float := 15.0;
      begin
         if not S_B.Ball_Launched then
            X_Delta := ECS.MousePos.CurrentPos.X - Mouse_Down_X;
            if X_Delta > BigThreshold then
               T_B.Velocity := (100.0,-40.0);
            elsif X_Delta < -BigThreshold then
               T_B.Velocity := (-100.0, -40.0);
            elsif X_Delta > Threshold then
               T_B.Velocity := (40.0,-40.0);
            elsif X_Delta < -Threshold then
               T_B.Velocity := (-40.0,-40.0);
            else
               T_B.Velocity := (0.0, -40.0);
            end if;
            S_B.Ball_Launched := True;
         end if;
      end;
   end L_Button_Up;

   procedure R_Button_Up(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      State_B        : Component_Access;
      Trans_B        : Component_Access;
      Ball_Entity    : Entity_Access;
   begin
      Ball_Entity    := Manager.GetEntity("Ball1");
      State_B        := Ball_Entity.all.Get_Component (Ball_State_T'Tag);
      Trans_B        := Ball_Entity.all.Get_Component (Transform_T'Tag);
      declare
         S_B renames Ball_State_T(State_B.all);
         T_B renames Transform_T(Trans_B.all);
      begin
         S_B.Ball_Launched := False;
         T_B.Velocity := (0.0,0.0);
      end;
   end R_Button_Up;

   procedure Mouse_Move(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      Trans          : Component_Access;
      Trans_B        : Component_Access;
      State_B        : Component_Access;
      Quad_P         : Component_Access;
      Player_Entity  : Entity_Access;
      Ball_Entity    : Entity_Access;
      Scaled_X       : Float;
   begin
      Player_Entity  := Manager.GetEntity("Playr");
      Ball_Entity    := Manager.GetEntity("Ball1");
      Trans          := Player_Entity.all.Get_Component (Transform_T'Tag);
      Trans_B        := Ball_Entity.all.Get_Component (Transform_T'Tag);
      State_B        := Ball_Entity.all.Get_Component (Ball_State_T'Tag);
      Quad_P         := Player_Entity.all.Get_Component (Quad_T'Tag);
      declare
         T     renames Transform_T(Trans.all);
         T_B   renames Transform_T(Trans_B.all);
         S_B   renames Ball_State_T(State_B.all);
         Q     renames Quad_T(Quad_P.all);
      begin
         Scaled_X := Float(ECS.MousePos.CurrentPos.X) * (225.0 / Float(ECS.WindowWidth));
         T.Position.X := Float'Max(8.0, Float'Min(216.0 - Q.Width, Scaled_X));
         if not S_B.Ball_Launched then
            T_B.Position := (T.Position.X + (Q.Width / 2.0) - 2.0, T.Position.Y - 4.0);
         end if;
         
      end; 
   end Mouse_Move;

end Arkanoid_Inputs;