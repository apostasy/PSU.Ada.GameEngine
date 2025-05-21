with Custom_Components; use Custom_Components;
with ECS.Entity; use ECS.Entity;
with GameMath;
package body ECS.System.Arkanoid is 


   BallPreviousPos : GameMath.Vec2;
   
   procedure Animate_Hit(Col,An,Br : Component_Access) is      
      C renames Collision_Params_T(Col.all);
      A renames Animation_Component_T(An.all);
      B renames Brick_Attributes(Br.all);
   begin
      if C.Prev_Frame_Collision then
         if B.Hits = 2 or B.Indestructable then
            A.Current := Walk;
         end if;
      elsif A.Current = Walk and A.Animations(Walk).CurFrame = 4 then
         A.Current := Idle;
      end if;
   end Animate_Hit;


   procedure Reflect_Ball(Manager : access Entity_Manager_T'Class; Col,AABB : Component_Access) is
      Ball        : constant Entity_Access := Manager.GetEntity ("Ball1");
      Collision   : Component_Access := Ball.all.Get_Component (Collision_Params_T'Tag);
      Transform   : Component_Access := Ball.all.Get_Component (Transform_T'Tag);
      B_AABB      : Component_Access := Ball.all.Get_Component(AABB_T'Tag);
      B_State     : Component_Access := Ball.all.Get_Component(Ball_State_T'Tag);
      B_C renames Collision_Params_T(Collision.all);
      B_T renames Transform_T(Transform.all);
      B_A renames AABB_T(B_AABB.all);
      B_S renames Ball_State_T(B_State.all);
      E_C renames Collision_Params_T(Col.all);
      E_A renames AABB_T(AABB.all);
   begin

      if B_S.Ball_Launched and B_C.Collision_Occurred and E_C.Prev_Frame_Collision then
         if abs(B_A.Top - E_A.Bottom) < 0.5 or abs(B_A.Bottom - E_A.Top) < 0.5 then
            B_T.Position := B_S.Previous_Pos;
            B_T.Velocity.Y := -B_T.Velocity.Y;
         elsif abs(B_A.Left - E_A.Right) < 0.5 or abs(B_A.Right - E_A.Left) < 0.5 then
            B_T.Position := B_S.Previous_Pos;
            B_T.Velocity.X := -B_T.Velocity.X;
         end if;
      end if;
   end Reflect_Ball;

   procedure Scoring(Collision, Brick_Attr: Component_Access) is
      C renames Collision_Params_T(Collision.all);
      B renames Brick_Attributes(Brick_Attr.all);
   begin
      if C.Collision_Occurred and not C.Prev_Frame_Collision then
            if B.Hits > 0 then
               B.Hits := B.Hits - 1;
               Score := Score + B.Score;
            end if;

            if B.Hits = 0 then
               C.Destroy_On_Collision := True;
            end if;

      end if;
   end Scoring;


   procedure Execute(Self : in out Arkanoid_T;
                      Dt   : Duration; 
                      Manager : access Entity_Manager_T'Class
   ) is 
   begin
      for Entity of Manager.all.Entities loop
      declare
         Trans       : Component_Access   :=    Entity.all.Get_Component (Transform_T'Tag);
         Collision   : Component_Access   :=    Entity.all.Get_Component (Collision_Params_T'Tag);
         Anims       : Component_Access   :=    Entity.all.Get_Component (Animation_Component_T'Tag);
         Brick_Attr  : Component_Access   :=    Entity.all.Get_Component (Brick_Attributes'Tag);
         AABB_E      : Component_Access   :=    Entity.all.Get_Component (AABB_T'Tag);
         Id          : Id_T               :=    Entity.all.Id;
      begin
         if Brick_Attr /= null then
            Animate_Hit (Collision, Anims, Brick_Attr);
            Scoring(Collision,Brick_Attr);
         end if;
         Reflect_Ball (Manager, Collision, AABB_E);
      end;
      end loop;
   end Execute;
end ECS.System.Arkanoid;