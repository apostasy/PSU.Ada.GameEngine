with ecs.component; use ecs.component;
with ecs.entity; use ecs.entity;
with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with ECS.Vec2; use ECS.Vec2;

package body ECS.System is

   procedure Execute ( Self      : Mover_T;
                       Dt        : Duration; 
                       Manager   : access Entity_Manager_T'Class ) is
   begin
      for Entity of Manager.all.Entities loop
         declare
            Trans       : Component_Access   :=    Entity.all.Get_Component (Transform_T'Tag);
            Rigidbodies : Component_Access   :=    Entity.all.Get_Component (Rigidbody_T'Tag);
            AABB        : Component_Access   :=    Entity.all.Get_Component (AABB_T'Tag);
            
            begin
               if Trans = null then
                  Put_Line ("No Transform");
                  return;
               end if;
               if Rigidbodies = null then
                  Put_Line ("No Rigidbodies");
                  return;
               end if;
               if AABB = null then 
                  Put_Line ("No AABB");
                  return;
               end if;

            declare
               T renames Transform_T (Trans.all);
               R renames Rigidbody_T (Rigidbodies.all);
               B renames AABB_T(AABB.all);
               Velocity_Scaled : ECS.Vec2.Vec2 := New_Vec2(X_In => T.Velocity.X, Y_In => T.Velocity.Y);
               begin
                  Scale(Velocity_Scaled, Float(Dt));
                  --Put_Line ("Moving " & Entity.all.Id & " from: " & T.Position.X'Image & ", " & T.Position.Y'Image & ", " & T.Rotation'Image);
                  -- Update the entity position
                  Add(T.Position,Velocity_Scaled);
                  -- Sync bounding box
                  B.Left := B.Left + Velocity_Scaled.X;
                  B.Right := B.Right + Velocity_Scaled.X;
                  B.Top := B.Top + Velocity_Scaled.Y;
                  B.Bottom := B.Bottom + Velocity_Scaled.Y;
                  --Put_Line ("Moved " & Entity.all.Id & " to: " & T.Position.X'Image & ", " & T.Position.Y'Image & ", " & T.Rotation'Image);
               end;
            end;
      end loop;   
   end Execute;

   -- Checks all entities for collisions 
   procedure Execute ( Self      : Collision_T;
                       Dt        : Duration;
                       Manager   : access Entity_Manager_T'Class ) is

         Length : constant Natural := Natural(Manager.all.Entities.Length);
    
          -- Helper to check if two entities are colliding
         function IsColliding (A : access Entity_T'Class; B : access Entity_T'Class) return Boolean is
         Bounding_Box_A : Component_Access := A.all.Get_Component(AABB_T'Tag);
         Bounding_Box_B : Component_Access := B.all.Get_Component(AABB_T'Tag);
         begin
            if Bounding_Box_A = null or else Bounding_Box_B = null then
               return False;
            end if;
            declare
               BB_A : AABB_T renames AABB_T(Bounding_Box_A.all);
               BB_B : AABB_T renames AABB_T(Bounding_Box_B.all);
               A_Right_Of_B : Boolean := BB_A.Left > BB_B.Right;
               A_Left_Of_B  : Boolean := BB_A.Right < BB_B.Left;
               A_Above_B    : Boolean := BB_A.Bottom < BB_B.Top;
               A_Below_B    : Boolean := BB_A.Top > BB_B.Bottom;
            begin
               return not (A_Right_Of_B or A_Left_Of_B or A_Above_B or A_Below_B);
            end;
         end IsColliding;
   begin
      -- Pairwise checking of entities for collision
      for Index1 in 0 .. Length - 1 loop
         declare
            E_1 : Entity_Access := Manager.all.Entities.Element(Index1);
         begin
            for Index2 in Index1 + 1 .. Length - 1 loop
               declare
               -- TODO: Add in null checking
                  E_2 : Entity_Access := Manager.all.Entities.Element(Index2);
                  E1_Collision_Params : Component_Access := E_1.all.Get_Component(Collision_Params_T'Tag);
                  E1_CP renames Collision_Params_T (E1_Collision_Params.all);  
                  E2_Collision_Params : Component_Access := E_2.all.Get_Component(Collision_Params_T'Tag);
                  E2_CP renames Collision_Params_T (E2_Collision_Params.all);         
               begin
                  if IsColliding(E_1,E_2) then
                     if E1_CP.Collision_Enabled and E2_CP.Collision_Enabled then
                        E1_CP.Collision_Occurred := True;
                        E2_CP.Collision_Occurred := True;
                        -- Flag the entity to be removed if set to be destroyed on collision
                        E_1.all.Destroyed := E1_CP.Destroy_On_Collision;
                        E_2.all.Destroyed := E2_CP.Destroy_On_Collision;
                     end if;
                  else
                     E1_CP.Collision_Occurred := False;
                     E2_CP.Collision_Occurred := False;
                  end if;
               end;
            end loop;
            -- TODO: Check for screen bound collisions here
         end;
      end loop;
   end Execute;
end ECS.System;