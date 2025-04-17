

package body ECS.System.Collision is
   -- Checks all entities for collisions 
   procedure Execute ( Self      : in out Collision_T;
                       Dt        : Duration;
                       Manager   : access Entity_Manager_T'Class ) is

         Length : constant Natural := Natural(Manager.all.Entities.Length);
    
          -- Helper to check if two entities are colliding
         function Entity_Collision (A : access Entity_T'Class; B : access Entity_T'Class) return Boolean is
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
         end Entity_Collision;
   begin
      -- Pairwise checking of entities for collision
      for I in 0 .. Length - 1 loop
         declare
            E_1 : Entity_Access := Manager.all.Entities.Element(I);
            E1_Collision_Params : Component_Access := E_1.all.Get_Component(Collision_Params_T'Tag);
            E1_CP renames Collision_Params_T (E1_Collision_Params.all);  
         begin
            for J in I + 1 .. Length - 1 loop
               declare
                  E_2 : Entity_Access := Manager.all.Entities.Element(J);
                  E2_Collision_Params : Component_Access := E_2.all.Get_Component(Collision_Params_T'Tag);
                  E2_CP renames Collision_Params_T (E2_Collision_Params.all);         
               begin
                  if Entity_Collision(E_1,E_2) then
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
         end;
      end loop;
   end Execute;
end ECS.System.Collision;