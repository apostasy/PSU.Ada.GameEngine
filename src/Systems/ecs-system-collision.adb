

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

         function Wall_Collision(E : access Entity_T'Class) return Boolean is
            Bounding_Box_E : Component_Access := E.all.Get_Component(AABB_T'Tag);
         begin
            if Bounding_Box_E = null then
               return False;
            end if;
            declare
               BB_E : AABB_T renames AABB_T(Bounding_Box_E.all);
               Left_Wall : Boolean := BB_E.Left <= 2.0;
               Top_Wall : Boolean := BB_E.Top <= 2.0;
               Right_Wall : Boolean := BB_E.Right >= Float(Self.Width) - 2.0;
               Bottom_Wall : Boolean := BB_E.Bottom >= Float(Self.Height) - 2.0;
            begin
               return Left_Wall or Top_Wall or Bottom_Wall or Right_Wall;
            end;
         end Wall_Collision;
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

                        if (E_1.all.Id = "Playr" and E_2.all.Id = "WarpZ") then
                           E2_CP.Collision_Occurred := True;
                           return;
                           --  goto Continue;
                        elsif (E_1.all.Id = "WarpZ" and E_2.all.Id = "Playr") then
                           E1_CP.Collision_Occurred := True;
                           return;
                           --  goto Continue;
                        end if;

                        E1_CP.Collision_Occurred := True;
                        E2_CP.Collision_Occurred := True;
                        -- Flag the entity to be removed if set to be destroyed on collision
                        E_1.all.Destroyed := E1_CP.Destroy_On_Collision;
                        E_2.all.Destroyed := E2_CP.Destroy_On_Collision;
                        if (E_1.all.Id = "B0001" and E_2.all.id = "Enemy") or
                           (E_1.all.Id = "Enemy" and E_2.all.id = "B0001") then
                           Score := Score + 10;
                        end if;

                        if ((E_1.all.Id = "Playr" or E_2.all.id = "Playr") and 
                           (E_1.all.Id = "Enemy" or E_2.all.Id = "Enemy")) then
                           GameOver := True;
                        end if;

                     end if;
                  else
                     E1_CP.Collision_Occurred := False;
                     E2_CP.Collision_Occurred := False;
                  end if;
               end;
               <<Continue>>
            end loop;            
            if E1_CP.Wall_Collision and then Wall_Collision (E_1) then
               if E1_CP.Collision_Enabled then
                  E1_CP.Collision_Occurred := True;
                  E_1.all.Destroyed := E1_CP.Destroy_On_Collision;
               end if; 
            else
               E1_CP.Collision_Occurred := False;
            end if;
         end;
      end loop;
   end Execute;
end ECS.System.Collision;