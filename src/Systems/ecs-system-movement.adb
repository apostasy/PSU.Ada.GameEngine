package body ECS.System.Movement is
   procedure Execute ( Self      : in out Mover_T;
                       Dt        : Duration; 
                       Manager   : access Entity_Manager_T'Class ) is
   begin
      for Entity of Manager.all.Entities loop
         declare
            Trans       : Component_Access   :=    Entity.all.Get_Component (Transform_T'Tag);
            Rigidbodies : Component_Access   :=    Entity.all.Get_Component (Rigidbody_T'Tag);
            AABB        : Component_Access   :=    Entity.all.Get_Component (AABB_T'Tag);
            Quad        : Component_Access   :=    Entity.all.Get_Component (Quad_T'Tag);
            Collision   : Component_Access   :=    Entity.all.Get_Component (Collision_Params_T'Tag);
            -- TODO: Add proper support for circle shaped objects
            
            begin
               if Trans = null then
                  Put_Line ("No Transform on entity " & Entity.all.Id);
                  return;
               end if;
               if Rigidbodies = null then
                  Put_Line ("No Rigidbodies on entity " & Entity.all.Id);
                  return;
               end if;
               if AABB = null then 
                  Put_Line ("No AABB on entity " & Entity.all.Id);
                  return;
               end if;
               if Quad = null then 
                  Put_Line ("No Quad on entity " & Entity.all.Id);
                  return;
               end if;

               if Collision = null then
                  Put_Line ("No Collision Parameters on entity " & Entity.all.Id);
                  return;
               end if;

            declare
               T renames Transform_T (Trans.all);
               R renames Rigidbody_T (Rigidbodies.all);
               B renames AABB_T(AABB.all);
               Q renames Quad_T(Quad.all);
               C renames Collision_Params_T(Collision.all);
               Velocity_Scaled : ECS.Vec2.Vec2 := New_Vec2(X_In => T.Velocity.X, Y_In => T.Velocity.Y);
               begin
                  Scale(Velocity_Scaled, Float(Dt));
                  -- Update the entity position while maintaining the position within screen bounds
                  T.Position.X := T.Position.X + Velocity_Scaled.X;
                  --  if T.Position.X >= Float(Self.Width) then
                  --     T.Position.X := T.Position.X - Float(Self.Width);
                  --  elsif T.Position.X < 0.0 then 
                  --     T.Position.X := T.Position.X + Float(Self.Width);
                  --  end if;

                  T.Position.Y := T.Position.Y + Velocity_Scaled.Y;
                  --  if T.Position.Y >= Float(Self.Height) then
                  --     T.Position.Y := T.Position.Y - Float(Self.Height);
                  --  elsif T.Position.Y < 0.0 then
                  --     T.Position.Y := T.Position.Y + Float(Self.Height);
                  --  end if;

                  -- Sync bounding box
                  B.Left := T.Position.X;
                  B.Right := T.Position.X + Q.Width;
                  B.Top := T.Position.Y;
                  B.Bottom := T.Position.Y + Q.Height;
               end;
            end;
      end loop;   
   end Execute;
end ECS.System.Movement;