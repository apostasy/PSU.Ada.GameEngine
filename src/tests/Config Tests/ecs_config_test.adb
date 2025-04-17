with Ada.Text_IO; use Ada.Text_IO;
with ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with ECS.Config_Loader; use ECS.Config_Loader;

procedure ECS_Config_Test is
   Manager : Entity_Manager_T;

   procedure Print_Entity(Entity : access ECS.Entity.Entity_T'Class) is
      Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
      RigidBody_Comp : constant Component_Access := Entity.Get_Component(Rigidbody_T'Tag);
      AABB_Comp      : constant Component_Access := Entity.Get_Component(AABB_T'Tag);
      Collision_Comp : constant Component_Access := Entity.Get_Component(Collision_Params_T'Tag);
   begin
      Put_Line("Entity ID: " & Entity.Id);
      if Transform_Comp /= null then
         declare
            T : Transform_T renames Transform_T(Transform_Comp.all);
         begin
            Put_Line("  Transform:");
            Put_Line("    Position: (" & Float'Image(T.Position.X) & ", " & Float'Image(T.Position.Y) & ")");
            Put_Line("    Velocity: (" & Float'Image(T.Velocity.X) & ", " & Float'Image(T.Velocity.Y) & ")");
            Put_Line("    Rotation: " & Float'Image(T.Rotation));
         end;
      else
         Put_Line("  No Transform component");
      end if;

      if RigidBody_Comp /= null then
         declare
            R : Rigidbody_T renames Rigidbody_T(RigidBody_Comp.all);
         begin
            Put_Line("  RigidBody:");
            Put_Line("    Mass: " & Float'Image(R.Mass));
         end;
      else
         Put_Line("  No RigidBody component");
      end if;

      if AABB_Comp /= null then
         declare
            B : AABB_T renames AABB_T(AABB_Comp.all);
         begin
            Put_Line("  AABB:");
            Put_Line("    Left: " & Float'Image(B.Left));
            Put_Line("    Bottom: " & Float'Image(B.Bottom));
            Put_Line("    Right: " & Float'Image(B.Right));
            Put_Line("    Top: " & Float'Image(B.Top));
         end;
      else
         Put_Line("  No AABB component");
      end if;

      if Collision_Comp /= null then
         declare
            C : Collision_Params_T renames Collision_Params_T(Collision_Comp.all);
         begin
            Put_Line("  Collision_Params:");
            Put_Line("    Collision_Enabled: " & Boolean'Image(C.Collision_Enabled));
            Put_Line("    Destroy_On_Collision: " & Boolean'Image(C.Destroy_On_Collision));
            Put_Line("    Collision_Occurred: " & Boolean'Image(C.Collision_Occurred));
            Put_Line("    Wall_Collision: " & Boolean'Image(C.Wall_Collision));
         end;
      else
         Put_Line("  No Collision_Params component");
      end if;
      New_Line;
   end Print_Entity;

begin
   -- Load configuration from INI file
   Load_Config(Manager, "C:\Users\zai\Documents\SPRING 2025\milestone project\Active Group Git\PSU.Ada.GameEngine\src\tests\Config Tests\entities.ini");

   -- Update the manager to process any pending entities
   Manager.Update;

   -- Print out the components of each entity
   for Entity of Manager.Entities loop
      Print_Entity(Entity);
   end loop;
end ECS_Config_Test;