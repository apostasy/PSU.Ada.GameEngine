with Ada.Text_IO; use Ada.Text_IO;
with ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with ECS.Config_Loader; use ECS.Config_Loader;
with Window; use Window;
with Win32; use Win32;
with Graphics.Renderer; use Graphics.Renderer;
with Ada.Real_Time; use Ada.Real_Time;
with Graphics.Color; use Graphics.Color;
with ecs.System.Render; use ecs.System.Render;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Collision;    use ECS.System.Collision;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure ECS_Config_Window_System_Test is
   Manager : aliased Entity_Manager_T;

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

   procedure Render_Entities(Manager : access Entity_Manager_T) is
      Width  : constant Integer := 800;
      Height : constant Integer := 600;
      Title  : Unbounded_String := To_Unbounded_String("Game Window");
      GameWindow : Window_Access;
      Buffer : Win32.Byte_Array_Access := new Win32.Byte_Array(0 .. Width * Height * 4);
      SkyBlue : Color := (R => 135, G => 206, B => 236, A => 255);
      Start_Time, Stop_Time : Time;
      Elapsed_Time : Time_Span;
      Render : Render_T := (Width, Height, Buffer);
      Mover : Mover_T := (Width, Height); -- Initialize the Movement system
      Collider : Collision_T := (Width, Height); -- Initialize the Collision system
      Has_Msg : Boolean := True;
      Message : MSG_Access := new MSG;
      Lp_Result : LRESULT;
   begin
      GameWindow := New_Window(IC.int(Width), IC.int(Height), Title);
      Put_Line("Start Engine");

      -- Initialize Start_Time before the loop
      Start_Time := Clock;

      while Has_Msg loop
         Stop_Time := Clock;
         Elapsed_Time := Stop_Time - Start_Time;
         Start_Time := Stop_Time;
         Lp_Result := Dispatch_Message(Message);
         Has_Msg := Get_Message(Message, System.Null_Address, 0, 0);

            -- Debugging output to check for missing components
         for Entity of Manager.Entities loop
            if Entity.Get_Component(Transform_T'Tag) = null then
               Put_Line("Entity ID:" & Entity.Id & " is missing Transform component");
            end if;
            if Entity.Get_Component(Rigidbody_T'Tag) = null then
               Put_Line("Entity ID:" & Entity.Id & " is missing RigidBody component");
            end if;
            if Entity.Get_Component(AABB_T'Tag) = null then
               Put_Line("Entity ID:" & Entity.Id & " is missing AABB component");
            end if;
            if Entity.Get_Component(Collision_Params_T'Tag) = null then
               Put_Line("Entity ID:" & Entity.Id & " is missing Collision_Params component");
            end if;
            -- Add checks for other required components as needed
         end loop;

         -- Update the Movement system
         Mover.Execute(To_Duration(Elapsed_Time), Manager);

         -- Update the Collision system
         Collider.Execute(To_Duration(Elapsed_Time), Manager);

         Manager.Update;
         Clear_Screen(Buffer.all, Graphics.Color.Blue, Width, Height);
         Render.Execute(To_Duration(Elapsed_Time), Manager);
         Draw_Buffer(Buffer.all'Address);
      end loop;
   end Render_Entities;

begin
   -- Load configuration from INI file
   Load_Config(Manager, "C:\Users\zai\Documents\SPRING 2025\milestone project\Active Group Git\PSU.Ada.GameEngine\src\tests\Config Tests\entities.ini");

   -- Update the manager to process any pending entities
   Manager.Update;

   -- Print out the components of each entity
   for Entity of Manager.Entities loop
      Print_Entity(Entity);
   end loop;


   -- Render the entities to the window
   Render_Entities(Manager'Access);

end ECS_Config_Window_System_Test;