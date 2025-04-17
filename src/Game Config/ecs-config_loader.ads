with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ECS.Entity; use ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with ECS.Vec2; use ECS.Vec2;
with Graphics.Color; use Graphics.Color;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Collision;    use ECS.System.Collision;

package ECS.Config_Loader is

   procedure Load_Config(Manager : in out ECS.Entity_Manager.Entity_Manager_T; File_Name : String);

end ECS.Config_Loader;