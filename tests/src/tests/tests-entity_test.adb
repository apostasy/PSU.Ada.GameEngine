with Ada.Text_IO; use Ada.Text_IO;

with ECS.Component; use ECS.Component;
with ECS.Entity;    use ECS.Entity;

with Test_Logger;           use Test_Logger;

with Ada.Containers; use Ada.Containers;

package body Tests.Entity_Test is

   procedure Test is

      -- This package is a test for the Entity module.
      -- It creates an entity with components and prints the components to the console.

      -- Declare a variable to hold the entity
      -- Entity : Entity_Access;

      Entity : Entity_Access :=
        new Entity_T'
          (Count      => 1,
           Id         => "TestE",
           Destroyed  => False,
           Components => Component_List.Empty_Vector);

      Transform_P : Component_Access :=
        new Transform_T'
          (Position => (X => 50.0, Y => 150.0),
           Velocity => (X => 0.0, Y => 0.0),
           Rotation => 0.0);

      Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 1.0);

      Indent : String := "    ";
      Separator : String := "===============================================";

   begin
      Put_Line ("Entity Test");
      Put_Line (Separator);

      Entity.all.Add_Component (Transform_P);
      Entity.all.Add_Component (Rigidbody_P);

      Log_Test ("Components Added",
                Entity.all.Components.Length = 2,
                "2",
                Entity.all.Components.Length'Image);

      Put_Line (Separator);  
   end Test;

end Tests.Entity_Test;
