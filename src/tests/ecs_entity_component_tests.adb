with ECS.Entity; use ECS.Entity;
with ECS.Component; use ECS.Component;
with Test_Logger; use Test_Logger;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Tags; use Ada.Tags;

procedure ECS_Entity_Component_Tests is

   -- Test cases
   procedure Test_Entity_Creation is
      Entity_Count : constant Positive := 1;
      Entity_Type  : constant Id_T := "E0001";
      Entity : Entity_Access := new Entity_T'(
         Count      => Entity_Count,
         Id         => Entity_Type,
         Destroyed  => False,
         Components => Component_List.Empty_Vector
      );
      Length : Natural := Natural(Entity.Components.Length);
   begin
      Log_Test("Entity ID", Entity.Id = "E0001", "E0001", Entity.Id);
      Log_Test("Entity Destroyed Flag", not Entity.Destroyed, "False", Boolean'Image(Entity.Destroyed));
      Log_Test("Entity Components Empty", Length = 0, "0", Natural'Image(Length));
   end Test_Entity_Creation;

   procedure Test_Add_Component is
      Entity_Count : constant Positive := 1;
      Entity_Type  : constant Id_T := "E0002";
      Entity : Entity_Access := new Entity_T'(
         Count      => Entity_Count,
         Id         => Entity_Type,
         Destroyed  => False,
         Components => Component_List.Empty_Vector
      );
      Transform_P : Component_Access := new Transform_T'(
         Position => (X => 1.0, Y => 2.0),
         Velocity => (X => 2.0, Y => 0.0),
         Rotation => 0.0
      );
   begin
      Add_Component(Entity.all, Transform_P);
      declare
      Length : Natural := Natural(Entity.Components.Length);
      begin
         Log_Test("Add Component Length", Length = 1, "1", Natural'Image(Length));
         Log_Test("Add Component Tag", Entity.Components(0).all'Tag = Transform_P'Tag, Transform_P'Tag'Image, Entity.Components(0).all'Tag'Image);
      end;
   end Test_Add_Component;

   procedure Test_Get_Component is
      Entity_Count : constant Positive := 1;
      Entity_Type  : constant Id_T := "E0003";
      Entity : Entity_Access := new Entity_T'(
         Count      => Entity_Count,
         Id         => Entity_Type,
         Destroyed  => False,
         Components => Component_List.Empty_Vector
      );
      Transform_P : Component_Access := new Transform_T'(
         Position => (X => 1.0, Y => 2.0),
         Velocity => (X => 2.0, Y => 0.0),
         Rotation => 0.0
      );
      Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 10.0);
      Trans : Component_Access;
   begin
      Add_Component(Entity.all, Transform_P);
      Add_Component(Entity.all, Rigidbody_P);

      -- Retrieve the Transform component by its tag
      Trans := Entity.all.Get_Component(Transform_T'Tag);

      Log_Test("Get Component Found", Trans /= null, "Not null", "null");

      if Trans /= null then
         Log_Test("Get Component Tag", Trans'Tag = Transform_P'Tag, 
                  Transform_P'Tag'Image, Trans'Tag'Image);
      end if;
   end Test_Get_Component;

begin
   Put_Line("Running ECS.Entity and ECS.Component Tests...");

   -- Run all tests
   Test_Entity_Creation;
   Test_Add_Component;
   Test_Get_Component;

end ECS_Entity_Component_Tests;
