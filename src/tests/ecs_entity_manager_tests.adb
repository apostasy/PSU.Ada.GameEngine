with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Entity; use ECS.Entity;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Logger; use Test_Logger;

procedure ECS_Entity_Manager_Tests is

   -- Test cases
   procedure Test_AddEntity is
      Manager : Manager_Access := new Entity_Manager_T'(
         Entities  => Entity_List.Empty_Vector,
         ToBeAdded => Entity_List.Empty_Vector
      );
      Player : Entity_Access := Manager.all.AddEntity("Playr");
      Length : Natural;
   begin
      Length := Natural(Manager.ToBeAdded.Length);
      Log_Test("AddEntity ToBeAdded Length", Length = 1, "1", Natural'Image(Length));
      Log_Test("AddEntity Player ID", Player.Id = "Playr", "Playr", Player.Id);
   end Test_AddEntity;

   procedure Test_Update_AddEntities is
      Manager : Manager_Access := new Entity_Manager_T'(
         Entities  => Entity_List.Empty_Vector,
         ToBeAdded => Entity_List.Empty_Vector
      );
      Player : Entity_Access := Manager.all.AddEntity("Playr");
      E1 : Entity_Access := Manager.all.AddEntity("E0001");
      
      
   begin
      declare
      Length_ToBeAdded : Natural := Natural(Manager.ToBeAdded.Length);
      begin
         Log_Test("Update Pre-Update ToBeAdded Length", Length_ToBeAdded = 2, "2", Natural'Image(Length_ToBeAdded));
      end;
      Manager.Update;
      declare
      Length_Entities  : Natural := Natural(Manager.Entities.Length);
      Length_ToBeAdded : Natural := Natural(Manager.ToBeAdded.Length);
      begin
         Log_Test("Update Entities Length After Update", Length_Entities = 2, "2", Natural'Image(Length_Entities));
         Log_Test("Update ToBeAdded Length After Update", Length_ToBeAdded = 0, "0", Natural'Image(Natural(Length_ToBeAdded)));
      end;
   end Test_Update_AddEntities;

   procedure Test_Update_RemoveDestroyed is
      Manager : Manager_Access := new Entity_Manager_T'(
         Entities  => Entity_List.Empty_Vector,
         ToBeAdded => Entity_List.Empty_Vector
      );
      Player : Entity_Access := Manager.all.AddEntity("Playr");
      E1 : Entity_Access := Manager.all.AddEntity("E0001");
      E2 : Entity_Access := Manager.all.AddEntity("E0002");

      Length_Before_Remove : Natural;
      Length_After_Remove  : Natural;
   begin
      Manager.Update; -- Ensure all entities are added to `Entities`

      -- Flag one entity as destroyed
      E2.all.Destroyed := True;

      Length_Before_Remove := Natural(Manager.Entities.Length);
      Log_Test("RemoveDestroyed Length Before Remove", Length_Before_Remove = 3, "3", Natural'Image(Length_Before_Remove));

      Manager.Update; -- Run update to remove destroyed entities

      Length_After_Remove := Natural(Manager.Entities.Length);
      Log_Test("RemoveDestroyed Length After Remove", Length_After_Remove = 2, "2", Natural'Image(Length_After_Remove));
      Log_Test("RemoveDestroyed E2 Removed", not Manager.Entities.Contains(E2), "False", "True");
   end Test_Update_RemoveDestroyed;

begin
   Put_Line("Running ECS.Entity_Manager Tests...");

   -- Run all tests
   Test_AddEntity;
   Test_Update_AddEntities;
   Test_Update_RemoveDestroyed;

end ECS_Entity_Manager_Tests;
