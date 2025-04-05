with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with ecs.entity; use ecs.entity;
with ecs.entity_manager; use ecs.entity_manager;

with Ada.Containers.Vectors;

package ECS.Entity_Manager_List is

   package Entity_Manager_List is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Manager_Access);
   use Entity_Manager_List;

   type Entity_Manager_List_T is tagged record
      Managers : Entity_Manager_List.Vector;
   Current_Manager : Manager_Access;
   end record;

   type Manager_List_Access is access all Entity_Manager_List_T'Class;
   procedure AddManager (Manager_List : in out Entity_Manager_List_T; Manager : in out Manager_Access);
   procedure SetManager (Manager_List : in out Entity_Manager_List_T; Manager : in Unbounded_String);
   procedure Free_Manager_List is new Ada.Unchecked_Deallocation(Entity_Manager_List_T'Class, Manager_List_Access);

end ECS.Entity_Manager_List;