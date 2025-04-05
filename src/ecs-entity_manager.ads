with Ada.Unchecked_Deallocation;
with ecs.entity; use ecs.entity;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Vectors;

package ECS.Entity_Manager is

   package Entity_List is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Entity_Access);
   use Entity_List;

   type Entity_Manager_T is tagged record
      ID: Unbounded_String;
      Entities : Entity_List.Vector;
      ToBeAdded : Entity_List.Vector;
   end record;

   type Manager_Access is access all Entity_Manager_T'Class;
   function AddEntity (Manager : in out Entity_Manager_T; Entity_Type : Id_T) return Entity_Access;
   procedure AddEntity (Manager : in out Entity_Manager_T; Entity : Entity_Access);
   function GetEntity (Manager : in out Entity_Manager_T; Entity_Type : Id_T) return Entity_Access;
   procedure Update (Manager : in out Entity_Manager_T);
   procedure Free_Manager is new Ada.Unchecked_Deallocation(Entity_Manager_T'Class, Manager_Access);

end ECS.Entity_Manager;