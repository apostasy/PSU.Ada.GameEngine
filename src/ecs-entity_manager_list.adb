with ecs.entity; use ecs.entity;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
package body ecs.Entity_Manager_List is

  Manager_Count : Positive := 1;

  procedure AddManager (Manager_List : in out Entity_Manager_List_T; Manager : in out Manager_Access) is  
  begin
    Manager_List.Managers.Append(Manager);
    Manager_Count := Manager_Count + 1;
  end AddManager;
  
  procedure SetManager (Manager_List : in out Entity_Manager_List_T; Manager : in Unbounded_String) is
  begin
    for M of Manager_List.Managers loop
      if M.all.ID = Manager then
        Manager_List.Current_Manager := M;
        return;
      end if;
    end loop;
  end SetManager;

end ecs.Entity_Manager_List;