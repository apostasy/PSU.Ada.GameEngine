with ecs.entity; use ecs.entity;
package body ecs.entity_manager is

  Entity_Count : Positive := 1;

  function AddEntity (Manager : in out Entity_Manager_T; Entity_Type : Id_T) return Entity_Access is
  Entity : Entity_Access := new Entity_T'(
                Count => Entity_Count,
                Id => Entity_Type,
                Destroyed => False,
                Components => Component_List.Empty_Vector
                );
  begin
    Manager.ToBeAdded.Append(Entity);
    Entity_Count := Entity_Count + 1;
    return Entity;
  end AddEntity;

  procedure Update(Manager : in out Entity_Manager_T) is
  -- Copy newly added Entities to the Entity list
  begin
    for E of Manager.ToBeAdded loop
      Manager.Entities.Append(E);
    end loop;
    Manager.ToBeAdded.Clear;
  -- Remove destroyed Entities
    for I in reverse Manager.Entities.First_Index .. Manager.Entities.Last_Index loop
      if Manager.Entities(I).all.Destroyed then
         Free_Components (Manager.Entities(I).all);
         Free_Entity (Manager.Entities(I));
        
         Manager.Entities.Delete(I);
      end if;
    end loop;
  end Update;
    
   function GetEntity(Manager : in out Entity_Manager_T; Entity_Type : Id_T) return Entity_Access is
   begin
      for E of Manager.Entities loop
         if E.all.Id = Entity_Type then
            return E;
         end if;
      end loop;
      return null;
   end GetEntity;

end ecs.entity_manager;