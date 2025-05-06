with ECS.System; use ECS.System;


package ECS.System.Enemy_Spawner is
   

   type Enemy_Spawn_T is new System_T with null record;

   overriding
   procedure Execute (Self : in out Enemy_Spawn_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class);

end ECS.System.Enemy_Spawner;