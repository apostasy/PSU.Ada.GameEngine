with ECS.System; use ECS.System;

package ECS.System.Collision is 

   type Collision_T is new System_T with record
      Width : Natural;
      Height : Natural;
   end record;

   overriding
   procedure Execute ( Self : in out Collision_T;
                       Dt   : Duration;
                       Manager    : access Entity_Manager_T'Class );
end ECS.System.Collision;