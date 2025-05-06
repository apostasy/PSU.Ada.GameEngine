with ECS.System; use ECS.System;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;



package ECS.System.Audio is
   

   type Audio_T is new System_T with null record;

   overriding
   procedure Execute (Self : in out Audio_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class);

end ECS.System.Audio;