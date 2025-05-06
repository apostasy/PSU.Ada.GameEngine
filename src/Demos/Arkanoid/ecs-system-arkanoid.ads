package ECS.System.Arkanoid is
   Score : Integer := 0;
   type Arkanoid_T is new System_T with null record;

   overriding
   procedure Execute (Self : in out Arkanoid_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class);

end ECS.System.Arkanoid;