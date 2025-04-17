with ECS.System; use ECS.System;
package ECS.System.User_Input is

   type User_Input_T is new System_T with record
      Player_Entity : Entity_Access;
      Handler       : Platform_Event_Handler_Access;
      MouseDown     : Boolean := False;
      MouseUp       : Boolean := True;
   end record;

   overriding
   procedure Execute (Self : in out User_Input_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class );


   type Input_Callback is access procedure (Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
   
   Key_Callbacks : array (0 .. 255) of Input_Callback := (others => null);
   Mouse_Callbacks : array (16#200# .. 16#202#) of Input_Callback := (others => null);
   procedure Register_Key_Callback (Key : Integer; Callback : Input_Callback);
   procedure Register_Mouse_Callback (Key : Integer; Callback : Input_Callback);
   Started : Boolean := False;

end ECS.System.User_Input;