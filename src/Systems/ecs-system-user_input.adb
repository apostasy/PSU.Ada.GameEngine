with ecs;
with GameMath; use GameMath;
package body ECS.System.User_Input is 

   procedure Register_Key_Callback(Key : Integer; Callback : Input_Callback) is
   begin
      Key_Callbacks(Key) := Callback;
   end Register_Key_Callback;

   procedure Register_Mouse_Callback (Key : Integer; Callback : Input_Callback) is
   begin
      Mouse_Callbacks(Key) := Callback;
   end Register_Mouse_Callback;

   overriding procedure Execute ( Self      : in out User_Input_T;
                       Dt        : Duration;
                       Manager   : access Entity_Manager_T'Class) is
   Trans    : Component_Access := Self.Player_Entity.all.Get_Component(Transform_T'Tag);
   T renames Transform_T(Trans.all);
   Event : Event_T := Get_Next_Event(Self.Handler.all);
   begin
      case Event.EventType is
         when ECS.Event.KeyDown =>
            if Key_Callbacks(Integer(Event.Data.KeyCode)) /= null then
               Key_Callbacks(Integer(Event.Data.KeyCode)).all(Manager, Dt, True);
            end if;
         when ECS.Event.KeyUp =>
            if Key_Callbacks(Integer(Event.Data.KeyCode)) /= null then
               Key_Callbacks(Integer(Event.Data.KeyCode)).all(Manager, Dt, False);
            end if;
         when ECS.Event.L_MouseDown =>  
            if Mouse_Callbacks(16#201#) /= null then
               Mouse_Callbacks(16#201#).all(Manager, Dt, True);
            end if;
         when ECS.Event.L_MouseUp =>
            if Mouse_Callbacks(16#202#) /= null then
               Mouse_Callbacks(16#202#).all(Manager, Dt, False);
            end if;
         when ECS.Event.MouseMove =>
            MousePos.PreviousPos := MousePos.CurrentPos;
            MousePos.CurrentPos := (Float(Event.Data.MouseX), Float(Event.Data.MouseY));
            if Mouse_Callbacks(16#200#) /= null then
               Mouse_Callbacks(16#200#).all(Manager, Dt, False);
            end if;
         when others =>
            null;
      end case;
   end Execute;
end ECS.System.User_Input;