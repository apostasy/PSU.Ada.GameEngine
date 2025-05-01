package body ECS.System.Audio is
   
   procedure Execute (Self    : in out Audio_T;
                     Dt       : Duration;
                     Manager  : access Entity_Manager_T'Class) is



   begin
      for Entity of Manager.all.Entities loop
         declare
            Audio : Component_Access := Entity.all.Get_Component (Audio_Component_T'Tag);

         begin
            if Audio /= null then

               declare
                  Audio_C renames Audio_Component_T(Audio.all);
      
               begin

                  if Audio_C.Sound_Triggered then
                     Play_Audio(To_String(Audio_C.File_Path));
                     Audio_C.Sound_Triggered := False;
                  end if;

               end;

            end if;

         end;
         
      end loop;

   end Execute;

end ECS.System.Audio;