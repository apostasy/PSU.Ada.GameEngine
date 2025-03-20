with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body ECS.System.Render is 
   -- Draw the entity to the screen
   procedure Execute (Self       : in out Render_T;
                      Dt         : Duration;
                      Manager    : access Entity_Manager_T'Class ) is
   begin
      for Entity of Manager.all.Entities loop
         declare
            Trans       : Component_Access   :=    Entity.all.Get_Component(Transform_T'Tag);
            Circle      : Component_Access   :=    Entity.all.Get_Component(Circle_T'Tag);
            Quad        : Component_Access   :=    Entity.all.Get_Component(Quad_T'Tag);
            Text        : Component_Access   :=    Entity.all.Get_Component(Text_T'Tag);
            Texture     : Component_Access   :=    Entity.all.Get_Component(Texture_T'Tag);
            Animation   : Component_Access   :=    Entity.all.Get_Component(Animation_Component_T'Tag);
         begin
            if Trans = null and Circle = null and Quad = null then
               Put_Line("Entity missing essential components");
               return;
            end if;
            if Trans /= null and Circle /= null then
               declare
                  T renames Transform_T(Trans.all);
                  C renames Circle_T(Circle.all);
               begin
                  Draw_Regular_Polygon(Self.Buffer.all, C.Sides, C.Radius, T.Position.X,T.Position.Y, C.C, Self.Width, Self.Height);
               end;
            elsif Trans /= null and Texture /= null then
               declare
                  T renames Transform_T(Trans.all);
                  Tx renames Texture_T(Texture.all);
                  Q renames Quad_T(Quad.all);
               begin
                  if Animation /= null then
                  declare
                     A renames Animation_Component_T(Animation.all);
                  begin
                     Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X), Integer(T.Position.Y), Integer(Q.Width), Integer(Q.Height), A.CurX,A.CurY, Self.Width, Self.Height,Natural(Tx.Width));
                  end;
                  else
                     Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X), Integer(T.Position.Y), Tx.Width, Tx.Height, Self.Width, Self.Height);
                  end if;
               end;
            elsif Trans /= null and Quad /= null then
               declare
                  T renames Transform_T(Trans.all);
                  Q renames Quad_T(Quad.all);
               begin
                  Draw_Filled_Quad (Self.Buffer.all,T.Position.X, T.Position.Y, Q.Width, Q.Height, Q.C,Self.Width, Self.Height);
                  if Text /= null then
                     declare
                        Txt renames Text_T(Text.all);
                     begin
                        Draw_String (Self.Buffer.all, Integer(T.Position.X), Integer(T.Position.Y), 0, 0, To_String(Txt.Text), Txt.C, Self.Width,Self.Height);
                     end;
                  end if;
               end;
            else
               Put_Line("Entity missing essential components");
               return;
            end if;
         end;
      end loop; 
   end Execute;
end ECS.System.Render;