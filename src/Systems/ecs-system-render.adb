with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body ECS.System.Render is 

   Use_Camera     : Boolean := False;
   Camera         : Component_Access := null;
   
   -- Wrapper procedures to contain data initialization before calling the internal drawing procedures
   procedure Draw_Circle(Self : in out Render_T; Transform, Circle : Component_Access) is   
      T renames Transform_T(Transform.all);
      C renames Circle_T(Circle.all);
   begin
      Draw_Regular_Polygon(Self.Buffer.all, C.Sides, C.Radius, T.Position.X,T.Position.Y, C.C, Self.Width, Self.Height);
   end Draw_Circle;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Animated_Texture(Self : in out Render_T; Transform, Animation, Quad : Component_Access) is
      T renames Transform_T(Transform.all);
      Q renames Quad_T(Quad.all);
      AC renames Animation_Component_T(Animation.all);
      A : Single_Animation_Access := AC.Animations(AC.Current);
      Tx : Texture_Access := AC.Textures(AC.Current);
   begin

      if Use_Camera then
         declare 
            Camera_Component : Camera_Component_T renames Camera_Component_T(Camera.all);
         begin
         -- Draw the animated texture using the camera's position and dimensions
         Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X - Camera_Component.Position.X), Integer(T.Position.Y - Camera_Component.Position.Y), Integer(Q.Width), Integer(Q.Height), A.CurX,A.CurY, Self.Width, Self.Height,Natural(Tx.Width));
         end;
      else
         -- Draw the animated texture without camera adjustments
         Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X), Integer(T.Position.Y), Integer(Q.Width), Integer(Q.Height), A.CurX,A.CurY, Self.Width, Self.Height,Natural(Tx.Width));
      end if;
   end Draw_Animated_Texture;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Static_Texture(Self : in out Render_T; Transform, Texture, Quad : Component_Access) is
      T renames Transform_T(Transform.all);
      Q renames Quad_T(Quad.all);
      Tx renames Texture_T(Texture.all);
   begin

      if Use_Camera then
         declare 
            Camera_Component : Camera_Component_T renames Camera_Component_T(Camera.all);
         begin
         -- Draw the static texture using the camera's position and dimensions
         Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X - Camera_Component.Position.X), Integer(T.Position.Y - Camera_Component.Position.Y), Tx.Width, Tx.Height, Self.Width, Self.Height);
         end;
      else
         -- Draw the static texture without camera adjustments
         Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X), Integer(T.Position.Y), Tx.Width, Tx.Height, Self.Width, Self.Height);
      end if;
   end Draw_Static_Texture;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Rectangle(Self : in out Render_T; Transform, Quad : Component_Access) is
      T renames Transform_T(Transform.all);
      Q renames Quad_T(Quad.all);
   begin
      Draw_Filled_Quad (Self.Buffer.all,T.Position.X, T.Position.Y, Q.Width, Q.Height, Q.C,Self.Width, Self.Height);
   end Draw_Rectangle;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Text(Self : in out Render_T; Transform, Text : Component_Access) is
      T renames Transform_T(Transform.all);
      Txt renames Text_T(Text.all);
   begin
      Draw_String (Self.Buffer.all, Integer(T.Position.X), Integer(T.Position.Y), 0, 0, To_String(Txt.Text), Txt.C, Self.Width,Self.Height);
   end Draw_Text;
-------------------------------------------------------------------------------------------------------------------------------------------------------------

   function Is_Visible
     (Entity_Position          : Vec2;
      Entity_Width, Entity_Height : Float;
      Camera : Camera_Component_T) return Boolean is
      Camera_X      : constant Float := Camera.Position.X;
      Camera_Y      : constant Float := Camera.Position.Y;
      Camera_Width  : constant Float := Float(Camera.Width);
      Camera_Height : constant Float := Float(Camera.Height);
      Half_Camera_Width  : constant Float := Camera_Width / 2.0;
      Half_Camera_Height : constant Float := Camera_Height / 2.0;
      Object_Left   : constant Float := Entity_Position.X - Camera_X + Half_Camera_Width;
      Object_Right  : constant Float := Object_Left + Entity_Width;
      Object_Top    : constant Float := Entity_Position.Y - Camera_Y + Half_Camera_Height;
      Object_Bottom : constant Float := Object_Top + Entity_Height;
   begin
      return Entity_Position.X >= 0.0 and Entity_Position.X <= (Camera_X + Camera_Width) and
             Entity_Position.Y >= 0.0 and Entity_Position.Y <= (Camera_Y + Camera_Height);
   end Is_Visible;


-------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Calls the drawing function for each entity depending on the attached components
   procedure Execute (Self       : in out Render_T;
                      Dt         : Duration;
                      Manager    : access Entity_Manager_T'Class ) is

      -- Camera-related variables
      --  Camera_X       : Float := 0.0;
      --  Camera_Y       : Float := 0.0;
      --  Camera_Width   : Float := Float(Self.Width);
      --  Camera_Height  : Float := Float(Self.Height);

   begin
         
      -- First pass: Find camera entity if it exists
      for Entity of Manager.all.Entities loop
         declare
            Entity_Camera     : Component_Access := Entity.all.Get_Component(Camera_Component_T'Tag);
            Transform  : Component_Access := Entity.all.Get_Component(Transform_T'Tag);
         begin
            if Entity_Camera /= null and Transform /= null then
               Use_Camera := True;
               Camera := Entity_Camera;
               exit; -- Use the first camera found
            end if;
         end;
      end loop;

      if Use_Camera then
         declare

            Camera_Component : Camera_Component_T renames Camera_Component_T(Camera.all);
            Camera_X      : constant Float := Camera_Component.Position.X;
            Camera_Y      : constant Float := Camera_Component.Position.Y;
            Camera_Width  : constant Float := Float(Camera_Component.Width);
            Camera_Height : constant Float := Float(Camera_Component.Height);
            Half_Camera_Width  : constant Float := Camera_Width / 2.0;
            Half_Camera_Height : constant Float := Camera_Height / 2.0;
         
         begin
            null;
         --  Draw_Regular_Polygon (Self.Buffer.all, 4, Positive(Camera_Component_T(Camera.all).Width / 2), Camera_Component_T(Camera.all).Position.X,
         --                       Camera_Component_T(Camera.all).Position.Y, (R => 255, G => 0, B => 0, A => 255), Self.Width, Self.Height);

         --  Draw_Regular_Polygon(Self.Buffer.all, 360, Positive(Camera_Width / 2.0), Camera_X + Half_Camera_Width, Camera_Y + Half_Camera_Height, (R=>255, G=>0, B=> 0, A=>255), Self.Width, Self.Height);

         end;

      end if;


      
      for Entity of Manager.all.Entities loop
      declare
         Transform   : Component_Access   :=    Entity.all.Get_Component(Transform_T'Tag);
         Circle      : Component_Access   :=    Entity.all.Get_Component(Circle_T'Tag);
         Quad        : Component_Access   :=    Entity.all.Get_Component(Quad_T'Tag);
         Text        : Component_Access   :=    Entity.all.Get_Component(Text_T'Tag);
         Texture     : Component_Access   :=    Entity.all.Get_Component(Texture_T'Tag);
         Animation   : Component_Access   :=    Entity.all.Get_Component(Animation_Component_T'Tag);
         Entity_Is_Visible : Boolean := True;

      begin

         --  if Use_Camera then 
         --     Entity_Is_Visible := Is_Visible (Transform_T(Transform.all).Position,
         --                                               Float(Quad_T(Quad.all).Width),
         --                                               Float(Quad_T(Quad.all).Height),
         --                                               Camera_Component_T(Camera.all));

         --  end if;

         -- for debugging and demo, this is off
         --  if Use_Camera and not (Entity_Is_Visible) then
         --     -- Skip rendering if the entity is not visible
         --     --  Put_Line ("Entity:"& Entity.all.Id'Image & " is not visible.");
         --     exit;
         --  end if;

         if Transform /= null then
            -- Draw components if they exist
            if Circle /= null then
               Self.Draw_Circle (Transform, Circle);
            end if;
         if Quad /= null then
            if Animation /= null  then
               Self.Draw_Animated_Texture (Transform, Animation, Quad);
            elsif Texture /= null then
               Self.Draw_Static_Texture(Transform,Texture,Quad);
            else
               Self.Draw_Rectangle(Transform, Quad);
            end if;
         end if;
            if Text /= null then
               Self.Draw_Text (Transform, Text);
            end if;
         else
            Put_Line ("Entity:"& Entity.all.Id'Image & " missing transform component.");
         end if;
      end;
      end loop; 
   end Execute;
end ECS.System.Render;