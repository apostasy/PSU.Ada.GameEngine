package body ECS.Config_Loader is

   procedure Load_Config(Manager : in out Entity_Manager_T; File_Name : String) is
      Input  : File_Type;
      Line   : String(1 .. 256);
      Last   : Natural;
      Section: Unbounded_String;
      Entity : Entity_Access;

   begin
      Put_Line("Opening file: " & File_Name);
      Open(File => Input, Mode => In_File, Name => File_Name);
      while not End_Of_File(Input) loop
         Get_Line(File => Input, Item => Line, Last => Last);
         declare
            Trimmed_Line : constant String := Ada.Strings.Fixed.Trim(Line(Line'First .. Last), Ada.Strings.Both);
         begin
            Put_Line("Read line: """ & Trimmed_Line & """");
            if Trimmed_Line'Length = 0 or else Trimmed_Line(Trimmed_Line'First) = '#' then
               -- Skip empty lines and comments
               Put_Line("Skipping empty or comment line");
               null;
            elsif Trimmed_Line(Trimmed_Line'First) = '[' then
               -- New section
               declare
                  Section_Name : constant String := Ada.Strings.Fixed.Trim(Trimmed_Line(Trimmed_Line'First + 1 .. Trimmed_Line'Last - 1), Ada.Strings.Both);
                  Adjusted_Section_Name : String(1 .. 5); -- Ensure it's exactly 5 characters
               begin
                  Put_Line("Found section: " & Trimmed_Line);
                  Put_Line("Trimmed_Line'Length: " & Integer'Image(Trimmed_Line'Length));
                  Put_Line("Section Start Index: 2");
                  Put_Line("Section End Index: " & Natural'Image(Trimmed_Line'Last - 1));
                  Put_Line("Extracted Section Name: " & Section_Name);

                  if Section_Name'Length >= 5 then
                     Adjusted_Section_Name := Section_Name(Section_Name'First .. Section_Name'First + 4);
                  else
                     Adjusted_Section_Name := Section_Name & (1 .. 5 - Section_Name'Length => ' ');
                  end if;

                  Section := To_Unbounded_String(Adjusted_Section_Name);
                  Entity := Manager.AddEntity(To_String(Section));
                  Put_Line("Created new entity with ID: " & Entity.Id);
               end;
            else
               -- Parse key-value pair
               declare
                  Key_Value : constant String := Trimmed_Line;
                  Key_Index : constant Natural := Ada.Strings.Fixed.Index(Key_Value, "=");
                  Key       : constant String := Ada.Strings.Fixed.Trim(Key_Value(Key_Value'First .. Key_Index - 1), Ada.Strings.Both);
                  Value     : constant String := Ada.Strings.Fixed.Trim(Key_Value(Key_Index + 1 .. Key_Value'Last), Ada.Strings.Both);
                  Dot_Index : constant Natural := Ada.Strings.Fixed.Index(Key, ".");
               begin
                  Put_Line("Parsing Key: " & Key);
                  Put_Line("Parsing Value: " & Value);

                  if Key'Length > 0 and Dot_Index /= 0 then
                     declare
                        Component_Name : constant String := Ada.Strings.Fixed.Head(Key, Dot_Index - 1);
                        Property_Name  : constant String := Ada.Strings.Fixed.Tail(Key, Key'Length - Dot_Index);
                     begin
                        Put_Line("Component Name: " & Component_Name);
                        Put_Line("Property Name: " & Property_Name);
                        Put_Line(" ");

                        if Component_Name = "Transform" then
                           declare
                              Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
                              New_Transform_Comp : Component_Access;
                           begin
                              if Transform_Comp = null then
                                 New_Transform_Comp := new Transform_T;
                                 Entity.Add_Component(New_Transform_Comp);
                                 Put_Line(" ");
                                 Put_Line("Created new Transform component");
                              else
                                 New_Transform_Comp := Transform_Comp;
                                 Put_Line(" ");
                                 Put_Line("Using existing Transform component");
                              end if;

                              case Property_Name is
                                 when "Position.X" =>
                                    Transform_T(New_Transform_Comp.all).Position.X := Float'Value(Value);
                                    Put_Line("Set Transform Position.X to " & Float'Image(Transform_T(New_Transform_Comp.all).Position.X));
                                 when "Position.Y" =>
                                    Transform_T(New_Transform_Comp.all).Position.Y := Float'Value(Value);
                                    Put_Line("Set Transform Position.Y to " & Float'Image(Transform_T(New_Transform_Comp.all).Position.Y));
                                 when "Velocity.X" =>
                                    Transform_T(New_Transform_Comp.all).Velocity.X := Float'Value(Value);
                                    Put_Line("Set Transform Velocity.X to " & Float'Image(Transform_T(New_Transform_Comp.all).Velocity.X));
                                 when "Velocity.Y" =>
                                    Transform_T(New_Transform_Comp.all).Velocity.Y := Float'Value(Value);
                                    Put_Line("Set Transform Velocity.Y to " & Float'Image(Transform_T(New_Transform_Comp.all).Velocity.Y));
                                 when "Rotation" =>
                                    Transform_T(New_Transform_Comp.all).Rotation := Float'Value(Value);
                                    Put_Line("Set Transform Rotation to " & Float'Image(Transform_T(New_Transform_Comp.all).Rotation));
                                 when others =>
                                    Put_Line("Unknown property for Transform component: " & Property_Name);
                              end case;
                           end;
                        elsif Component_Name = "RigidBody" then
                           declare
                              RigidBody_Comp : constant Component_Access := Entity.Get_Component(Rigidbody_T'Tag);
                              New_RigidBody_Comp : Component_Access;
                           begin
                              if RigidBody_Comp = null then
                                 New_RigidBody_Comp := new Rigidbody_T;
                                 Entity.Add_Component(New_RigidBody_Comp);
                                 Put_Line(" ");
                                 Put_Line("Created new RigidBody component");
                              else
                                 New_RigidBody_Comp := RigidBody_Comp;
                                 Put_Line(" ");
                                 Put_Line("Using existing RigidBody component");
                              end if;

                              case Property_Name is
                                 when "Mass" =>
                                    Rigidbody_T(New_RigidBody_Comp.all).Mass := Float'Value(Value);
                                    Put_Line("Set RigidBody Mass to " & Float'Image(Rigidbody_T(New_RigidBody_Comp.all).Mass));
                                 when others =>
                                    Put_Line("Unknown property for RigidBody component: " & Property_Name);
                              end case;
                           end;
                        elsif Component_Name = "Quad" then
                           declare
                              Quad_Comp : constant Component_Access := Entity.Get_Component(Quad_T'Tag);
                              New_Quad_Comp : Component_Access;
                           begin
                              if Quad_Comp = null then
                                 New_Quad_Comp := new Quad_T;
                                 Entity.Add_Component(New_Quad_Comp);
                                 Put_Line(" ");
                                 Put_Line("Created new Quad component");
                              else
                                 New_Quad_Comp := Quad_Comp;
                                 Put_Line(" ");
                                 Put_Line("Using existing Quad component");
                              end if;

                              case Property_Name is
                                 when "Width" =>
                                    Quad_T(New_Quad_Comp.all).Width := Float'Value(Value);
                                    Put_Line("Set Quad Width to " & Float'Image(Quad_T(New_Quad_Comp.all).Width));
                                 when "Height" =>
                                    Quad_T(New_Quad_Comp.all).Height := Float'Value(Value);
                                    Put_Line("Set Quad Height to " & Float'Image(Quad_T(New_Quad_Comp.all).Height));
                                 when "C.R" =>
                                    Quad_T(New_Quad_Comp.all).C.R := Color_Int'Value(Value);
                                    Put_Line("Set Quad Color R to " & Color_Int'Image(Quad_T(New_Quad_Comp.all).C.R));
                                 when "C.G" =>
                                    Quad_T(New_Quad_Comp.all).C.G := Color_Int'Value(Value);
                                    Put_Line("Set Quad Color G to " & Color_Int'Image(Quad_T(New_Quad_Comp.all).C.G));
                                 when "C.B" =>
                                    Quad_T(New_Quad_Comp.all).C.B := Color_Int'Value(Value);
                                    Put_Line("Set Quad Color B to " & Color_Int'Image(Quad_T(New_Quad_Comp.all).C.B));
                                 when "C.A" =>
                                    Quad_T(New_Quad_Comp.all).C.A := Color_Int'Value(Value);
                                    Put_Line("Set Quad Color A to " & Color_Int'Image(Quad_T(New_Quad_Comp.all).C.A));
                                 when others =>
                                    Put_Line("Unknown property for Quad component: " & Property_Name);
                              end case;
                           end;
                        elsif Component_Name = "Collision_Params" then
                           declare
                              Collision_Params_Comp : constant Component_Access := Entity.Get_Component(Collision_Params_T'Tag);
                              New_Collision_Params_Comp : Component_Access;
                           begin
                              if Collision_Params_Comp = null then
                                 New_Collision_Params_Comp := new Collision_Params_T;
                                 Entity.Add_Component(New_Collision_Params_Comp);
                                 Put_Line(" ");
                                 Put_Line("Created new Collision_Params component");
                              else
                                 New_Collision_Params_Comp := Collision_Params_Comp;
                                 Put_Line(" ");
                                 Put_Line("Using existing Collision_Params component");
                              end if;

                              case Property_Name is
                                 when "Collision_Enabled" =>
                                    Collision_Params_T(New_Collision_Params_Comp.all).Collision_Enabled := Boolean'Value(Value);
                                    Put_Line("Set Collision_Params Collision_Enabled to " & Boolean'Image(Collision_Params_T(New_Collision_Params_Comp.all).Collision_Enabled));
                                 when "Destroy_On_Collision" =>
                                    Collision_Params_T(New_Collision_Params_Comp.all).Destroy_On_Collision := Boolean'Value(Value);
                                    Put_Line("Set Collision_Params Destroy_On_Collision to " & Boolean'Image(Collision_Params_T(New_Collision_Params_Comp.all).Destroy_On_Collision));
                                 when "Collision_Occurred" =>
                                    Collision_Params_T(New_Collision_Params_Comp.all).Collision_Occurred := Boolean'Value(Value);
                                 when "Wall_Collision" =>
                                    Collision_Params_T(New_Collision_Params_Comp.all).Wall_Collision := Boolean'Value(Value);
                                 when others =>
                                    Put_Line("Unknown property for Collision_Params component: " & Property_Name);
                              end case;
                           end;
                        else
                           Put_Line("Unknown component: " & Component_Name);
                        end if;
                     end;
                  else
                     Put_Line("Invalid key format: " & Key);
                  end if;
               end;
            end if;
         end;

         -- Initialize AABB based on Transform and Quad components
         declare
            Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
            Quad_Comp      : constant Component_Access := Entity.Get_Component(Quad_T'Tag);
         begin
            if Transform_Comp /= null and Quad_Comp /= null then
               declare
                  Transform : Transform_T renames Transform_T(Transform_Comp.all);
                  Quad      : Quad_T renames Quad_T(Quad_Comp.all);
                  AABB_Comp : constant Component_Access := Entity.Get_Component(AABB_T'Tag);
                  New_AABB_Comp : Component_Access;
               begin
                  if AABB_Comp = null then
                     New_AABB_Comp := new AABB_T;
                     Entity.Add_Component(New_AABB_Comp);
                     Put_Line(" ");
                     Put_Line("Created new AABB component");
                  else
                     New_AABB_Comp := AABB_Comp;
                     Put_Line(" ");
                     Put_Line("Using existing AABB component");
                  end if;

                  AABB_T(New_AABB_Comp.all).Left   := Transform.Position.X;
                  AABB_T(New_AABB_Comp.all).Right  := Transform.Position.X + Quad.Width;
                  AABB_T(New_AABB_Comp.all).Bottom := Transform.Position.Y;
                  AABB_T(New_AABB_Comp.all).Top    := Transform.Position.Y + Quad.Height;

                  Put_Line("Initialized AABB for entity " & Entity.Id);
               end;
            end if;
         end;
      end loop;
      Put_Line("Closing file");
      Close(Input);

   end Load_Config;
end ECS.Config_Loader;
