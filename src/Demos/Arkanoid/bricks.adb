with Custom_Components; use Custom_Components;
with ECS.Entity; use ECS.Entity;
package body Bricks is 


procedure Add_Brick(
   Manager : access ECS.Entity_Manager.Entity_Manager_T'Class; 
   Position : Vec2; 
   Brick_Type : Integer;  
   Texture : Texture_Access
   ) is
      Brick_Trans       : Component_Access;
      Brick_RBody       : Component_Access;
      Brick_Col         : Component_Access;
      Brick_AABB        : Component_Access;
      Brick_Quad        : Component_Access;
      Brick_Animations  : Component_Access;
      Brick_Attribs     : Component_Access;
      Brick             : Entity_Access := Manager.AddEntity("Brik1");
      Brick_NoHit       : Single_Animation_Access;
      Brick_Hit         : Single_Animation_Access;
      Anim_Speed        : Duration := 0.05;
   begin
      Brick_Trans := new Transform_T'(Position,(0.0,0.0),0.0);
      Brick_RBody := new Rigidbody_T'(Mass => 1.0);
      if Brick_Type >= 10 then
         Brick_Col := new Collision_Params_T'(True,False,False,False);
      else
         Brick_Col := new Collision_Params_T'(True,False,False,False);
      end if;
      Brick_Quad := new Quad_T'(15.0, 7.0,(0,0,0,0));
      declare
         B_T renames Transform_T(Brick_Trans.all);
         B_Q renames Quad_T(Brick_Quad.all);
      begin
         Brick_AABB := new AABB_T'(
            Left => B_T.Position.X, 
            Bottom => B_T.Position.Y + B_Q.Height, 
            Right => B_T.Position.X + B_Q.Width, 
            Top => B_T.Position.Y
         );
      end;
      case Brick_Type is
         when 20  =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,0,24,0,24,0,1);
            Brick_Hit   := new Single_Animation_T'(16,0,Anim_Speed,0.0,16,24,16,24,0,5);
         when 10  =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,0,16,0,16,0,1);
            Brick_Hit   := new Single_Animation_T'(16,0,Anim_Speed,0.0,16,16,16,16,0,5);
         when 7   =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,48,8,48,8,0,1);
            Brick_Hit   := new Single_Animation_T'(0,0,0.0,0.0,48,8,48,8,0,1);
         when 6   =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,32,8,32,8,0,1);
            Brick_Hit   := new Single_Animation_T'(0,0,0.0,0.0,32,8,32,8,0,1);
         when 5   =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,16,8,16,8,0,1);
            Brick_Hit   := new Single_Animation_T'(0,0,0.0,0.0,16,8,16,8,0,1);
         when 4   =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,0,8,0,8,0,1);
            Brick_Hit   := new Single_Animation_T'(0,0,0.0,0.0,0,8,0,8,0,1);
         when 3   =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,48,0,48,0,0,1);
            Brick_Hit   := new Single_Animation_T'(0,0,0.0,0.0,48,0,48,0,0,1);
         when 2   =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,32,0,32,0,0,1);
            Brick_Hit   := new Single_Animation_T'(0,0,0.0,0.0,32,0,32,0,0,1);
         when 1   =>
            Brick_NoHit := new Single_Animation_T'(0,0,0.0,0.0,16,0,16,0,0,1);
            Brick_Hit   := new Single_Animation_T'(0,0,0.0,0.0,16,0,16,0,0,1);
         when others =>
            null;
      end case;
      Anim_Comp : constant Animation_Component_T := (
         Animations  => (others => null), 
         Textures    => (others => null),
         Current     => Idle
      );
      Brick_Animations := new Animation_Component_T'(Anim_Comp);

      if Brick_Type = 20 then
         Brick_Attribs := new Brick_Attributes'(0,0,True);
      elsif Brick_Type = 10 then
         Brick_Attribs := new Brick_Attributes'(2, 200, False);
      else
         Brick_Attribs := new Brick_Attributes'(1, 100, False);
      end if;
                  


      Brick.all.Add_Component (Brick_Trans);
      Brick.all.Add_Component (Brick_RBody);
      Brick.all.Add_Component (Brick_Col);
      Brick.all.Add_Component (Brick_AABB);
      Brick.all.Add_Component (Brick_Quad);
      Brick.all.Add_Component (Brick_Animations);
      Brick.all.Add_Component (Brick_Attribs);

      Anims_Brick : Animation_Component_T renames Animation_Component_T(Brick_Animations.all);
      Anims_Brick.Animations(Idle)  := Brick_NoHit;
      Anims_Brick.Animations(Walk)  := Brick_Hit;
      Anims_Brick.Textures(Idle)    := Texture;
      Anims_Brick.Textures(Walk)    := Texture;
   end Add_Brick;
end Bricks;