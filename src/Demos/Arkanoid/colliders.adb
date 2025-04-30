-- Invisible entities used for collision
with ECS.Component; use ECS.Component;
package body Colliders is 
   procedure Add_Wall(
      Manager        : access Entity_Manager_T'Class; 
      Width,Height   : Float;
      Position       : Vec2;
      Id             : Id_T
   ) is
      Wall           : Entity_Access            := Manager.all.AddEntity (Id);
      Wall_Trans     : Component_Access;
      Wall_RBody     : Component_Access;
      Wall_Col       : Component_Access;
      Wall_AABB      : Component_Access;
      Wall_Quad      : Component_Access;
   begin
      Wall_Trans     := new Transform_T'(Position,(0.0,0.00),0.0);
      Wall_RBody     := new Rigidbody_T'(Mass => 1.0);
      Wall_Col       := new Collision_Params_T'(True,False,False,False);
      Wall_Quad      := new Quad_T'(Width,Height,(0,0,0,0));
      Wall_AABB      := new AABB_T'(
         Left     => Position.X, 
         Bottom   => Position.Y + Height, 
         Right    => Position.X + Width, 
         Top      => Position.Y
      );

      Wall.all.Add_Component (Wall_Trans);
      Wall.all.Add_Component (Wall_RBody);
      Wall.all.Add_Component (Wall_Col);
      Wall.all.Add_Component (Wall_AABB);
      Wall.all.Add_Component (Wall_Quad);

   end Add_Wall;
end Colliders;