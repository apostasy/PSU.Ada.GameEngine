with ECS.Component; use ECS.Component;
with GameMath;
package Custom_Components is
   type Ball_State_T is new Component_T with record
      Ball_Launched : Boolean;
      Previous_Pos : GameMath.Vec2;
   end record;

   type Brick_Attributes is new Component_T with record
      Hits : Integer;
      Score : Integer;
      Indestructable : Boolean;
   end record;

end Custom_Components;