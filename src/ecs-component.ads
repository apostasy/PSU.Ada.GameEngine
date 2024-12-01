with Ada.Tags; use Ada.Tags;
with ECS.Vec2;

package ECS.Component is 

    type Component_T is tagged null record;
    type Component_Access is access all Component_T'Class;
    type Components_T is array (Natural range <>) of Component_Access;

    type Transform_T is new Component_T with record
        Position : ECS.Vec2.Vec2;
        Velocity : ECS.Vec2.Vec2;
        Rotation : Float;
    end record;

    type Rigidbody_T is new Component_T with record
        Mass : Float;
    end record;
    -- Axis aligned bounding box for collision detection between objects
    type AABB_T is new Component_T with record
        Left : Float;
        Bottom : Float;
        Right : Float;
        Top : Float;
    end record;

    type Collision_Params_T is new Component_T with record
        Collision_Enabled : Boolean;
        Collision_Occurred : Boolean := False;
        Destroy_On_Collision : Boolean := False;
        Left_Bound : Boolean;
        Right_Bound : Boolean;
        Top_Bound : Boolean;
        Bottom_Bound : Boolean;
    end record;

    
end ECS.Component;