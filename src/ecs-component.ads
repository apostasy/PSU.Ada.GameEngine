with Ada.Tags; use Ada.Tags;
with Renderer;

package ecs.component is 

    type Component_T is tagged null record;
    type Component_Access is access all Component_T'Class;
    type Components_T is array (Natural range <>) of Component_Access;

    type Transform_T is new Component_T with record
        X, Y : Float;
        Rotation : Float;
    end record;

    type Shape_T is new Component_T with record
        Sides : Positive range 3..360;
        Radius : Positive;
        Color : Renderer.Color;
    end record;

    type Rigidbody_T is new Component_T with record
        Mass : Float;
    end record;
    -- Axis aligned bounding box for collision detection between objects
    type AABB_T is new Component_T with record
        MinX : Float;
        MinY : Float;
        MaxX : Float;
        MaxY : Float;
    end record;

    
end ecs.component;