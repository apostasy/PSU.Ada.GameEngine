with ecs.component; use ecs.component;
with ecs.entity; use ecs.entity;
with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with Renderer;
with Window;

package body ecs.system is

    procedure Execute (Self : Mover_T;
                       Dt   : Duration; 
                       E    : access Entity_T'Class;  
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        Trans  : Component_Access :=       E.all.Get_Component (Transform_T'Tag);
        Rigidbodies : Component_Access :=  E.all.Get_Component (Rigidbody_T'Tag);

        DeltaY : Float := Float(Dt) * (-9.8);
    begin

        if Trans = null then
            Put_Line ("No Transform");
            return;
        end if;
        if Rigidbodies = null then
            Put_Line ("No Rigidbodies");
            return;
        end if;

        declare
            T renames Transform_T (Trans.all);
            R renames Rigidbody_T (Rigidbodies.all);           

        begin
        Put_Line ("Mover: " & T.X'Image & ", " & T.Y'Image & ", " & T.Rotation'Image);
        T.Y := T.Y + DeltaY;
        Put_Line ("Mover: " & T.X'Image & ", " & T.Y'Image & ", " & T.Rotation'Image);
        end;
    end Execute;

    procedure Execute (Self : Renderer_T;
                       Dt   : Duration;
                       E    : access Entity_T'Class;
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        Trans  : Component_Access :=       E.all.Get_Component (Transform_T'Tag);
        Shapes : Component_Access :=  E.all.Get_Component (Shape_T'Tag);
    begin

        if Trans = null then
            Put_Line ("No Transform");
            return;
        end if;
        if Shapes = null then
            Put_Line ("No Shapes");
            return;
        end if;

        declare
            T renames Transform_T (Trans.all);
            S renames Shape_T (Shapes.all); 
            Img : Renderer.Image := Self.Image;          

        begin
            Renderer.Draw_Regular_Polygon (Img, S.Sides, S.Radius, T.X, T.Y, S.Color);
            Renderer.Draw_Image_To_Window (Img);
        end;
    end Execute;

    
    procedure Execute (Self : Collision_T;
                       Dt   : Duration;
                       E    : access Entity_T'Class;
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        AABB : Component_Access :=   E.all.Get_Component (AABB_T'Tag);
        -- Checks all entities for collisions and makes appropriate update to transform
        procedure ProcessCollisions is
        begin
            null;
        end ProcessCollisions;
        -- Helper to check if two entities are colliding
        function IsColliding return Boolean is
        begin
            return False;
        end IsColliding;
    begin
        null;
    end Execute;

end ecs.system;