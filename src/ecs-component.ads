with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Deallocation;
with GameMath;
with Graphics.Color; use Graphics.Color;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Renderer; use Graphics.Renderer;

package ECS.Component is 

   type Component_T is tagged null record;
   type Component_Access is access all Component_T'Class;
   type Components_T is array (Natural range <>) of Component_Access;
   type Entity_State is (Idle,Walk,Run); 
   
   procedure Free_Component is new Ada.Unchecked_Deallocation(Component_T'Class, Component_Access);

   type Entity_State_T is new Component_T with record
      State : Entity_State := Idle;
   end record;

   type Transform_T is new Component_T with record
      Position : GameMath.Vec2;
      Velocity : GameMath.Vec2;
      Rotation : Float;
   end record;

   type Rigidbody_T is new Component_T with record
      Mass : Float;
   end record;
    -- Axis aligned bounding box for collision detection between objects
   type AABB_T is new Component_T with record
      Left   : Float;
      Bottom : Float;
      Right  : Float;
      Top    : Float;
   end record;

   type Collision_Params_T is new Component_T with record
      Collision_Enabled    : Boolean;
      Collision_Occurred   : Boolean := False;
      Destroy_On_Collision : Boolean := False;
      Wall_Collision       : Boolean := False;
   end record;

   type Circle_T is new Component_T with record
      Sides    : Positive;
      Radius   : Positive;
      C        : Color;
   end record;
   
   type Quad_T is new Component_T with record
      Width    : Float;
      Height   : Float;
      C        : Color;
   end record;

   type Text_T is new Component_T with record
      Text    : Unbounded_String;
      C       : Color;
   end record;

   type Texture_T is new Component_T with record
      Width    : Integer;
      Height   : Integer;
      Data     : Storage_Array_Access;
   end record;

   type Single_Animation_T is new Component_T with record
      OffsetX     : Integer   := 0;
      OffsetY     : Integer   := 0;   
      Time        : Duration  := 0.0;
      Total_Time  : Duration  := 0.0;
      InitialX    : Integer   := 0;
      InitialY    : Integer   := 0;
      CurX        : Integer   := 0;
      CurY        : Integer   := 0;
      CurFrame    : Integer   := 0;
      TotFrame    : Integer   := 0;
   end record;

   

   type Single_Animation_Access  is access all Single_Animation_T;
   type Texture_Access           is access all Texture_T;
   type Animation_Map            is array (Entity_State) of Single_Animation_Access;
   type Texture_Map              is array (Entity_State) of Texture_Access;

   type Animation_Component_T is new Component_T with record
      Animations     : Animation_Map   := (others => null);
      Textures       : Texture_Map     := (others => null);
      Current        : Entity_State    := Idle; 
   end record;

   type Audio_Component_T is new Component_T with record
      Sound_Triggered   : Boolean            := False;
      File_Path         : Unbounded_String;
   end record;

end ECS.Component;
