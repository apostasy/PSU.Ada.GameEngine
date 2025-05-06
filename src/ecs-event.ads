with Interfaces;
with Ada.Streams;
with Ada.Containers.Vectors;

-- EVENT PACKAGE ADDED 11/25/24
package ECS.Event is

   -- Use Interfaces for low-level types
   type Byte is mod 256; -- Equivalent to an unsigned 8-bit type
   type Byte_Array is array (Positive range <>) of Byte;

   type Event_Data is record
      KeyCode      : Byte := 0;
      MouseX       : Integer := 0;
      MouseY       : Integer := 0;
      W_Width      : Integer := 0;
      W_Height     : Integer := 0;
      Additional   : Byte_Array(1 .. 5) := (others => 0);
   end record;

   -- Event types
   type EventType_T is (
                        NoEvent    , KeyDown  , KeyUp, 
                        L_MouseDown, L_MouseUp, MouseMove,
                        R_MouseDown, R_MouseUp, W_Size
   );

   -- Define the event structure
   type Event_T is record
      Source    : Natural;            -- Event source identifier
      EventType : EventType_T;        -- Type of event
      Data      : Event_Data;
   end record;

    -- Base class for platform-specific event handlers
   type Event_Handler is abstract tagged null record;

   -- Abstract procedures for platform-specific behavior
   procedure Emit_Event (Handler : in out Event_Handler; Event : Event_T) is abstract;
   procedure Process_Events (Handler : in out Event_Handler) is abstract;
   function Get_Next_Event (Handler : in out Event_Handler) return Event_T is abstract;

end ECS.Event;