with Ada.Text_IO; use Ada.Text_IO;
with ecs;
with ECS.Entity; use ECS.Entity;
with ECS.Component; use ECS.Component;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Renderer; use Graphics.Renderer;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with QOI; use QOI;
with GNAT.OS_Lib;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Input_Callbacks is 

   type Input_Data2 is record
    Data : Storage_Array_Access;
    Desc : QOI.QOI_Desc;
  end record;


  icon : constant String := "C:\ProgramData\Ada\PSU.Ada.GameEngine.Fork\Data\skull.qoi";
  Texture_Image : Input_Data2;

  function Load_QOI (Filename : String) return Input_Data2 is
    use GNAT.OS_Lib;

    FD  : File_Descriptor;
    Ret : Integer;
    Result : Input_Data2;

  begin

    FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

    if FD = Invalid_FD then
      Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
      GNAT.OS_Lib.OS_Exit (1);
    end if;

    declare
      Len     : constant Storage_Count := Storage_Count (File_Length (FD));
      In_Data : constant Storage_Array_Access := new Storage_Array (1 .. Len);
    begin
      Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

      if Ret /= In_Data'Length then
        Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
        GNAT.OS_Lib.OS_Exit (1);
      end if;

      Close (FD);

      QOI.Get_Desc (In_Data.all, Result.Desc);

      declare
        Out_Len     : constant Storage_Count        :=
         Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
        Out_Data    : constant Storage_Array_Access :=
         new Storage_Array (1 .. Out_Len);
        Output_Size : Storage_Count;
      begin
        QOI.Decode
         (Data => In_Data.all, Desc => Result.Desc, Output => Out_Data.all,
          Output_Size => Output_Size);

        Result.Data := Out_Data;

        return Result;

      end;

    end;

  end Load_QOI;


    


   
    
    
   
   Last_Fired : Duration := 0.0;

   procedure Spawn_Bullet(Manager : access Entity_Manager_T'Class) is
      Trans    : Component_Access;
      Rect     : Component_Access;
      Player_Entity : Entity_Access;
   begin
      Player_Entity := Manager.GetEntity("Playr");
      if Player_Entity = null then
         Put_Line ("Player entity not found");
         return;
      end if;
      Trans := Player_Entity.all.Get_Component(Transform_T'Tag);
      Rect :=  Player_Entity.all.Get_Component(Quad_T'Tag);
      declare
         T renames Transform_T(Trans.all);
         R renames Quad_T(Rect.all);
         StartPosX   : Float;
         B_Trans     : Component_Access;
         B_RBody     : Component_Access;
         B_AABB      : Component_Access;
         B_ColPar    : Component_Access;
         B_Shape     : Component_Access;
         Texture_B   : Component_Access;
        
      begin
         Texture_Image := Load_QOI(icon);
         Texture_B := new Texture_T'
            (Width => Integer(Texture_Image.Desc.Width), Height => Integer(Texture_Image.Desc.Height), Data => Texture_Image.Data);
         Bullet : Entity_Access := Manager.all.AddEntity ("B0001");
         StartPosX := (T.Position.X + (R.Width / 2.0)) ;
         B_Trans := new Transform_T'((StartPosX + 22.0,T.Position.Y),(400.0,0.0),0.0);
         B_RBody := new Rigidbody_T'(Mass => 0.0);
         B_Shape := new Quad_T'(22.0,22.0,(255,255,255,255));
         B_ColPar := new Collision_Params_T'(True,False,True,True);
         declare
            BT renames Transform_T(B_Trans.all);
            BS renames Quad_T(B_Shape.all);
         begin
            B_AABB  := new AABB_T'(BT.Position.X, BT.Position.Y + BS.Height, BT.Position.X + BS.Width, BT.Position.Y);
         end;
         Bullet.all.Add_Component (B_Trans);
         Bullet.all.Add_Component (B_RBody);
         Bullet.all.Add_Component (B_AABB);
         Bullet.all.Add_Component (B_ColPar);
         Bullet.all.Add_Component (B_Shape);
         Bullet.all.Add_Component (Texture_B);
      end;
   end Spawn_Bullet;



   procedure Space_Key (Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Cooldown   : constant Duration := 0.02;
   begin
      if KeyDown then
         Last_Fired := Last_Fired + Dt;
         if Last_Fired >= Cooldown then
            Spawn_Bullet(Manager);
            Last_Fired := Last_Fired - Cooldown;
         end if;
      else
         Last_Fired := 0.0;
      end if;
   end Space_Key;

   procedure W_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Trans : Component_Access;
      Animation : Component_Access;
      Player_Entity : Entity_Access;
   begin
      Player_Entity := Manager.GetEntity("Playr");
      if Player_Entity = null then
         Put_Line ("Player entity not found");
         return;
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      Animation := Player_Entity.all.Get_Component (Animation_Component_T'Tag);
      declare
         T renames Transform_T(Trans.all);
         A renames Animation_Component_T(Animation.all);
      begin
         if KeyDown then
            T.Velocity.Y := -100.0;
            A.Current := Walk;
         else
            T.Velocity.Y := 0.0;
            A.Current := Idle;
         end if;
      end; 
   end W_Key;

   procedure A_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Trans : Component_Access;
      Animation : Component_Access;
      Player_Entity : Entity_Access;
   begin
      Player_Entity := Manager.GetEntity("Playr");
      if Player_Entity = null then
         Put_Line ("Player entity not found");
         return;
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      Animation := Player_Entity.all.Get_Component (Animation_Component_T'Tag);
      declare
         T renames Transform_T(Trans.all);
         A renames Animation_Component_T(Animation.all);
      begin
         if KeyDown then
            T.Velocity.X := -100.0;
            A.Current := Walk;
         else
            T.Velocity.X := 0.0;
            A.Current := Idle;
         end if;
      end; 
   end A_Key;

   procedure S_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Trans : Component_Access;
      Animation : Component_Access;
      Player_Entity : Entity_Access;
   begin
      Player_Entity := Manager.GetEntity("Playr");
      if Player_Entity = null then
         Put_Line ("Player entity not found");
         return;
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      Animation := Player_Entity.all.Get_Component (Animation_Component_T'Tag);
      declare
         T renames Transform_T(Trans.all);
         A renames Animation_Component_T(Animation.all);
      begin
         if KeyDown then
            T.Velocity.Y := 100.0;
            A.Current := Walk;
         else
            T.Velocity.Y := 0.0;
            A.Current := Idle;
         end if;
      end; 
   end S_Key;   

   procedure D_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Trans : Component_Access;
      Animation : Component_Access;
      Player_Entity : Entity_Access;
   begin
      Player_Entity := Manager.GetEntity("Playr");
      if Player_Entity = null then
         Put_Line ("Player entity not found");
         return;
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      Animation := Player_Entity.all.Get_Component (Animation_Component_T'Tag);
      declare
         T renames Transform_T(Trans.all);
         A renames Animation_Component_T(Animation.all);
      begin
         if KeyDown then
            T.Velocity.X := 100.0;
            A.Current := Walk;
         else
            T.Velocity.X := 0.0;
            A.Current := Idle;
         end if;
      end; 
   end D_Key;

   procedure L_Button_Down(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
   begin
      Put_Line ("Left mouse pressed at "& ECS.MousePos.CurrentPos'Image);
   end L_Button_Down;

   procedure L_Button_Up(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
   begin
      Put_Line ("Left mouse released at "& ECS.MousePos.CurrentPos'Image);
   end L_Button_Up;

   procedure Mouse_Move(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      Trans : Component_Access;
      Player_Entity : Entity_Access;
   begin
      Player_Entity := Manager.GetEntity("Playr");
      if Player_Entity = null then
         Put_Line ("Player entity not found");
         return;
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      declare
         T renames Transform_T(Trans.all);
      begin
         T.Position.X := ECS.MousePos.CurrentPos.X;
      end; 
   end Mouse_Move;

end Input_Callbacks;