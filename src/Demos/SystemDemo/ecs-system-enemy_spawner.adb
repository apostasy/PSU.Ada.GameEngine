with Graphics.Texture_Loader; use Graphics.Texture_Loader;
with Ada.Streams;           
with Ada.Streams.Stream_IO;
with GameMath; use GameMath;

package body ECS.System.Enemy_Spawner is 

   Last_Spawned   : Duration;
   Cooldown   : constant Duration := 2.0;
   Enemy_Texture : constant String := "Data\Zombie.qoi";
   Texture_Image : QOI_Image_Data := Load_QOI(Enemy_Texture);
   type Vec2_Array is array (0 .. 4) of Vec2;
   Pos_Index : Natural := 0;

   Enemy_Positions : Vec2_Array := ((550.0,150.0),(550.0,75.0),(550.0,275.0),(550.0,125.0),(550.0,275.0));

   procedure SpawnEnemy (Manager : access Entity_Manager_T'Class) is
      Texture_E   : Component_Access;
      E_Trans     : Component_Access;
      E_RBody     : Component_Access;
      E_ColPar    : Component_Access;
      E_AABB      : Component_Access;
      E_Quad      : Component_Access;
      
   begin
      
      
      Texture_E := new Texture_T'
         (Width => Integer(Texture_Image.Desc.Width), Height => Integer(Texture_Image.Desc.Height), Data => Texture_Image.Data);
      Enemy : Entity_Access := Manager.all.AddEntity ("Enemy");
      E_Trans := new Transform_T'(Enemy_Positions(Pos_Index),(-100.0,0.0),0.0);
      E_RBody := new Rigidbody_T'(Mass => 0.0);
      E_ColPar := new Collision_Params_T'(True,False,True,True);
      E_Quad := new Quad_T'(64.0,78.0,(0,0,0,0));
      declare
         BT renames Transform_T(E_Trans.all);
      begin
         E_AABB  := new AABB_T'(BT.Position.X, BT.Position.Y + Float(Texture_Image.Desc.Height), BT.Position.X + Float(Texture_Image.Desc.Width), BT.Position.Y);
      end;
      Enemy.all.Add_Component (E_Trans);
      Enemy.all.Add_Component (E_RBody);
      Enemy.all.Add_Component (E_AABB);
      Enemy.all.Add_Component (E_ColPar);
      Enemy.all.Add_Component (E_Quad);
      Enemy.all.Add_Component (Texture_E);
      Pos_Index := (Pos_Index + 1) mod 5;
   end SpawnEnemy;



   procedure Execute ( Self      : in out Enemy_Spawn_T;
                       Dt        : Duration;
                       Manager   : access Entity_Manager_T'Class ) is
   
   begin

   Last_Spawned := Last_Spawned + Dt;

   if Last_Spawned >= Cooldown then
      SpawnEnemy(Manager);
      Last_Spawned := 0.0;
   end if;
   end Execute;



end ECS.System.Enemy_Spawner;