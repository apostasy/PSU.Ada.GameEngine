with ECS.Entity_Manager; use ECS.Entity_Manager;
package Input_Callbacks is



procedure Space_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure W_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure A_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure S_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure D_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure L_Button_Down(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure L_Button_Up(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
end Input_Callbacks;