with ECS.Entity_Manager; use ECS.Entity_Manager;
package Arkanoid_Inputs is

procedure L_Button_Down(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure L_Button_Up(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
procedure Mouse_Move(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);

end Arkanoid_Inputs;