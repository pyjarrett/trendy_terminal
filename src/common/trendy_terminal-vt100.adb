with Ada.Characters.Latin_1;

package body Trendy_Terminal.VT100 is

    procedure Cursor_Left is
    begin
        Write_Terminal (Ada.Characters.Latin_1.ESC & '[' & 'D');
    end Cursor_Left;

    procedure Cursor_Right is
    begin
        Write_Terminal (Ada.Characters.Latin_1.ESC & '[' & 'C');
    end Cursor_Right;

    procedure Erase is
    begin
        Write_Terminal (Ada.Characters.Latin_1.ESC & '[' & 'X');
    end Erase;

    procedure Clear_Line is
    begin
        Write_Terminal (Ada.Characters.Latin_1.ESC & '[' & 'G');
        Write_Terminal (Ada.Characters.Latin_1.ESC & '[' & 'K');
    end Clear_Line;

end Trendy_Terminal.VT100;
