with Ada.Strings.Fixed;

package body Trendy_Terminal.VT100 is
    procedure Cursor_Left is
    begin
        Put (CSI & 'D');
    end Cursor_Left;

    procedure Cursor_Right is
    begin
        Put (CSI & 'C');
    end Cursor_Right;

    procedure Erase is
    begin
        Put (CSI & 'X');
    end Erase;

    procedure Clear_Line is
    begin
        Put (CSI & 'G');
        Put (CSI & 'K');
    end Clear_Line;

    procedure Report_Cursor_Position is
    begin
        Put (CSI & "6n");
    end Report_Cursor_Position;

    procedure Position_Cursor (C : Cursor_Position) is
        use Ada.Strings;
        use Ada.Strings.Fixed;
    begin
        Put (CSI & Trim (C.Row'Image, Left) & ";" & Trim (C.Col'Image, Left) & "H");
    end Position_Cursor;

end Trendy_Terminal.VT100;
