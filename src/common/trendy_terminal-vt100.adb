with Ada.Strings.Fixed;

with Trendy_Terminal.Platform;

package body Trendy_Terminal.VT100 is
    procedure Cursor_Left is
    begin
        Platform.Put (CSI & 'D');
    end Cursor_Left;

    procedure Cursor_Right is
    begin
        Platform.Put (CSI & 'C');
    end Cursor_Right;

    procedure Erase is
    begin
        Platform.Put (CSI & 'X');
    end Erase;

    procedure Beginning_Of_Line is
    begin
        Platform.Put (CSI & "1G");
    end Beginning_Of_Line;

    procedure Clear_Line is
    begin
        Platform.Put (CSI & 'K');
    end Clear_Line;

    procedure Report_Cursor_Position is
    begin
        Platform.Put (CSI & "6n");
    end Report_Cursor_Position;

    procedure Set_Cursor_Position (C : Cursor_Position) is
        use Ada.Strings;
        use Ada.Strings.Fixed;
    begin
        Platform.Put (CSI & Trim (C.Row'Image, Left) & ";" & Trim (C.Col'Image, Left) & "H");
    end Set_Cursor_Position;

end Trendy_Terminal.VT100;
