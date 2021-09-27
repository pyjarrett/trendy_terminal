with Ada.Strings.Fixed;

with Trendy_Terminal.Maps;
with Trendy_Terminal.Platform;

package body Trendy_Terminal.VT100 is
    use Trendy_Terminal.Maps;

    procedure Cursor_Left is
    begin
        Platform.Put (CSI & "1D");
    end Cursor_Left;

    procedure Cursor_Right is
    begin
        Platform.Put (CSI & "1C");
    end Cursor_Right;

    procedure Cursor_Down is
    begin
        Platform.Put (CSI & "B");
    end Cursor_Down;

    procedure Cursor_Up is
    begin
        Platform.Put (CSI & "A");
    end Cursor_Up;

    procedure Scroll_Down is
    begin
        Platform.Put (CSI & "T");
    end Scroll_Down;

    procedure Scroll_Up is
    begin
        Platform.Put (CSI & "S");
    end Scroll_Up;

    procedure Cursor_Next_Line is
    begin
        Platform.Put (CSI & "E");
    end Cursor_Next_Line;

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
