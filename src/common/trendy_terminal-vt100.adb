with Ada.Strings.Fixed;

with Trendy_Terminal;

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

    procedure Set_Cursor_Position (C : Cursor_Position) is
        use Ada.Strings;
        use Ada.Strings.Fixed;
    begin
        Put (CSI & Trim (C.Row'Image, Left) & ";" & Trim (C.Col'Image, Left) & "H");
    end Set_Cursor_Position;

    function Get_Cursor_Position return Cursor_Position is
    begin
        loop
            Clear_Input_Buffer;
            VT100.Report_Cursor_Position;
            declare
                Result : constant String := Get_Input;
                Semicolon_Index : constant Natural := Ada.Strings.Fixed.Index(Result, ";", 1);
                Row : Integer := 1;
                Col : Integer := 1;
            begin
                -- The cursor position is reported as
                -- ESC [ ROW ; COL R

                -- May throw on bad parse.
                Row := Integer'Value(Result(3 .. Semicolon_Index - 1));
                Col := Integer'Value(Result(Semicolon_Index + 1 .. Result'Length - 1));

                return Cursor_Position'(Row => Row, Col => Col);
            exception
                -- Bad parse due to existing input on the line.
                when Constraint_Error =>
                    null;
            end;
        end loop;
    end Get_Cursor_Position;

end Trendy_Terminal.VT100;
