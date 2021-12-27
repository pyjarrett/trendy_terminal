-------------------------------------------------------------------------------
-- Copyright 2021, The Trendy Terminal Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO;

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

    procedure Hide_Cursor is
    begin
        Platform.Put (CSI & "?25l");
    end Hide_Cursor;

    procedure Show_Cursor is
    begin
        Platform.Put (CSI & "?25h");
    end Show_Cursor;

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

    function Get_Cursor_Position return VT100.Cursor_Position is
    begin
        loop
            VT100.Report_Cursor_Position;
            declare
                Result : constant String := Platform.Get_Input;
                Semicolon_Index : constant Natural := Ada.Strings.Fixed.Index(Result, ";", 1);
                Row : Integer := 1;
                Col : Integer := 1;
            begin
                -- The cursor position is reported as
                -- ESC [ ROW ; COL R

                -- May throw on bad parse.
                Row := Integer'Value(Result(3 .. Semicolon_Index - 1));
                Col := Integer'Value(Result(Semicolon_Index + 1 .. Result'Length - 1));

                return VT100.Cursor_Position'(Row => Row, Col => Col);
            exception
                -- Bad parse due to existing input on the line.
                when Constraint_Error =>
                    null;
            end;
        end loop;
    end Get_Cursor_Position;

end Trendy_Terminal.VT100;
