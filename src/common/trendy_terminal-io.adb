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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Trendy_Terminal.Maps;

package body Trendy_Terminal.IO is

    procedure Put (S : ASU.Unbounded_String) is
    begin
        Put (ASU.To_String(S));
    end Put;

    procedure Put_Line(S : String) is
    begin
        Put (S);
        New_Line;
    end Put_Line;

    procedure Put_Line (S : ASU.Unbounded_String) is
    begin
        Put_Line (ASU.To_String(S));
    end Put_Line;

    procedure Rewrite_Line (Pos : VT100.Cursor_Position; S : String) is
    begin
        VT100.Hide_Cursor;
        VT100.Set_Cursor_Position (Pos);
        VT100.Clear_Line;
        VT100.Show_Cursor;
        Put (S);
    end Rewrite_Line;

    procedure New_Line (Num_Lines : Positive := 1) is
    begin
        -- TODO: very inefficient
        for I in 1 .. Num_Lines loop
            Put (Trendy_Terminal.Platform.End_Of_Line);
        end loop;
    end New_Line;

    procedure Set_Col (Column : Positive) is
        Cursor_Pos : VT100.Cursor_Position := VT100.Get_Cursor_Position;
    begin
        Cursor_Pos.Col := Column;
        VT100.Set_Cursor_Position (Cursor_Pos);
    end Set_Col;

end Trendy_Terminal.IO;
