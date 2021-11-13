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

    function Should_Terminate_Input (Input_Line : ASU.Unbounded_String) return Boolean is
        Key_CR : constant := 10;
        Key_FF : constant := 13;
        Input  : constant Integer := Character'Pos(ASU.Element(Input_Line, 1));
    begin
        return Input = Key_CR or else Input = Key_FF;
    end Should_Terminate_Input;

    -- Processes the next line of input in according to completion, formatting,
    -- and hinting callbacks.
    --
    -- TODO: Support full utf-8.  Only ASCII is supported for now.
    --
    -- Helper to implicitly use a Stateless_Line_Editor
    function Get_Line(Format_Fn     : Format_Function := null;
                      Completion_Fn : Completion_Function := null) return String
    is
        Editor : Stateless_Line_Editor := (Format_Fn     => Format_Fn,
                                           Completion_Fn => Completion_Fn);
    begin
        return Get_Line (Editor);
    end Get_Line;

    function Get_Line (Editor : in out Line_Editor'Class) return String is
        use Trendy_Terminal.Maps;
        use all type ASU.Unbounded_String;
        use all type Ada.Containers.Count_Type;

        Input_Line  : ASU.Unbounded_String;
        L           : Lines.Line;
        Line_Pos    : constant VT100.Cursor_Position := VT100.Get_Cursor_Position;
        Edit_Pos    : VT100.Cursor_Position := Line_Pos;
        Tab_Pos     : Integer := 1;
        Completions : Line_Vectors.Vector;

        -- Prints an updated input line at the given starting position.
        procedure Print_Line (Pos : VT100.Cursor_Position; S : String) is
        begin
            VT100.Set_Cursor_Position (Pos);
            VT100.Clear_Line;
            Put (S);
        end Print_Line;

        procedure Reset_Completions is
        begin
            Tab_Pos := 1;
            Completions.Clear;
        end Reset_Completions;

        procedure Set_Tab_Pos (N : Integer) is
        begin
            Tab_Pos := N;
            if Tab_Pos <= 0 then
                Tab_Pos := Integer (Completions.Length);
            else
                Tab_Pos := Tab_Pos mod Integer (Completions.Length);
                if Tab_Pos = 0 then
                    Tab_Pos := Integer (Completions.Length);
                end if;
            end if;
        end Set_Tab_Pos;
    begin
        Edit_Pos.Row := Line_Pos.Row;

        loop
            Print_Line (Line_Pos, Lines.Current (Editor.Format (L)));

            Edit_Pos.Col := Lines.Get_Cursor_Index(L) + Line_Pos.Col - 1;
            VT100.Set_Cursor_Position (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Platform.Get_Input);

            if Maps.Sequence_For(Key_Left) = Input_Line then
                Lines.Move_Cursor(L, Lines.Left);
                Reset_Completions;
            elsif Maps.Sequence_For (Key_Right) = Input_Line then
                Lines.Move_Cursor(L, Lines.Right);
                Reset_Completions;
            elsif Maps.Sequence_For (Key_Backspace) = Input_Line then
                Lines.Backspace (L);
                Reset_Completions;
            elsif Maps.Sequence_For (Key_Delete) = Input_Line then
                Lines.Delete (L);
                Reset_Completions;
            elsif Maps.Sequence_For (Key_Home) = Input_Line then
                Lines.Set_Cursor_Index (L, 1);
                Reset_Completions;
            elsif Maps.Sequence_For (Key_End) = Input_Line then
                Lines.Set_Cursor_Index (L, Lines.Length (L) + 1);
                Reset_Completions;
            elsif Maps.Sequence_For (Key_Shift_Tab) = Input_Line then
                if Completions.Is_Empty then
                    Completions := Editor.Complete (L);
                else
                    Set_Tab_Pos (Tab_Pos - 1);
                end if;

                if not Completions.Is_Empty then
                    L := Completions (Tab_Pos);
                end if;
            elsif Maps.Sequence_For (Key_Tab) = Input_Line then
                    if Completions.Is_Empty then
                    Completions := Editor.Complete (L);
                else
                    Set_Tab_Pos (Tab_Pos + 1);
                end if;

                if not Completions.Is_Empty then
                    L := Completions (Tab_Pos);
                end if;
            elsif ASU.Length (Input_Line) = 1 and then Should_Terminate_Input (Input_Line) then
                return Lines.Current (L);
            elsif not Maps.Is_Key (ASU.To_String (Input_Line)) then
                -- Actual text was inserted.
                -- TODO: Maybe add a "replace" mode?
                Reset_Completions;
                Lines.Insert (L, ASU.To_String (Input_Line));
            end if;
        end loop;
    end Get_Line;

    function Format (E : in out Stateless_Line_Editor; L : Lines.Line) return Lines.Line is
        (if E.Format_Fn /= null then E.Format_Fn (L) else L);

    function Complete (E : in out Stateless_Line_Editor; L : Lines.Line) return Line_Vectors.Vector is
        (if E.Completion_Fn /= null then E.Completion_Fn (L) else Line_Vectors.Empty);

end Trendy_Terminal.IO;
