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

    procedure Beginning_Of_Line is
    begin
        VT100.Beginning_Of_Line;
    end Beginning_Of_Line;

    procedure Clear_Line is
    begin
        VT100.Clear_Line;
    end Clear_Line;

    procedure New_Line (Num_Lines : Positive := 1) is
    begin
        -- TODO: very inefficient
        for I in 1 .. Num_Lines loop
            Put (Trendy_Terminal.Platform.End_Of_Line);
        end loop;
    end New_Line;

    procedure Set_Col (Column : Positive) is
        Cursor_Pos : VT100.Cursor_Position := Get_Cursor_Position;
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
    function Get_Line(Format_Fn     : Format_Function := null;
                      Completion_Fn : Completion_Function := null) return String
    is
        package TTI renames Trendy_Terminal.Input;
        use Trendy_Terminal.Maps;
        use all type ASU.Unbounded_String;
        use all type Ada.Containers.Count_Type;

        Input_Line  : ASU.Unbounded_String;
        KM          : constant Trendy_Terminal.Maps.Key_Maps.Map := Maps.Make_Key_Map;
        MK          : constant Trendy_Terminal.Maps.Inverse_Key_Maps.Map := Maps.Make_Key_Lookup_Map;
        L           : Trendy_Terminal.Input.Line_Input;
        Line_Pos    : constant VT100.Cursor_Position := Get_Cursor_Position;
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
            if Format_Fn /= null then
                Print_Line (Line_Pos, Format_Fn (TTI.Current (L)));
            else
                Print_Line (Line_Pos, TTI.Current (L));
            end if;

            Edit_Pos.Col := TTI.Get_Cursor_Index(L) + Line_Pos.Col - 1;
            VT100.Set_Cursor_Position (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Platform.Get_Input);

            if MK(Key_Left) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Left);
                Reset_Completions;
            elsif MK(Key_Right) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Right);
                Reset_Completions;
            elsif MK(Key_Backspace) = Input_Line then
                TTI.Backspace (L);
                Reset_Completions;
            elsif MK(Key_Delete) = Input_Line then
                TTI.Delete (L);
                Reset_Completions;
            elsif MK(Key_Home) = Input_Line then
                TTI.Set_Cursor_Index (L, 1);
                Reset_Completions;
            elsif MK(Key_End) = Input_Line then
                TTI.Set_Cursor_Index (L, TTI.Length (L) + 1);
                Reset_Completions;
            elsif MK(Key_Shift_Tab) = Input_Line then
                if Completion_Fn /= null then
                    if Completions.Is_Empty then
                        Completions := Completion_Fn (L);
                    else
                        Set_Tab_Pos (Tab_Pos - 1);
                    end if;

                    if not Completions.Is_Empty then
                        L := Completions (Tab_Pos);
                    end if;
                end if;
            elsif MK(Key_Tab) = Input_Line then
                if Completion_Fn /= null then
                    if Completions.Is_Empty then
                        Completions := Completion_Fn (L);
                    else
                        Set_Tab_Pos (Tab_Pos + 1);
                    end if;

                    if not Completions.Is_Empty then
                        L := Completions (Tab_Pos);
                    end if;
                end if;
            elsif ASU.Length (Input_Line) = 1 and then Should_Terminate_Input (Input_Line) then
                return TTI.Current (L);
            elsif not KM.Contains (Input_Line) then
                -- Actual text was inserted.
                -- TODO: Maybe add a "replace" mode?
                Reset_Completions;
                TTI.Insert (L, ASU.To_String (Input_Line));
            end if;
        end loop;
    end Get_Line;

    function Get_Cursor_Position return VT100.Cursor_Position is
    begin
        loop
            Platform.Clear_Input_Buffer;
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

end Trendy_Terminal.IO;
